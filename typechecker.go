package main

import (
	"fmt"
)

type TypeChecker struct {
	code     string
	filename string
}

func NewTypeChecker(code, filename string) *TypeChecker {
	return &TypeChecker{code: code, filename: filename}
}

// ****

// index to refere to a (currently only builtin) type
// somehow unify this mess
type ScopeImpl struct {
	Parent Scope
	// A return stmt needs to know which procedure it belongs to. This
	// pointer points to the corresponding procedure. This should
	// probably be redued to be just the proc signature.
	CurrentProc *TcProcDef
	Variables   map[string]TcSymbol
	Procedures  map[string]*TcProcDef
	Types       map[string]Type
}

type Scope = *ScopeImpl

func (scope Scope) NewSubScope() Scope {
	return &ScopeImpl{
		Parent:     scope,
		Variables:  make(map[string]TcSymbol),
		Procedures: make(map[string]*TcProcDef),
		Types:      make(map[string]Type),
	}
}

func (scope Scope) NewSymbol(name Ident, kind SymbolKind, typ Type) TcSymbol {
	result := TcSymbol{Name: name.source, Kind: kind, Typ: typ}
	scope.Variables[name.source] = result
	return result
}

func (tc *TypeChecker) LookUpType(scope Scope, expr TypeExpr) Type {
	// TODO this is a temporary hack to get arrays somehow working

	if expr.Ident.source == "array" {
		var at ArrayType
		at.Len = expr.ExprArgs[0].(IntLit).Value
		at.Elem = tc.LookUpType(scope, expr.TypeArgs[0])
		return &at
	}

	// TODO really slow lookup, should really be faster
	name := expr.Ident.source
	for key, value := range scope.Types {
		if key == name {
			return value
		}
	}
	if scope.Parent != nil {
		return tc.LookUpType(scope.Parent, expr)
	}
	panic(tc.Errorf(expr, "Type not found: %s", name)) // some comment
}

func (tc *TypeChecker) LookUpProc(scope Scope, ident Ident) TcProcSymbol {
	if scope == nil {
		panic(tc.Errorf(ident, "proc not found: %s", ident.source))
	}

	if impl, ok := scope.Procedures[ident.source]; ok {
		return TcProcSymbol{Name: ident.source, Impl: impl}
	}
	return tc.LookUpProc(scope.Parent, ident)
}

func (tc *TypeChecker) LookUpLetSym(scope Scope, ident Ident) TcSymbol {
	if scope == nil {
		panic(fmt.Sprintf("let sym not found: %s", ident.source))
	}

	if sym, ok := scope.Variables[ident.source]; ok {
		sym.source = ident.source
		return sym
	}
	return tc.LookUpLetSym(scope.Parent, ident)
}

func (tc *TypeChecker) LineColumnNode(node AstNode) (line, columnStart, columnEnd int) {
	return LineColumnStr(tc.code, node.Source())
}

func (tc *TypeChecker) TypeCheckStructDef(scope Scope, def StructDef) (result *TcStructDef) {
	result = &TcStructDef{}
	result.Name = def.Name.source
	for _, field := range def.Fields {
		var tcField TcStructField
		tcField.Name = field.Name.source
		tcField.Type = tc.LookUpType(scope, field.TypeExpr)
		result.Fields = append(result.Fields, tcField)
	}
	return result
}

func (tc *TypeChecker) TypeCheckProcDef(parentScope Scope, def ProcDef) (result *TcProcDef) {
	scope := parentScope.NewSubScope()
	result = &TcProcDef{}
	scope.CurrentProc = result
	result.Name = def.Name.source
	for _, arg := range def.Args {
		tcArg := scope.NewSymbol(arg.Name, SkProcArg, tc.LookUpType(scope, arg.Type))
		result.Args = append(result.Args, tcArg)
	}
	resultType := tc.LookUpType(scope, def.ResultType)
	result.ResultType = resultType
	result.Body = tc.TypeCheckExpr(scope, def.Body, resultType)

	// TODO this is very ugly, store a pointer to a local, return a copy
	parentScope.Procedures[result.Name] = result
	return
}

func (tc *TypeChecker) Errorf(node AstNode, msg string, args ...interface{}) error {
	line, columnStart, columnEnd := tc.LineColumnNode(node)
	return fmt.Errorf("%s(%d, %d-%d) Error: %s", tc.filename, line, columnStart, columnEnd,
		fmt.Sprintf(msg, args...))
}

func (tc *TypeChecker) ExpectType(node AstNode, gotten, expected Type) {
	// TODO this doesn't work for partial types (e.g. array[<unspecified>])
	if expected != TypeUnspecified && expected != gotten {
		panic(tc.Errorf(node, "expected type '%s' but got type '%s'",
			AstFormat(expected), AstFormat(gotten)))
	}
}

func (tc *TypeChecker) ExpectArgsLen(node AstNode, gotten, expected int) {
	if expected != gotten {
		panic(tc.Errorf(node, "expected %d arguments, but got %d",
			expected, gotten))
	}
}

func (tc *TypeChecker) ExpectMinArgsLen(node AstNode, gotten, expected int) {
	if gotten < expected {
		panic(tc.Errorf(node, "Expected at least %d arguments, but got %d.",
			expected, gotten))
	}
}

func (tc *TypeChecker) TypeCheckPrintfArgs(scope Scope, printfSym TcProcSymbol, args []Expr) (result []TcExpr) {
	result = make([]TcExpr, 0, len(args))

	prefixArgs := printfSym.Impl.Args
	tc.ExpectMinArgsLen(printfSym, len(args), len(prefixArgs))

	for i := 0; i < len(prefixArgs); i++ {
		expectedType := prefixArgs[i].Typ
		tcArg := tc.TypeCheckExpr(scope, args[i], expectedType)
		result = append(result, tcArg)
	}

	formatExpr := tc.TypeCheckExpr(scope, args[len(prefixArgs)], TypeString)
	result = append(result, formatExpr)
	i := len(prefixArgs) + 1
	// format string must me a string literal
	formatStr := formatExpr.(StrLit).Value
	for j := 0; j < len(formatStr); j++ {
		c1 := formatStr[j]
		if c1 != '%' {
			continue
		}
		j++
		if j == len(formatStr) {
			panic(tc.Errorf(formatExpr, "incomplete format expr at end of format string"))
		}
		c2 := formatStr[j]
		var argType Type
		switch c2 {
		case '%':
			continue
		case 's':
			argType = TypeString
		case 'd':
			argType = TypeInt
		case 'f':
			argType = TypeFloat
		default:
			panic(tc.Errorf(formatExpr, "invalid format expr %%%c in %s", c2, AstFormat(formatExpr)))
		}
		if i == len(args) {
			panic(tc.Errorf(formatExpr, "not enough arguments for %s", AstFormat(formatExpr)))
		}
		tcArg := tc.TypeCheckExpr(scope, args[i], argType)
		result = append(result, tcArg)
		i++
	}

	return result
}

func (tc *TypeChecker) TypeCheckCall(scope Scope, call Call, expected Type) (result TcCall) {
	procSym := tc.LookUpProc(scope, call.Callee.(Ident))
	tc.ExpectType(call, procSym.Impl.ResultType, expected)
	result.Sym = procSym
	if procSym.Impl.printfargs {
		result.Args = tc.TypeCheckPrintfArgs(scope, procSym, call.Args)
		return
	}
	result.Args = make([]TcExpr, 0, len(call.Args))
	expectedArgs := procSym.Impl.Args

	tc.ExpectArgsLen(call, len(call.Args), len(expectedArgs))

	for i, arg := range call.Args {
		expectedType := expectedArgs[i].Typ
		tcArg := tc.TypeCheckExpr(scope, arg, expectedType)
		result.Args = append(result.Args, tcArg)
	}
	return
}

func (tc *TypeChecker) TypeCheckCodeBlock(scope Scope, arg CodeBlock, expected Type) TcCodeBlock {
	var result TcCodeBlock
	N := len(arg.Items)
	if N > 0 {
		result.Items = make([]TcExpr, N)
		for i, item := range arg.Items {
			if i == N-1 {
				result.Items[i] = tc.TypeCheckExpr(scope, item, expected)
			} else {
				result.Items[i] = tc.TypeCheckExpr(scope, item, TypeVoid)
			}
		}
	} else {
		// empty block is type void
		tc.ExpectType(arg, TypeVoid, expected)
	}
	return result
}

func (expr TypeExpr) IsSet() bool {
	return expr.source != ""
}

func (tc *TypeChecker) TypeCheckVariableDefStmt(scope Scope, arg VariableDefStmt) TcVariableDefStmt {
	var expected Type = TypeUnspecified
	if arg.TypeExpr.IsSet() {
		expected = tc.LookUpType(scope, arg.TypeExpr)
	}

	var result TcVariableDefStmt
	result.Value = tc.TypeCheckExpr(scope, arg.Value, expected)
	result.Sym = scope.NewSymbol(arg.Name, arg.Kind, result.Value.Type())
	return result
}

func (tc *TypeChecker) TypeCheckReturnStmt(scope Scope, arg ReturnStmt) (result TcReturnStmt) {
	result.Value = tc.TypeCheckExpr(scope, arg.Value, scope.CurrentProc.ResultType)
	return
}

func (block TcCodeBlock) Type() Type {
	if len(block.Items) == 0 {
		return TypeVoid
	}
	return block.Items[len(block.Items)-1].Type()
}

func (call TcCall) Type() Type {
	return call.Sym.Impl.ResultType
}

func (lit StrLit) Type() Type {
	return TypeString
}

func (lit CharLit) Type() Type {
	return TypeChar
}

func (lit IntLit) Type() Type {
	return TypeInt
}

func (lit TcArrayLit) Type() Type {
	var result ArrayType
	result.Len = len(lit.Items)
	if len(lit.Items) > 0 {
		result.Elem = lit.Items[0].Type()
	} else {
		result.Elem = TypeNoReturn
	}
	return &result
}

func (sym TcSymbol) Type() Type {
	return sym.Typ
}

func (stmt TcVariableDefStmt) Type() Type {
	return TypeVoid
}

func (stmt TcForLoopStmt) Type() Type {
	return TypeVoid
}

func UnifyType(a, b Type) Type {
	if a != b {
		panic("type incompatible")
	}
	return a
}

func (stmt TcIfStmt) Type() Type {
	return TypeVoid
}

func (stmt TcIfElseStmt) Type() Type {
	return UnifyType(stmt.Body.Type(), stmt.Else.Type())
}

func (returnStmt TcReturnStmt) Type() Type {
	return TypeNoReturn
}

func MatchNegativeNumber(arg Call) (number IntLit, ok bool) {
	if ident, kk := arg.Callee.(Ident); kk && ident.source == "-" {
		if len(arg.Args) == 1 {
			switch lit := arg.Args[0].(type) {
			case IntLit:
				ok = true
				number = lit
			}
		}
	}
	return
}

func (tc *TypeChecker) TypeCheckExpr(scope Scope, arg Expr, expected Type) TcExpr {
	var result TcExpr
	switch arg := arg.(type) {
	case Call:
		// HACK: support for negative literals
		if number, ok := MatchNegativeNumber(arg); ok {
			number.Value = -number.Value
			number.source = arg.source
			return (TcExpr)(number)
		}
		return (TcExpr)(tc.TypeCheckCall(scope, arg, expected))
	case CodeBlock:
		return (TcExpr)(tc.TypeCheckCodeBlock(scope, arg, expected))
	case Ident:
		sym := tc.LookUpLetSym(scope, arg)
		tc.ExpectType(sym, sym.Typ, expected)
		return (TcExpr)(sym)
	case StrLit:
		tc.ExpectType(arg, TypeString, expected)
		return (TcExpr)(arg)
	case CharLit:
		tc.ExpectType(arg, TypeChar, expected)
		return (TcExpr)(arg)
	case IntLit:
		tc.ExpectType(arg, TypeInt, expected)
		return (TcExpr)(arg)
	case ReturnStmt:
		// ignoring expected type here, because the return as expression
		// never evaluates to anything
		return (TcExpr)(tc.TypeCheckReturnStmt(scope, arg))
	case VariableDefStmt:
		tc.ExpectType(arg, TypeVoid, expected)
		return (TcExpr)(tc.TypeCheckVariableDefStmt(scope, arg))
	case ForLoopStmt:
		tc.ExpectType(arg, TypeVoid, expected)
		return (TcExpr)(tc.TypeCheckForLoopStmt(scope, arg))
	case IfStmt:
		tc.ExpectType(arg, TypeVoid, expected)
		return (TcExpr)(tc.TypeCheckIfStmt(scope, arg))
	case IfElseStmt:
		return (TcExpr)(tc.TypeCheckIfElseStmt(scope, arg, expected))
	case ArrayLit:
		return (TcExpr)(tc.TypeCheckArrayLit(scope, arg, expected))
	default:
		panic(tc.Errorf(arg, "not implemented %T", arg))
	}

	return result
}

type ArrayTypeMapKey struct {
	elem Type
	len  int
}

var arrayTypeMap map[ArrayTypeMapKey]*ArrayType

func GetArrayType(elem Type, len int) (result *ArrayType) {
	// TODO all types in the `Type` interface must be pointer types
	result = arrayTypeMap[ArrayTypeMapKey{elem, len}]
	if result == nil {
		result = &ArrayType{Elem: elem, Len: len}
		arrayTypeMap[ArrayTypeMapKey{elem, len}] = result
	}
	return
}

func (tc *TypeChecker) TypeCheckArrayLit(scope Scope, arg ArrayLit, expected Type) (result TcArrayLit) {
	// TODO expect use expect length
	//expectedLen := expected.(ArrayType).Len
	expected = expected.(*ArrayType).Elem
	result.Items = make([]TcExpr, len(arg.Items))

	for i, item := range arg.Items {
		result.Items[i] = tc.TypeCheckExpr(scope, item, expected)
	}
	return
}

func (tc *TypeChecker) TypeCheckIfStmt(scope Scope, stmt IfStmt) (result TcIfStmt) {
	// currently only iteration on strings in possible (of course that is not final)
	result.Condition = tc.TypeCheckExpr(scope, stmt.Condition, TypeBoolean)
	result.Body = tc.TypeCheckExpr(scope, stmt.Body, TypeVoid)
	return
}

func (tc *TypeChecker) TypeCheckIfElseStmt(scope Scope, stmt IfElseStmt, expected Type) (result TcIfElseStmt) {
	// currently only iteration on strings in possible (of course that is not final)
	fmt.Println(AstFormat(stmt))
	result.Condition = tc.TypeCheckExpr(scope, stmt.Condition, TypeBoolean)
	result.Body = tc.TypeCheckExpr(scope, stmt.Body, expected)
	result.Else = tc.TypeCheckExpr(scope, stmt.Else, expected)
	return
}

// TODO ElementType should be some form of language feature
func (tc *TypeChecker) ElementType(expr TcExpr) Type {
	switch typ := expr.Type().(type) {
	case *ArrayType:
		return typ.Elem
	case *BuiltinType:
		if typ.name == "string" {
			return TypeChar
		}
	}
	panic(tc.Errorf(expr, "expect type with elements to iterate over"))
}

func (tc *TypeChecker) TypeCheckForLoopStmt(scope Scope, loopArg ForLoopStmt) (result TcForLoopStmt) {
	scope = scope.NewSubScope()
	// currently only iteration on strings in possible (of course that is not final)
	result.Collection = tc.TypeCheckExpr(scope, loopArg.Collection, TypeUnspecified)
	elementType := tc.ElementType(result.Collection)
	result.LoopSym = scope.NewSymbol(loopArg.LoopIdent, SkLoopIterator, elementType)
	result.Body = tc.TypeCheckExpr(scope, loopArg.Body, TypeVoid)
	return
}

func (tc *TypeChecker) TypeCheckPackage(arg PackageDef) (result TcPackageDef) {
	scope := builtinScope.NewSubScope()
	result.Name = arg.Name
	for _, typeDef := range arg.TypeDefs {
		result.TypeDefs = append(result.TypeDefs, tc.TypeCheckStructDef(scope, typeDef))
	}
	for _, procDef := range arg.ProcDefs {
		procDef := tc.TypeCheckProcDef(scope, procDef)
		result.ProcDefs = append(result.ProcDefs, procDef)
		if procDef.Name == "main" {
			result.Main = procDef
		}
	}
	return
}
