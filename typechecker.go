package main

import (
	"fmt"
)

type BuiltinType struct {
	name string
}

func (typ BuiltinType) Source() string {
	// should this panic?
	return ""
}

type TypeChecker struct {
	code string
	filename string
}

func NewTypeChecker(code, filename string) *TypeChecker {
	return &TypeChecker{code: code, filename: filename}
}

func (typ *BuiltinType) Name() string {
	return typ.name
}

// **** Constants ****

// These type names are by no means final, there are just to get
// something working.
var TypeBoolean = &BuiltinType{"bool"}
var TypeInt = &BuiltinType{"int"}
var TypeFloat = &BuiltinType{"float"}
var TypeString = &BuiltinType{"string"}
var TypeChar = &BuiltinType{"char"}
var TypeVoid = &BuiltinType{"void"}

// This type is used to tag that a function never returns.
var TypeNoReturn = &BuiltinType{"noreturn"}

// this type is the internal representation when no type has been
// specified. It is not a type by its own.
var TypeUnspecified = &BuiltinType{"<unspecified>"}

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

func (scope Scope) LookUpType(expr TypeExpr) Type {
	// TODO really slow lookup, should really be faster
	name := expr.Ident.source
	for key, value := range scope.Types {
		if key == name {
			return value
		}
	}
	if scope.Parent != nil {
		return scope.Parent.LookUpType(expr)
	}
	panic(fmt.Sprintf("Type not found: %s", name))
}

func (scope Scope) LookUpProc(ident Ident) TcProcSymbol {
	if scope == nil {
		panic(fmt.Sprintf("Proc not found: %s", ident.source))
	}

	if impl, ok := scope.Procedures[ident.source]; ok {
		return TcProcSymbol{Name: ident.source, Impl: impl}
	}
	return scope.Parent.LookUpProc(ident)
}

func (scope Scope) LookUpLetSym(ident Ident) TcSymbol {
	if scope == nil {
		panic(fmt.Sprintf("let sym not found: %s", ident.source))
	}

	if sym, ok := scope.Variables[ident.source]; ok {
		return sym
	}
	return scope.Parent.LookUpLetSym(ident)
}

func (tc *TypeChecker) TypeCheckStructDef(scope Scope, def StructDef) TcStructDef {
	var result TcStructDef
	result.Name = def.Name.source
	for _, field := range def.Fields {
		var tcField TcStructField
		tcField.Name = field.Name.source
		tcField.Type = scope.LookUpType(field.TypeExpr)
		result.Fields = append(result.Fields, tcField)
	}
	return result
}

func (tc *TypeChecker) TypeCheckProcDef(parentScope Scope, def ProcDef) (result TcProcDef) {
	scope := parentScope.NewSubScope()
	scope.CurrentProc = &result
	result.Name = def.Name.source
	for _, arg := range def.Args {
		tcArg := scope.NewSymbol(arg.Name, SkProcArg, scope.LookUpType(arg.Type))
		result.Args = append(result.Args, tcArg)
	}
	resultType := scope.LookUpType(def.ResultType)
	result.ResultType = resultType
	result.Body = tc.TypeCheckExpr(scope, def.Body, resultType)

	// TODO this is very ugly, store a pointer to a local, return a copy
	parentScope.Procedures[result.Name] = &result
	return
}

func ExpectType(gotten, expected Type) {
	if expected != TypeUnspecified && expected != gotten {
		// TODO print proper line information here
		panic(fmt.Sprintf("Expected type '%s' but got type '%s'", expected.Name(), gotten.Name()))
	}
}

func ExpectArgsLen(gotten, expected int) {
	if expected != gotten {
		panic(fmt.Sprintf("Expected %d arguments, but got %d.",
			expected, gotten))
	}
}

func ExpectMinArgsLen(gotten, expected int) {
	if gotten < expected {
		panic(fmt.Sprintf("Expected at least %d arguments, but got %d.",
			expected, gotten))
	}
}

func (tc *TypeChecker) TypeCheckPrintfArgs(scope Scope, printfSym TcProcSymbol, args []Expr) (result []TcExpr) {
	result = make([]TcExpr, 0, len(args))

	prefixArgs := printfSym.Impl.Args
	ExpectMinArgsLen(len(args), len(prefixArgs))

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
			panic(fmt.Sprintf("incomplete format expr at end of format string", formatStr))
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
			panic(fmt.Sprintf("invalid format expr %%%c in %s", c2, AstFormat(formatExpr)))
		}
		if i == len(args) {
			panic(fmt.Sprintf("not enough arguments for %s", AstFormat(formatExpr)))
		}
		tcArg := tc.TypeCheckExpr(scope, args[i], argType)
		result = append(result, tcArg)
		i++
	}

	return result
}

func (tc *TypeChecker) TypeCheckCall(scope Scope, call Call, expected Type) (result TcCall) {
	procSym := scope.LookUpProc(call.Callee.(Ident))
	ExpectType(procSym.Impl.ResultType, expected)
	result.Sym = procSym
	if procSym.Impl.printfargs {
		result.Args = tc.TypeCheckPrintfArgs(scope, procSym, call.Args)
		return
	}
	result.Args = make([]TcExpr, 0, len(call.Args))
	expectedArgs := procSym.Impl.Args
	ExpectArgsLen(len(call.Args), len(expectedArgs))
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
		ExpectType(TypeVoid, expected)
	}
	return result
}

func (expr TypeExpr) IsSet() bool {
	return expr.source != ""
}

func (tc *TypeChecker) TypeCheckVariableDefStmt(scope Scope, arg VariableDefStmt) TcVariableDefStmt {
	var expected Type = TypeUnspecified
	if arg.TypeExpr.IsSet() {
		expected = scope.LookUpType(arg.TypeExpr)
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

func (tc *TypeChecker) TypeCheckExpr(scope Scope, arg Expr, expected Type) TcExpr {
	var result TcExpr
	switch arg := arg.(type) {
	case Call:
		return (TcExpr)(tc.TypeCheckCall(scope, arg, expected))
	case CodeBlock:
		return (TcExpr)(tc.TypeCheckCodeBlock(scope, arg, expected))
	case Ident:
		sym := scope.LookUpLetSym(arg)
		ExpectType(sym.Typ, expected)
		return (TcExpr)(sym)
	case StrLit:
		ExpectType(TypeString, expected)
		return (TcExpr)(arg)
	case CharLit:
		ExpectType(TypeChar, expected)
		return (TcExpr)(arg)
	case IntLit:
		ExpectType(TypeInt, expected)
		return (TcExpr)(arg)
	case ReturnStmt:
		// ignoring expected type here, because the return as expression
		// never evaluates to anything
		return (TcExpr)(tc.TypeCheckReturnStmt(scope, arg))
	case VariableDefStmt:
		ExpectType(TypeVoid, expected)
		return (TcExpr)(tc.TypeCheckVariableDefStmt(scope, arg))
	case ForLoopStmt:
		ExpectType(TypeVoid, expected)
		return (TcExpr)(tc.TypeCheckForLoopStmt(scope, arg))
	case IfStmt:
		ExpectType(TypeVoid, expected)
		return (TcExpr)(tc.TypeCheckIfStmt(scope, arg))
	case IfElseStmt:
		return (TcExpr)(tc.TypeCheckIfElseStmt(scope, arg, expected))
	default:
		panic(fmt.Sprintf("not implemented %T", arg))
	}

	// TODO not implemented
	return result
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

func (tc *TypeChecker) TypeCheckForLoopStmt(scope Scope, loopArg ForLoopStmt) (result TcForLoopStmt) {
	scope = scope.NewSubScope()
	// currently only iteration on strings in possible (of course that is not final)
	result.Collection = tc.TypeCheckExpr(scope, loopArg.Collection, TypeString)
	result.LoopSym = scope.NewSymbol(loopArg.LoopIdent, SkLoopIterator, TypeChar)
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
		result.ProcDefs = append(result.ProcDefs, tc.TypeCheckProcDef(scope, procDef))
	}
	return
}
