package main

import (
	"fmt"
	"strings"
)

type CompileError struct {
	node AstNode
	msg  string
}

type TypeChecker struct {
	code         string
	filename     string
	silentErrors bool
	errors       []CompileError
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
	Procedures  map[string][]*TcProcDef
	Types       map[string]Type
}

type Scope = *ScopeImpl

func (scope Scope) NewSubScope() Scope {
	return &ScopeImpl{
		Parent:      scope,
		CurrentProc: scope.CurrentProc,
		Variables:   make(map[string]TcSymbol),
		Procedures:  make(map[string][]*TcProcDef),
		Types:       make(map[string]Type),
	}
}

func (scope Scope) NewSymbol(tc *TypeChecker, name Ident, kind SymbolKind, typ Type) TcSymbol {
	//result := TcSymbol{Name: name.source, Kind: kind, Typ: typ}
	rawName := name.Source
	result := TcSymbol{Source: name.Source, Kind: kind, Typ: typ}
	_, alreadyExists := scope.Variables[rawName]
	if alreadyExists {
		tc.ReportErrorf(name, "redefinition of %s", rawName)
	}
	scope.Variables[name.Source] = result
	return result
}

func (tc *TypeChecker) LookUpType(scope Scope, expr TypeExpr) Type {
	if expr.Ident.Source == "array" {
		if !tc.ExpectArgsLen(expr, len(expr.TypeArgs), 1) {
			return TypeError
		}
		if !tc.ExpectArgsLen(expr, len(expr.ExprArgs), 1) {
			return TypeError
		}
		elem := tc.LookUpType(scope, expr.TypeArgs[0])
		// tc.ExpectMinArgsLen(node AstNode, gotten int, expected int)
		// tc.ExpectMinArgsLen(node AstNode, gotten int, expected int)
		// len := expr.ExprArgs[0].(IntLit).Value
		lenLit, ok := expr.ExprArgs[0].(IntLit)
		if !ok {
			tc.ReportErrorf(expr.ExprArgs[0], "Expect Int Lit")
			return TypeError
		}
		return GetArrayType(elem, lenLit.Value)
	}
	if expr.Ident.Source == "set" {
		if !tc.ExpectArgsLen(expr, len(expr.TypeArgs), 1) {
			return TypeError
		}
		if !tc.ExpectArgsLen(expr, len(expr.ExprArgs), 0) {
			return TypeError
		}
		elem, ok := tc.LookUpType(scope, expr.TypeArgs[0]).(*TcEnumDef)
		if !ok {
			tc.ReportErrorf(expr.TypeArgs[0], "expect enum type")
		}
		return GetEnumSetType(elem)
	}

	// TODO really slow lookup, should really be faster
	name := expr.Ident.Source
	for key, value := range scope.Types {
		if key == name {
			return value
		}
	}
	if scope.Parent != nil {
		return tc.LookUpType(scope.Parent, expr)
	}
	tc.ReportErrorf(expr, "Type not found: %s", name)
	return TypeError
}

func (tc *TypeChecker) LookUpProc(scope Scope, ident Ident, procSyms []TcProcSymbol) []TcProcSymbol {
	for scope != nil {
		if impls, ok := scope.Procedures[ident.Source]; ok {
			for _, impl := range impls {
				procSym := TcProcSymbol{Name: ident.Source, Impl: impl}
				procSyms = append(procSyms, procSym)
			}
		}
		scope = scope.Parent
	}
	return procSyms
}

func (tc *TypeChecker) LookUpLetSymRecursive(scope Scope, ident Ident) TcSymbol {
	if scope == nil {
		tc.ReportErrorf(ident, "let sym not found: %s", ident.Source)
		var result TcSymbol
		result.Kind = SkLet
		result.Source = ident.Source
		result.Typ = TypeError
		return result
	}

	if sym, ok := scope.Variables[ident.Source]; ok {
		sym.Source = ident.Source
		return sym
	}
	return tc.LookUpLetSymRecursive(scope.Parent, ident)
}

func (tc *TypeChecker) LookUpLetSym(scope Scope, ident Ident, expected Type) (result TcSymbol) {
	if enumDef, ok := expected.(*TcEnumDef); ok {
		for _, sym := range enumDef.Values {
			if sym.Source == ident.Source {
				// change line info
				sym.Source = ident.Source
				return sym
			}
		}
	}
	result = tc.LookUpLetSymRecursive(scope, ident)
	result.Typ = tc.ExpectType(result, result.Typ, expected)
	return
}

func (tc *TypeChecker) LineColumnNode(node AstNode) (line, columnStart, columnEnd int) {
	return LineColumnStr(tc.code, node.GetSource())
}

func (tc *TypeChecker) TypeCheckStructDef(scope Scope, def TypeDef) Type {
	switch def.Kind.Source {
	case "struct":
		result := &TcStructDef{}
		result.Name = def.Name.Source
		for _, item := range def.Body.Items {
			if colonExpr, ok := item.(ColonExpr); !ok {
				tc.ReportErrorf(item, "expect ColonExpr, but got %T", item)
			} else {
				if nameIdent, ok := colonExpr.Lhs.(Ident); !ok {
					tc.ReportErrorf(colonExpr.Lhs, "expect Ident, but got %T", colonExpr.Lhs)
				} else {
					var tcField TcStructField
					tcField.Name = nameIdent.Source
					tcField.Type = tc.LookUpType(scope, colonExpr.Rhs)
					result.Fields = append(result.Fields, tcField)
				}
			}
		}
		scope.Types[result.Name] = result
		return result
	case "enum":
		result := &TcEnumDef{}
		result.Name = def.Name.Source
		for _, item := range def.Body.Items {
			if ident, ok := item.(Ident); !ok {
				tc.ReportErrorf(item, "expect Ident, but got %T", item)
			} else {
				var sym TcSymbol
				sym.Source = ident.Source
				sym.Kind = SkEnum
				sym.Typ = result
				result.Values = append(result.Values, sym)
			}
		}
		scope.Types[result.Name] = result
		registerBuiltin("string", fmt.Sprintf("%s_names_array[", result.Name), "", "]", []Type{result}, TypeString)
		for _, typ := range TypeAnyInt.items {
			builtinType := typ.(*BuiltinType)
			registerBuiltin(builtinType.name, fmt.Sprintf("(%s)", builtinType.internalName), "", "", []Type{result}, typ)
			registerBuiltin(result.Name, fmt.Sprintf("(%s)", result.Name), "", "", []Type{typ}, result)
		}
		registerBuiltin("contains", "(((", ") & (1 << (", "))) != 0)", []Type{GetEnumSetType(result), result}, TypeBoolean)
		return result
	case "union":
		panic("not implemented union")
	default:
		tc.ReportErrorf(def.Kind, "invalid type kind %s, expect one of struct, enum, union", def.Kind.Source)
		return nil
	}
}

func (tc *TypeChecker) TypeCheckProcDef(parentScope Scope, def ProcDef) (result *TcProcDef) {
	scope := parentScope.NewSubScope()
	result = &TcProcDef{}
	scope.CurrentProc = result
	result.Name = def.Name.Source
	mangledNameBuilder := &strings.Builder{}
	mangledNameBuilder.WriteString(def.Name.Source)
	mangledNameBuilder.WriteRune('_')
	for _, arg := range def.Args {
		typ := tc.LookUpType(scope, arg.Type)
		tcArg := scope.NewSymbol(tc, arg.Name, SkProcArg, typ)
		result.Args = append(result.Args, tcArg)
		typ.ManglePrint(mangledNameBuilder)
	}
	resultType := tc.LookUpType(scope, def.ResultType)
	result.ResultType = resultType
	result.Body = tc.TypeCheckExpr(scope, def.Body, resultType)
	parentScope.Procedures[result.Name] = append(parentScope.Procedures[result.Name], result)

	// TODO, don't special case it like this here
	if def.Name.Source == "main" {
		result.Prefix = "main("
	} else {
		mangledNameBuilder.WriteRune('(')
		result.Prefix = mangledNameBuilder.String()
	}
	result.Infix = ", "
	result.Postfix = ")"

	return result
}

func (tc *TypeChecker) ReportErrorf(node AstNode, msg string, args ...interface{}) {
	newMsg := fmt.Sprintf(msg, args...)
	tc.errors = append(tc.errors, CompileError{node: node, msg: newMsg})
	if !tc.silentErrors {
		if node == nil {
			fmt.Println(msg)
		} else {
			line, columnStart, columnEnd := tc.LineColumnNode(node)
			fmt.Printf("%s(%d, %d-%d) Error: %s\n", tc.filename, line, columnStart, columnEnd, newMsg)
		}
	}
}

func (tc *TypeChecker) ReportInvalidDocCommentKey(section NamedDocSection) {
	tc.ReportErrorf(section, "invalid doc comment key: %s", section.Name)
}

func (tc *TypeChecker) ReportIllegalDocComment(doc DocComment) {
	tc.ReportErrorf(doc, "doc comment is illegal here, use normal comment instead")
}

func (tc *TypeChecker) ExpectType(node AstNode, gotten, expected Type) Type {
	// TODO this doesn't work for partial types (e.g. array[<unspecified>])
	// TODO this should have some cleanup, it has redundant code and seems to be error prone
	if expected == TypeError {
		return TypeError
	}
	if gotten == TypeError {
		return TypeError
	}

	if expected == TypeUnspecified {
		if gotten == TypeUnspecified {
			tc.ReportErrorf(node, "expression type is unspecified")
			return TypeError
		}
		if gottenTg, ok := gotten.(*TypeGroup); ok {
			tc.ReportErrorf(node, "narrowing of type '%s' is required", AstFormat(gottenTg))
			return TypeError
		}
		return gotten
	}
	if expectedTg, ok := expected.(*TypeGroup); ok {
		if gotten == TypeUnspecified {
			tc.ReportErrorf(node, "narrowing of type '%s' is required", AstFormat(expectedTg))
			return TypeError
		}
		if gottenTg, ok := gotten.(*TypeGroup); ok {
			tc.ReportErrorf(node, "internal error: narrowing of type group '%s' and '%s' is not implemented yet", AstFormat(gottenTg), AstFormat(expectedTg))
			return TypeError
		}
		for _, it := range expectedTg.items {
			if gotten == it {
				return gotten
			}
		}
		tc.ReportErrorf(node, "expected type '%s' but got type '%s'", AstFormat(expected), AstFormat(gotten))
		return TypeError
	}

	if gotten == TypeUnspecified {
		return expected
	}
	if gottenTg, ok := gotten.(*TypeGroup); ok {
		for _, it := range gottenTg.items {
			if it == expected {
				return expected
			}
		}
	}

	if expected == gotten {
		return expected
	}

	tc.ReportErrorf(node, "expected type '%s' but got type '%s'",
		AstFormat(expected), AstFormat(gotten))
	return TypeError
}

func (tc *TypeChecker) ExpectArgsLen(node AstNode, gotten, expected int) bool {
	if expected != gotten {
		tc.ReportErrorf(node, "expected %d arguments, but got %d", expected, gotten)
		return false
	}
	return true
}

func (tc *TypeChecker) ExpectMinArgsLen(node AstNode, gotten, expected int) bool {
	if gotten < expected {
		tc.ReportErrorf(node, "Expected at least %d arguments, but got %d.", expected, gotten)
		return false
	}
	return true
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
	formatStrC := &strings.Builder{}
	for j := 0; j < len(formatStr); j++ {
		c1 := formatStr[j]
		formatStrC.WriteByte(c1)
		if c1 != '%' {
			continue
		}
		j++
		if j == len(formatStr) {
			tc.ReportErrorf(formatExpr, "incomplete format expr at end of format string")
			break
		}
		c2 := formatStr[j]
		var argType Type
		switch c2 {
		case '%':
			formatStrC.WriteRune('%')
			continue
		case 's':
			argType = TypeString
		case 'd':
			argType = TypeAnyInt
		case 'f':
			argType = TypeAnyFloat
		default:
			tc.ReportErrorf(formatExpr, "invalid format expr %%%c in %s", c2, AstFormat(formatExpr))
			argType = TypeError
		}
		if i == len(args) {
			tc.ReportErrorf(formatExpr, "not enough arguments for %s", AstFormat(formatExpr))
			break
		}
		tcArg := tc.TypeCheckExpr(scope, args[i], argType)
		switch tcArg.Type() {
		case TypeInt8:
			formatStrC.WriteString("hhd")
		case TypeInt16:
			formatStrC.WriteString("hd")
		case TypeInt32:
			formatStrC.WriteString("d")
		case TypeInt64:
			formatStrC.WriteString("ld")
		case TypeString:
			formatStrC.WriteString("s")
		case TypeFloat32:
			formatStrC.WriteString("f")
		case TypeFloat64:
			formatStrC.WriteString("f")
		case TypeError:
			formatStrC.WriteString("<error>")
		}
		result = append(result, tcArg)
		i++
	}

	result[len(prefixArgs)] = StrLit{Value: formatStrC.String()}

	//formatExpr.(StrLit).Value = formatStrC.String()
	return result
}

func (tc *TypeChecker) TypeCheckDotExpr(scope Scope, lhs, rhs Expr, expected Type) (result TcExpr) {

	tcLhs := tc.TypeCheckExpr(scope, lhs, TypeUnspecified)
	switch t := tcLhs.Type().(type) {
	case *TcStructDef:
		tcRhs, idx := t.GetField(rhs.GetSource())
		if idx < 0 {
			tc.ReportErrorf(rhs, "type %s has no field %s", t.Name, rhs.GetSource())
		}
		tc.ExpectType(rhs, tcRhs.Type, expected)
		return TcDotExpr{Lhs: tcLhs, Rhs: tcRhs}
	default:
		panic("dot call is only supported on struct field")
	}
}

func errorProcSym(ident Ident) TcProcSymbol {
	return TcProcSymbol{
		Name: ident.GetSource(),
		Impl: nil,
	}
}

func (tc *TypeChecker) TypeCheckCall1(scope Scope, procSym TcProcSymbol, checkedArgs []TcExpr, uncheckedArgs []Expr, expected Type) (result TcCall) {
	result.Sym = procSym
	if procSym.Impl.printfargs {
		if len(checkedArgs) > 0 {
			panic("not implemented")
		}
		result.Args = tc.TypeCheckPrintfArgs(scope, procSym, uncheckedArgs)
	} else {
		result.Args = checkedArgs
		expectedArgs := procSym.Impl.Args
		tc.ExpectArgsLen(procSym, len(checkedArgs)+len(uncheckedArgs), len(expectedArgs))
		for i, arg := range uncheckedArgs {
			expectedType := expectedArgs[len(checkedArgs)+i].Typ
			tcArg := tc.TypeCheckExpr(scope, arg, expectedType)
			result.Args = append(result.Args, tcArg)
		}
	}
	// discard the type, it can't be stored in TcCall. Error reporting is still happening though.
	tc.ExpectType(procSym, procSym.Impl.ResultType, expected)
	return result
}

func argTypeGroupAtIndex(symChoice []TcProcSymbol, idx int) Type {
	switch len(symChoice) {
	case 0:
		return TypeUnspecified
	case 1:
		return symChoice[0].Impl.Args[idx].Typ
	case 2:
		typ1 := symChoice[0].Impl.Args[idx].Typ
		typ2 := symChoice[1].Impl.Args[idx].Typ
		if typ1 == typ2 {
			return typ1
		}
		return &TypeGroup{items: []Type{typ1, typ2}}
	default:
		types := []Type{}
	outer:
		for _, sym := range symChoice {
			argType := sym.Impl.Args[idx].Typ
			for _, typ := range types {
				if argType == typ {
					continue outer
				}
			}
			types = append(types, argType)
		}
		if len(types) == 1 {
			return types[0]
		}
		return &TypeGroup{items: types}
	}
}

func (tc *TypeChecker) TypeCheckCall(scope Scope, call Call, expected Type) TcExpr {
	ident := call.Callee.(Ident)
	if ident.Source == "." && len(call.Args) == 2 {
		return tc.TypeCheckDotExpr(scope, call.Args[0], call.Args[1], expected)
	}

	procSyms := tc.LookUpProc(scope, ident, nil)

	var result TcCall
	switch len(procSyms) {
	case 0:
		tc.ReportErrorf(ident, "proc not found: %s", ident.Source)
		result.Sym = errorProcSym(ident)
		var tcArgs []TcExpr
		for _, arg := range call.Args {
			tcArgs = append(tcArgs, tc.TypeCheckExpr(scope, arg, TypeUnspecified))
		}
		result.Args = tcArgs
		return result
	default:

		// TODO: this is very quick and dirty. These three branches must be merged into one general algorithm
		var checkedArgs []TcExpr
		for i, arg := range call.Args {
			if len(procSyms) == 1 {
				return tc.TypeCheckCall1(scope, procSyms[0], checkedArgs, call.Args[i:], expected)
			}

			expectedArgType := argTypeGroupAtIndex(procSyms, i)
			tcArg := tc.TypeCheckExpr(scope, arg, expectedArgType)
			checkedArgs = append(checkedArgs, tcArg)

			// filter procedures for right one
			n := 0
			for _, procSym := range procSyms {
				expectedArg := procSym.Impl.Args[i]
				if tcArg.Type() == expectedArg.Typ {
					procSyms[n] = procSym
					n++
				}
			}
			procSyms = procSyms[:n]

		}
		if len(procSyms) == 0 {
			builder := &AstPrettyPrinter{}
			builder.WriteString("proc not found: ")
			builder.WriteString(ident.Source)
			builder.WriteRune('(')
			for i, arg := range checkedArgs {
				if i != 0 {
					builder.WriteString(", ")
				}
				arg.Type().prettyPrint(builder)
			}
			builder.WriteRune(')')
			tc.ReportErrorf(ident, "%s", builder.String())
			result.Sym = errorProcSym(ident)
			result.Args = checkedArgs
			return result
		}
		if len(procSyms) > 1 {
			tc.ReportErrorf(ident, "too many overloads: %s", ident.Source)
			result.Sym = errorProcSym(ident)
			result.Args = checkedArgs
			return result
		}

		result.Sym = procSyms[0]
		result.Args = checkedArgs
	}
	tc.ExpectType(call, result.Sym.Impl.ResultType, expected)
	return result
}

func (tc *TypeChecker) ApplyDocComment(expr Expr, doc DocComment) Expr {
	switch expr2 := expr.(type) {
	case VariableDefStmt:
		expr2.Name.Comment = append(expr2.Name.Comment, doc.BaseDoc...)
		for _, it := range doc.NamedDocSections {
			key := it.Name
			value := it.Lines

			if expr2.Name.Source != key {
				tc.ReportInvalidDocCommentKey(it)
				continue
			}

			commentRef := &expr2.Name.Comment
			*commentRef = append(*commentRef, value...)
		}
		return expr2
	case ProcDef:
		expr2.Name.Comment = append(expr2.Name.Comment, doc.BaseDoc...)

	DOCSECTIONS1:
		for _, it := range doc.NamedDocSections {
			key := it.Name
			value := it.Lines

			for i := range expr2.Args {
				if expr2.Args[i].Name.Source == key {
					commentRef := &expr2.Args[i].Name.Comment
					*commentRef = append(*commentRef, value...)
					continue DOCSECTIONS1
				}
			}
			tc.ReportInvalidDocCommentKey(it)
		}
		return expr2
	case TypeDef:
		expr2.Name.Comment = append(expr2.Name.Comment, doc.BaseDoc...)
	DOCSECTIONS2:
		for _, it := range doc.NamedDocSections {
			key := it.Name
			value := it.Lines
			for i := range expr2.Body.Items {
				if colonExpr, ok := expr2.Body.Items[i].(ColonExpr); ok {
					if ident, ok := colonExpr.Lhs.(Ident); ok {
						if ident.Source == key {
							ident.Comment = append(ident.Comment, value...)
							colonExpr.Lhs = ident
							expr2.Body.Items[i] = colonExpr
							continue DOCSECTIONS2
						}
					}
				}
			}
			tc.ReportInvalidDocCommentKey(it)
		}
		return expr2
	default:
		fmt.Printf("typ: %T\n", expr2)
		tc.ReportIllegalDocComment(doc)
		return expr2
	}
}
func (tc *TypeChecker) TypeCheckCodeBlock(scope Scope, arg CodeBlock, expected Type) (result TcCodeBlock) {
	scope = scope.NewSubScope()
	N := len(arg.Items)
	if N > 0 {
		resultItems := make([]TcExpr, 0, N)
		var docComment DocComment
		var applyDocComment bool = false
		for i, item := range arg.Items {
			if comment, ok := item.(DocComment); ok {
				docComment = comment
				applyDocComment = true
				continue
			} else {
				if applyDocComment {
					item = tc.ApplyDocComment(item, docComment)
					applyDocComment = false
				}
				if i == N-1 {
					resultItems = append(resultItems, tc.TypeCheckExpr(scope, item, expected))
				} else {
					resultItems = append(resultItems, tc.TypeCheckExpr(scope, item, TypeVoid))
				}
			}
		}
		result.Items = resultItems
	} else {
		// empty block is type void
		tc.ExpectType(arg, TypeVoid, expected)
	}
	return
}

func (expr TypeExpr) IsSet() bool {
	return expr.Source != ""
}

func (tc *TypeChecker) TypeCheckVariableDefStmt(scope Scope, arg VariableDefStmt) (result TcVariableDefStmt) {
	result.Source = arg.Source
	var expected Type = TypeUnspecified
	if arg.TypeExpr.IsSet() {
		expected = tc.LookUpType(scope, arg.TypeExpr)
	}
	if arg.Value == nil {
		result.Value = expected.DefaultValue(tc, arg.TypeExpr)
	} else {
		result.Value = tc.TypeCheckExpr(scope, arg.Value, expected)
	}
	result.Sym = scope.NewSymbol(tc, arg.Name, arg.Kind, result.Value.Type())
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
	impl := call.Sym.Impl
	if impl == nil {
		return TypeError
	}
	return impl.ResultType
}

func (lit StrLit) Type() Type {
	return TypeString
}

func (lit CharLit) Type() Type {
	return TypeChar
}

func (lit IntLit) Type() Type {
	if lit.typ == nil {
		panic(fmt.Errorf("internal error: type of IntLit not set"))
	}
	return lit.typ
}

func (lit FloatLit) Type() Type {
	if lit.typ == nil {
		panic(fmt.Errorf("internal error: type of FloatLit not set"))
	}
	return lit.typ
}

func (lit TcArrayLit) Type() Type {
	return GetArrayType(lit.ElemType, int64(len(lit.Items)))
}

func (lit TcEnumSetLit) Type() Type {
	return GetEnumSetType(lit.ElemType)

}

func (lit TcStructLit) Type() Type {
	return lit.typ
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

func (stmt TcIfElseExpr) Type() Type {
	return UnifyType(stmt.Body.Type(), stmt.Else.Type())
}

func (returnStmt TcReturnStmt) Type() Type {
	return TypeNoReturn
}

func (expr TcDotExpr) Type() Type {
	return expr.Rhs.Type
}

func MatchNegativeNumber(arg Call) (number IntLit, ok bool) {
	if ident, kk := arg.Callee.(Ident); kk && ident.Source == "-" {
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

func (tc *TypeChecker) TypeCheckColonExpr(scope Scope, arg ColonExpr, expected Type) TcExpr {
	typ := tc.LookUpType(scope, arg.Rhs)
	typ = tc.ExpectType(arg, typ, expected)
	return tc.TypeCheckExpr(scope, arg.Lhs, typ)
}

func (tc *TypeChecker) TypeCheckExpr(scope Scope, arg Expr, expected Type) TcExpr {
	switch arg := arg.(type) {
	case Call:
		// HACK: support for negative literals
		if number, ok := MatchNegativeNumber(arg); ok {
			number.Value = -number.Value
			number.Source = arg.Source
			return (TcExpr)(number)
		}
		return (TcExpr)(tc.TypeCheckCall(scope, arg, expected))
	case CodeBlock:
		return (TcExpr)(tc.TypeCheckCodeBlock(scope, arg, expected))
	case Ident:
		sym := tc.LookUpLetSym(scope, arg, expected)
		return (TcExpr)(sym)
	case StrLit:
		tc.ExpectType(arg, TypeString, expected)
		return (TcExpr)(arg)
	case CharLit:
		tc.ExpectType(arg, TypeChar, expected)
		return (TcExpr)(arg)
	case IntLit:
		typ := tc.ExpectType(arg, TypeAnyNumber, expected)
		if typ == TypeFloat32 || typ == TypeFloat64 {
			var lit FloatLit
			lit.Source = arg.Source
			lit.Value = float64(arg.Value)
			if typ == TypeFloat32 {
				lit.typ = TypeFloat32
				if int64(float32(lit.Value)) != arg.Value {
					tc.ReportErrorf(arg, "can't represent %d as float32 precisely", arg.Value)
				}
			} else if typ == TypeFloat64 {
				lit.typ = TypeFloat64
				if int64(lit.Value) != arg.Value {
					tc.ReportErrorf(arg, "can't represent %d as float64 precisely", arg.Value)
				}
			}
			return lit
		}
		arg.typ = typ.(*BuiltinType)
		return (TcExpr)(arg)
	case FloatLit:
		typ := tc.ExpectType(arg, TypeAnyFloat, expected)
		arg.typ = typ.(*BuiltinType)
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
	case IfExpr:
		tc.ExpectType(arg, TypeVoid, expected)
		return (TcExpr)(tc.TypeCheckIfStmt(scope, arg))
	case IfElseExpr:
		return (TcExpr)(tc.TypeCheckIfElseStmt(scope, arg, expected))
	case ArrayLit:
		return (TcExpr)(tc.TypeCheckArrayLit(scope, arg, expected))
	case ColonExpr:
		return (TcExpr)(tc.TypeCheckColonExpr(scope, arg, expected))
	default:
		panic(fmt.Errorf("not implemented %T", arg))
	}
}

type ArrayTypeMapKey struct {
	elem Type
	len  int64
}

var arrayTypeMap map[ArrayTypeMapKey]*ArrayType
var enumSetTypeMap map[*TcEnumDef]*EnumSetType

func GetArrayType(elem Type, len int64) (result *ArrayType) {
	// TODO all types in the `Type` interface must be pointer types
	result, ok := arrayTypeMap[ArrayTypeMapKey{elem, len}]
	if !ok {
		result = &ArrayType{Elem: elem, Len: len}
		arrayTypeMap[ArrayTypeMapKey{elem, len}] = result
	}
	return result
}

func GetEnumSetType(elem *TcEnumDef) (result *EnumSetType) {
	// TODO all types in the `Type` interface must be pointer types
	result, ok := enumSetTypeMap[elem]
	if !ok {
		result = &EnumSetType{Elem: elem}
		enumSetTypeMap[elem] = result
	}
	return result
}

func matchAssign(arg Expr) (lhs, rhs Expr, ok bool) {
	call, isCall := arg.(Call)
	if !isCall || len(call.Args) != 2 || call.Callee.GetSource() != "=" {
		return nil, nil, false
	}
	return call.Args[0], call.Args[1], true
}

func (tc *TypeChecker) TypeCheckArrayLit(scope Scope, arg ArrayLit, expected Type) TcExpr {
	// TODO expect use expect length
	// expectedLen := expected.(ArrayType).Len
	//
	switch exp := expected.(type) {
	case *ArrayType:
		var result TcArrayLit
		result.Items = make([]TcExpr, len(arg.Items))
		result.ElemType = exp.Elem
		for i, item := range arg.Items {
			result.Items[i] = tc.TypeCheckExpr(scope, item, exp.Elem)
		}
		return result
	case *EnumSetType:
		var result TcEnumSetLit
		result.Items = make([]TcExpr, len(arg.Items))
		result.ElemType = exp.Elem
		for i, item := range arg.Items {
			result.Items[i] = tc.TypeCheckExpr(scope, item, exp.Elem)
		}
		return result
	case *TcStructDef:
		result := TcStructLit{}
		result.Items = make([]TcExpr, len(exp.Fields))
		result.Source = arg.Source
		result.typ = exp

		if len(arg.Items) == 0 {
			for i := range result.Items {
				result.Items[i] = exp.Fields[i].Type.DefaultValue(tc, arg)
			}
			return result
		} else if _, _, isAssign0 := matchAssign(arg.Items[0]); isAssign0 {
			lastIdx := -1
			for _, it := range arg.Items {
				lhs, rhs, isAssign := matchAssign(it)
				if !isAssign {
					panic(isAssign)
				}
				lhsIdent := lhs.(Ident)
				field, idx := exp.GetField(lhsIdent.Source)
				if idx < 0 {
					tc.ReportErrorf(lhsIdent, "type %s has no field %s", exp.Name, lhsIdent.Source)
				} else {
					result.Items[idx] = tc.TypeCheckExpr(scope, rhs, field.Type)
					if idx < lastIdx {
						tc.ReportErrorf(lhsIdent, "out of order initialization is not allowed (yet?)")
					}
				}
				lastIdx = idx
			}
			// fill up all unset fields with default values
			for i := range result.Items {
				if result.Items[i] == nil {
					result.Items[i] = exp.Fields[i].Type.DefaultValue(tc, arg)
				}
			}
			return result
		} else {
			// must have all fields of struct
			if len(arg.Items) != len(exp.Fields) {
				tc.ReportErrorf(arg, "literal has %d values, but %s needs %d values", len(arg.Items), exp.Name, len(exp.Fields))
			}
			for i := range result.Items {
				result.Items[i] = tc.TypeCheckExpr(scope, arg.Items[i], exp.Fields[i].Type)
			}
			return result
		}
	default:
		panic(fmt.Errorf("I don't know about type %T!", exp))
	}
}

func (tc *TypeChecker) TypeCheckIfStmt(scope Scope, stmt IfExpr) (result TcIfStmt) {
	// currently only iteration on strings in possible (of course that is not final)
	result.Condition = tc.TypeCheckExpr(scope, stmt.Condition, TypeBoolean)
	result.Body = tc.TypeCheckExpr(scope, stmt.Body, TypeVoid)
	return
}

func (tc *TypeChecker) TypeCheckIfElseStmt(scope Scope, stmt IfElseExpr, expected Type) (result TcIfElseExpr) {
	// currently only iteration on strings in possible (of course that is not final)
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
	case *EnumSetType:
		return typ.Elem
	case *BuiltinType:
		if typ.name == "string" {
			return TypeChar
		}
	}
	tc.ReportErrorf(expr, "expect type with elements to iterate over")
	return TypeError
}

func (tc *TypeChecker) TypeCheckForLoopStmt(scope Scope, loopArg ForLoopStmt) (result TcForLoopStmt) {
	scope = scope.NewSubScope()
	// currently only iteration on strings in possible (of course that is not final)
	result.Collection = tc.TypeCheckExpr(scope, loopArg.Collection, TypeUnspecified)
	elementType := tc.ElementType(result.Collection)
	result.LoopSym = scope.NewSymbol(tc, loopArg.LoopIdent, SkLoopIterator, elementType)
	result.Body = tc.TypeCheckExpr(scope, loopArg.Body, TypeVoid)
	return
}

func (tc *TypeChecker) TypeCheckPackage(arg PackageDef) (result TcPackageDef) {
	scope := builtinScope.NewSubScope()
	result.Name = arg.Name

	hasDocComment := false
	var docComment DocComment

	for _, stmt := range arg.TopLevelStmts {
		if hasDocComment {
			stmt = tc.ApplyDocComment(stmt, docComment)
			hasDocComment = false
		}
		switch stmt := stmt.(type) {
		case TypeDef:
			switch td := tc.TypeCheckStructDef(scope, stmt).(type) {
			case *TcStructDef:
				result.StructDefs = append(result.StructDefs, td)
			case *TcEnumDef:
				result.EnumDefs = append(result.EnumDefs, td)
			}
		case ProcDef:
			procDef := tc.TypeCheckProcDef(scope, stmt)
			result.ProcDefs = append(result.ProcDefs, procDef)
			if procDef.Name == "main" {
				result.Main = procDef
			}
		case VariableDefStmt:
			varDef := tc.TypeCheckVariableDefStmt(scope, stmt)
			result.VarDefs = append(result.VarDefs, varDef)
		case DocComment:
			docComment = stmt
			hasDocComment = true
		}
	}
	return
}
