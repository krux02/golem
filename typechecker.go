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
	Parent *ScopeImpl
	// A return stmt needs to know which procedure it belongs to. This
	// pointer points to the corresponding procedure. This should
	// probably be redued to be just the proc signature.
	CurrentProc *TcProcDef
	Variables   map[string]TcSymbol
	Procedures  map[string][]ProcSignature
	Types       map[string]Type
}

type Scope = *ScopeImpl

func (scope Scope) NewSubScope() Scope {
	return &ScopeImpl{
		Parent:      scope,
		CurrentProc: scope.CurrentProc,
		Variables:   make(map[string]TcSymbol),
		Procedures:  make(map[string][]ProcSignature),
		Types:       make(map[string]Type),
	}
}

func (tc *TypeChecker) RegisterType(scope Scope, name string, typ Type, context AstNode) {
	if _, ok := scope.Types[name]; ok {
		tc.ReportErrorf(context, "double definition of type '%s'", name)
		// TODO previously defined at ...
		return
	}
	scope.Types[name] = typ
}

func (scope Scope) NewSymbol(tc *TypeChecker, name Ident, kind SymbolKind, typ Type) TcSymbol {
	//result := TcSymbol{Name: name.source, Kind: kind, Typ: typ}
	rawName := name.Source
	result := TcSymbol{Source: name.Source, Kind: kind, Type: typ}
	_, alreadyExists := scope.Variables[rawName]
	if alreadyExists {
		tc.ReportErrorf(name, "redefinition of %s", rawName)
	}
	scope.Variables[name.Source] = result
	return result
}

func (tc *TypeChecker) LookUpType(scope Scope, expr TypeExpr) Type {
	switch x := expr.(type) {
	case Call:
		ident, ok := x.Callee.(Ident)
		if !ok {
			tc.ReportErrorf(x.Callee, "identifier expected but got %T", x.Callee)
			return TypeError
		}
		switch ident.Source {
		case "array":
			if !tc.ExpectArgsLen(expr, len(x.Args), 2) {
				return TypeError
			}

			arg0 := tc.LookUpType(scope, x.Args[0])
			if arg0 == TypeError {
				// maybe fall back to unchecked array
				return TypeError
			}
			intLit, ok := arg0.(*IntLit)
			if !ok {
				tc.ReportErrorf(x.Args[0], "expect int literal but got %T", x.Args[0])
				return TypeError
			}
			elem := tc.LookUpType(scope, x.Args[1])
			return GetArrayType(elem, intLit.Value)
		case "set":
			if !tc.ExpectArgsLen(expr, len(x.Args), 1) {
				return TypeError
			}
			arg0 := tc.LookUpType(scope, x.Args[0])
			if arg0 == TypeError {
				return TypeError
			}
			elem, ok := arg0.(*EnumType)
			if !ok {
				tc.ReportErrorf(x.Args[0], "expect enum type but got %T", arg0)
				return TypeError
			}
			return GetEnumSetType(elem)
		case "ptr":
			if !tc.ExpectArgsLen(expr, len(x.Args), 1) {
				return TypeError
			}
			targetType := tc.LookUpType(scope, x.Args[0])
			if targetType == TypeError {
				return TypeError
			}
			return GetPtrType(targetType)
		default:
			// TODO implement this
			tc.ReportErrorf(ident, "expected 'array', 'set' or 'ptr' here, but got '%s'", ident.Source)
			return TypeError
		}
	case Ident:
		name := x.Source
		if typ, ok := scope.Types[name]; ok {
			return typ
		}
		if scope.Parent != nil {
			return tc.LookUpType(scope.Parent, expr)
		}
		tc.ReportErrorf(expr, "Type not found: %s", name)
		return TypeError
	case *IntLit:
		return x.Type
	}
	tc.ReportErrorf(expr, "unexpected ast node in type exper: %T", expr)
	return TypeError
}

func (tc *TypeChecker) LookUpProc(scope Scope, ident Ident, signatures []ProcSignature) []ProcSignature {
	for scope != nil {
		if localSignatures, ok := scope.Procedures[ident.Source]; ok {
			signatures = append(signatures, localSignatures...)
		}
		scope = scope.Parent
	}
	return signatures
}

func (tc *TypeChecker) LookUpLetSymRecursive(scope Scope, ident Ident) TcSymbol {
	if scope == nil {
		tc.ReportErrorf(ident, "let sym not found: %s", ident.Source)
		var result TcSymbol
		result.Kind = SkLet
		result.Source = ident.Source
		result.Type = TypeError
		return result
	}

	if sym, ok := scope.Variables[ident.Source]; ok {
		sym.Source = ident.Source
		return sym
	}
	return tc.LookUpLetSymRecursive(scope.Parent, ident)
}

func (tc *TypeChecker) LookUpLetSym(scope Scope, ident Ident, expected Type) (result TcSymbol) {
	if enumDef, ok := expected.(*EnumType); ok {
		for _, sym := range enumDef.Impl.Values {
			if sym.Source == ident.Source {
				// change line info
				sym.Source = ident.Source
				return sym
			}
		}
	}
	result = tc.LookUpLetSymRecursive(scope, ident)
	result.Type = tc.ExpectType(result, result.Type, expected)
	return
}

func (tc *TypeChecker) LineColumnNode(node AstNode) (line, columnStart, columnEnd int) {
	return LineColumnStr(tc.code, node.GetSource())
}

func (tc *TypeChecker) TypeCheckStructDef(scope Scope, def StructDef) *TcStructDef {
	result := &TcStructDef{}
	structType := &StructType{Impl: result}
	result.Source = def.Source
	result.Name = def.Name.Source
	result.Importc = def.Annotations.Value == "importc"

	// TODO: test when Importc that all fields are also Importc (or importc compatible, like builtin integer types)

	for _, colonExpr := range def.Fields {
		if nameIdent, ok := colonExpr.Lhs.(Ident); !ok {
			tc.ReportErrorf(colonExpr.Lhs, "expect Ident, but got %T", colonExpr.Lhs)
		} else {
			var tcField TcStructField
			tcField.Name = nameIdent.Source
			tcField.Type = tc.LookUpType(scope, colonExpr.Rhs)
			result.Fields = append(result.Fields, tcField)
		}
	}
	tc.RegisterType(scope, structType.Impl.Name, structType, def.Name)
	return result
}

func (tc *TypeChecker) TypeCheckEnumDef(scope Scope, def EnumDef) *TcEnumDef {
	result := &TcEnumDef{}
	enumType := &EnumType{Impl: result}
	result.Name = def.Name.Source
	if def.Annotations.Value != "" {
		if def.Annotations.Value == "importc" {
			result.Importc = true
		} else {
			tc.ReportInvalidAnnotations(def.Annotations)
		}

	}
	for _, ident := range def.Values {
		var sym TcSymbol
		sym.Source = ident.Source
		sym.Kind = SkEnum
		sym.Type = enumType
		result.Values = append(result.Values, sym)
	}
	tc.RegisterType(scope, enumType.Impl.Name, enumType, def.Name)
	registerBuiltin("string", fmt.Sprintf("%s_names_array[", result.Name), "", "]", []Type{enumType}, TypeStr)
	for _, intType := range TypeAnyInt.Items {
		builtinType := intType.(*BuiltinType)
		registerBuiltin(builtinType.Name, fmt.Sprintf("(%s)", builtinType.InternalName), "", "", []Type{enumType}, intType)
		registerBuiltin(result.Name, fmt.Sprintf("(%s)", result.Name), "", "", []Type{intType}, enumType)
	}
	registerBuiltin("contains", "(((", ") & (1 << (", "))) != 0)", []Type{GetEnumSetType(enumType), enumType}, TypeBoolean)
	return result
}

// tc.ReportErrorf(def.Kind, "invalid type kind %s, expect one of struct, enum, union", def.Kind.Source)
func (tc *TypeChecker) NewGenericParam(scope Scope, name Ident) Type {
	result := &GenericTypeSymbol{Source: name.Source}
	tc.RegisterType(scope, name.Source, result, name)
	return result
}

func (tc *TypeChecker) TypeCheckProcDef(parentScope Scope, def ProcDef) (result *TcProcDef) {
	procScope := parentScope.NewSubScope()
	result = &TcProcDef{}
	result.Signature.Impl = result
	procScope.CurrentProc = result
	result.Name = def.Name.Source

	if def.Annotations.Source != "" {
		if def.Annotations.Value == "importc" {
			result.Importc = true
		} else {
			tc.ReportInvalidAnnotations(def.Annotations)
		}
	}

	for _, arg := range def.Args {
		typ := tc.LookUpType(procScope, arg.Type)
		tcArg := procScope.NewSymbol(tc, arg.Name, SkProcArg, typ)
		result.Signature.Params = append(result.Signature.Params, tcArg)
	}
	if result.Importc || def.Name.Source == "main" {
		// TODO, don't special case `main` like this here
		result.MangledName = def.Name.Source
	} else {
		mangledNameBuilder := &strings.Builder{}
		mangledNameBuilder.WriteString(def.Name.Source)
		mangledNameBuilder.WriteRune('_')
		for _, arg := range result.Signature.Params {
			arg.Type.ManglePrint(mangledNameBuilder)
		}
		result.MangledName = mangledNameBuilder.String()
	}
	resultType := tc.LookUpType(procScope, def.ResultType)
	result.Signature.ResultType = resultType
	if result.Importc {
		if def.Body != nil {
			tc.ReportErrorf(def.Body, "proc is importc, it may not have a body")
		}
	} else {
		if def.Body == nil {
			tc.ReportErrorf(def, "proc def missas a body")
		} else {
			result.Body = tc.TypeCheckExpr(procScope, def.Body, resultType)
		}
	}
	parentScope.Procedures[result.Name] = append(parentScope.Procedures[result.Name], result.Signature)
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

func (tc *TypeChecker) ReportUnexpectedType(context AstNode, expected, gotten Type) {
	tc.ReportErrorf(context, "expected type '%s' but got type '%s'", AstFormat(expected), AstFormat(gotten))
}

func (tc *TypeChecker) ReportInvalidDocCommentKey(section NamedDocSection) {
	tc.ReportErrorf(section, "invalid doc comment key: %s", section.Name)
}

func (tc *TypeChecker) ReportInvalidAnnotations(lit StrLit) {
	tc.ReportErrorf(lit, "invalid annotations string: %s", lit.Value)
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
		for _, it := range expectedTg.Items {
			if gotten == it {
				return gotten
			}
		}
		tc.ReportUnexpectedType(node, expected, gotten)
		return TypeError
	}

	if gotten == TypeUnspecified {
		return expected
	}
	if gottenTg, ok := gotten.(*TypeGroup); ok {
		for _, it := range gottenTg.Items {
			if it == expected {
				return expected
			}
		}
	}

	ok, _ := ParamSignatureMatch(gotten, expected)
	if ok {
		return gotten
	}

	tc.ReportUnexpectedType(node, expected, gotten)
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

func (structDef *TcStructDef) GetField(name string) (resField TcStructField, idx int) {
	for i, field := range structDef.Fields {
		if field.Name == name {
			return field, i
		}
	}
	return TcStructField{Source: name, Name: name, Type: TypeError}, -1
}

func (tc *TypeChecker) TypeCheckDotExpr(scope Scope, parentSource string, lhs, rhs Expr, expected Type) (result TcDotExpr) {
	result.Lhs = tc.TypeCheckExpr(scope, lhs, TypeUnspecified)
	result.Source = parentSource

	// fmt.Printf("lhs: %T %+v\n", result.Lhs, result.Lhs)
	typ := result.Lhs.GetType()
	switch t := typ.(type) {
	case *StructType:
		rhsSrc := rhs.GetSource()
		var idx int
		result.Rhs, idx = t.Impl.GetField(rhsSrc)
		if idx < 0 {
			tc.ReportErrorf(rhs, "type %s has no field %s", t.Impl.Name, rhs.GetSource())
			return result
		}
		tc.ExpectType(rhs, result.Rhs.GetType(), expected)
		return result
	case *ErrorType:
		result.Rhs = TcErrorNode{rhs}
		return result
	default:
		tc.ReportErrorf(lhs, "dot call is only supported on struct types, but got: %s %T", AstFormat(typ), typ)
		result.Rhs = TcErrorNode{rhs}
		return result
	}
}

func errorProcSym(ident Ident) TcProcSymbol {
	return TcProcSymbol{
		Source: ident.GetSource(),
		Impl:   &TcErrorProcDef{}, // maybe add some debug information here
	}
}

type TypeGroupBuilder TypeGroup

func AppendNoDuplicats(types []Type, typ Type) (result []Type) {
	for _, it := range types {
		if it == typ {
			return types
		}
	}
	return append(types, typ)
}

// inversion of arguments, because go only has polymorphism on the self argument
func (typ *BuiltinType) AppendToGroup(builder *TypeGroupBuilder) bool {
	builder.Items = AppendNoDuplicats(builder.Items, typ)
	return false
}

func (typ *UnspecifiedType) AppendToGroup(builder *TypeGroupBuilder) bool {
	builder.Items = nil
	return true
}

func (typ *OpenGenericType) AppendToGroup(builder *TypeGroupBuilder) bool {
	builder.Items = nil
	return true
}

func (typ *TypeGroup) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	for _, it := range typ.Items {
		result = it.AppendToGroup(builder)
		// early return in case of TypeUnspecified
		if result {
			return result
		}
	}
	return result
}

func (typ *EnumType) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	builder.Items = AppendNoDuplicats(builder.Items, typ)
	return false
}

func (typ *StructType) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	builder.Items = AppendNoDuplicats(builder.Items, typ)
	return false
}

func (typ *ArrayType) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	builder.Items = AppendNoDuplicats(builder.Items, typ)
	return false
}

func (typ *EnumSetType) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	builder.Items = AppendNoDuplicats(builder.Items, typ)
	return false
}

func (typ *PtrType) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	builder.Items = AppendNoDuplicats(builder.Items, typ)
	return false
}

func (typ *ErrorType) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	// not sure if this is correct
	// either it should be appended, or all should just be replaced with the error type.
	builder.Items = AppendNoDuplicats(builder.Items, typ)
	return false
}

func (typ *GenericTypeSymbol) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	return typ.Constraint.AppendToGroup(builder)
}

func (typ *IntLit) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	// TODO implement
	panic("not implemented")
}

func (typ *TypeType) AppendToGroup(builder *TypeGroupBuilder) (result bool) {
	builder.Items = AppendNoDuplicats(builder.Items, typ)
	return false
}

func argTypeGroupAtIndex(signatures []ProcSignature, idx int) (result Type) {
	builder := &TypeGroupBuilder{}
	for _, sig := range signatures {
		if sig.Varargs && idx >= len(sig.Params) {
			return TypeUnspecified
		}
		typ := sig.Params[idx].Type
		if _, ok := typ.(*OpenGenericType); ok {
			// TODO this can be more precise than the most generic `TypeUnspecified`
			return TypeUnspecified
		}
		typ.AppendToGroup(builder)
	}
	switch len(builder.Items) {
	case 0:
		return TypeUnspecified
	case 1:
		return builder.Items[0]
	}
	return (*TypeGroup)(builder)
}

type Substitution = struct {
	sym     *GenericTypeSymbol
	newType Type
}

func GenericParamSignatureMatch(exprType, paramType Type, substitutions []Substitution) (ok bool, outSubstitutions []Substitution) {

	if typeSym, isTypeSym := paramType.(*GenericTypeSymbol); isTypeSym {
		// check if the type is somehow in the substitutions list
		for _, sub := range substitutions {
			if typeSym == sub.sym {
				if sub.sym == exprType {
					return true, substitutions
				} else {
					return false, nil
				}
			}
		}

		// not in the substitutions list, so append it
		return true, append(substitutions, Substitution{typeSym, exprType})
	}

	{
		exprArrType, exprIsArrType := exprType.(*ArrayType)
		paramArrType, paramIsArrType := paramType.(*ArrayType)
		if exprIsArrType && paramIsArrType {
			if exprArrType.Len != paramArrType.Len {
				return false, nil
			}
			return GenericParamSignatureMatch(exprArrType.Elem, paramArrType.Elem, substitutions)
		}
	}

	{
		exprPtrType, exprIsPtrType := exprType.(*PtrType)
		paramPtrType, paramIsPtrType := paramType.(*PtrType)
		if exprIsPtrType && paramIsPtrType {
			ok, substitutions = GenericParamSignatureMatch(exprPtrType.Target, paramPtrType.Target, substitutions)
			return ok, substitutions
		}
	}

	{
		exprEnumSetType, exprIsEnumSetType := exprType.(*EnumSetType)
		paramEnumSetType, paramIsEnumSetType := exprType.(*EnumSetType)
		if exprIsEnumSetType && paramIsEnumSetType {
			return GenericParamSignatureMatch(exprEnumSetType.Elem, paramEnumSetType.Elem, substitutions)
		}
	}

	// TODO apply type substitutions to struct type

	return false, nil
}

func ParamSignatureMatch(exprType, paramType Type) (ok bool, substitutions []Substitution) {
	if genericParamType, paramIsGeneric := paramType.(*OpenGenericType); paramIsGeneric {
		return GenericParamSignatureMatch(exprType, genericParamType.Type, nil)
	}

	// fast non recursive pass
	return exprType == paramType, []Substitution{}
}

func RecursiveTypeSubstitution(typ Type, substitutions []Substitution) Type {
	switch typ := typ.(type) {
	case *GenericTypeSymbol:
		for _, it := range substitutions {
			if it.sym == typ {
				return it.newType
			}
		}
		return typ
	case *BuiltinType:
		return typ
	case *EnumType:
		return typ
	case *IntLit:
		return typ
	case *StructType:
		return typ
	case *EnumSetType:
		return GetEnumSetType(RecursiveTypeSubstitution(typ.Elem, substitutions).(*EnumType))
	case *ArrayType:
		// TODO: array length substitution is not possible right now
		return GetArrayType(RecursiveTypeSubstitution(typ.Elem, substitutions), typ.Len)
	case *TypeType:
		return GetTypeType(RecursiveTypeSubstitution(typ.Type, substitutions))
	case *PtrType:
		return GetPtrType(RecursiveTypeSubstitution(typ.Target, substitutions))
	case *TypeGroup:
		panic("internal error")
	case *ErrorType:
		panic("internal error")
	case *OpenGenericType:
		panic("internal error")
	default:
		panic("internal error")
	}
}

func Contains(theSet []*GenericTypeSymbol, item *GenericTypeSymbol) bool {
	for _, it := range theSet {
		if it == item {
			return true
		}
	}
	return false
}

func Contains2(theSet []Substitution, item *GenericTypeSymbol) bool {
	for _, it := range theSet {
		if it.sym == item {
			return true
		}
	}
	return false
}

func ApplyTypeSubstitutions(argTyp Type, substitutions []Substitution) Type {
	typ, isOpenGeneric := argTyp.(*OpenGenericType)
	if !isOpenGeneric {
		// not a type with open generic symbols. nothing to substitute here
		return argTyp
	}

	// all substitutions that are actively part of the open symbols
	var filteredSubstitutions []Substitution
	for _, sub := range substitutions {
		if Contains(typ.OpenSymbols, sub.sym) {
			filteredSubstitutions = append(filteredSubstitutions, sub)
		}
	}

	if len(filteredSubstitutions) == 0 {
		// substitutions do not apply to this type, nothing to do here
		return typ
	}

	// compute new open symbols
	var openSymbols []*GenericTypeSymbol = nil
	for _, sym := range typ.OpenSymbols {
		if !Contains2(filteredSubstitutions, sym) {
			openSymbols = append(openSymbols, sym)
		}
	}
	newTyp := RecursiveTypeSubstitution(typ.Type, filteredSubstitutions)
	if len(openSymbols) == 0 {
		return newTyp
	}
	return &OpenGenericType{newTyp, openSymbols}

}

func SignatureApplyTypeSubstitution(sig ProcSignature, substitutions []Substitution) ProcSignature {
	if len(substitutions) == 0 {
		return sig
	}
	newParams := make([]TcSymbol, len(sig.Params))
	for j, param := range sig.Params {
		param.Type = ApplyTypeSubstitutions(param.Type, substitutions)
		newParams[j] = param
	}
	sig.Params = newParams
	sig.ResultType = ApplyTypeSubstitutions(sig.ResultType, substitutions)
	sig.Substitutions = append(sig.Substitutions, substitutions...)
	return sig
}

func (tc *TypeChecker) TypeCheckCall(scope Scope, call Call, expected Type) TcExpr {
	ident := call.Callee.(Ident)
	// language level reserved calls
	switch ident.Source {
	case ".":
		if !tc.ExpectArgsLen(call, len(call.Args), 2) {
			return TcErrorNode{SourceNode: call}
		}
		return tc.TypeCheckDotExpr(scope, call.Source, call.Args[0], call.Args[1], expected)
	case ":":
		if !tc.ExpectArgsLen(call, len(call.Args), 2) {
			return TcErrorNode{SourceNode: call}
		}
		typ := tc.LookUpType(scope, TypeExpr(call.Args[1]))
		tc.ExpectType(call, typ, expected)
		result := tc.TypeCheckExpr(scope, call.Args[0], typ)
		return result
	}
	signatures := tc.LookUpProc(scope, ident, nil)
	var checkedArgs []TcExpr
	hasArgTypeError := false
	for i, arg := range call.Args {
		// TODO reuse TypeGroupBuilder here
		expectedArgType := argTypeGroupAtIndex(signatures, i)
		tcArg := tc.TypeCheckExpr(scope, arg, expectedArgType)
		checkedArgs = append(checkedArgs, tcArg)
		argType := tcArg.GetType()
		if argType == TypeError {
			hasArgTypeError = true
			signatures = nil
			continue
		}
		// in place filter procedures for compatilbes
		n := 0
		for _, sig := range signatures {
			if sig.Varargs && i >= len(sig.Params) {
				signatures[n] = sig
				n++
				continue
			}
			typ := sig.Params[i].Type

			if ok, substitutions := ParamSignatureMatch(argType, typ); ok {
				// instantiate generic
				signatures[n] = SignatureApplyTypeSubstitution(sig, substitutions)
				n++
			}
		}
		signatures = signatures[:n]
	}

	result := TcCall{Source: call.Source}

	// if call.Callee.GetSource() == "addr" {
	// 	fmt.Printf("signatures: %s\n", call.Callee.GetSource())
	// 	for i, sig := range signatures {
	// 		fmt.Printf(" Signature: %v\n", i)
	// 		for j, arg := range sig.Params {
	// 			fmt.Printf("  arg %d: '%s'\n", j, AstFormat(arg.Type))
	// 		}
	// 		fmt.Printf("  result %v\n", AstFormat(sig.ResultType))
	// 	}
	// }

	switch len(signatures) {
	case 0:
		// don't report that proc `foo(TypeError)` can't be resolved. The error that
		// lead to `TypeError` is already reported at this point, so nothing new to
		// report here.
		if !hasArgTypeError {
			builder := &AstPrettyPrinter{}
			builder.WriteString("proc not found: ")
			builder.WriteString(ident.Source)
			builder.WriteRune('(')
			for i, arg := range checkedArgs {
				if i != 0 {
					builder.WriteString(", ")
				}
				arg.GetType().PrettyPrint(builder)
			}
			builder.WriteString(")")

			// original signatures
			signatures = tc.LookUpProc(scope, ident, nil)
			if len(signatures) > 0 {
				builder.NewlineAndIndent()
				builder.WriteString("available overloads: ")
				for _, sig := range signatures {
					builder.NewlineAndIndent()
					builder.WriteString("proc ")
					builder.WriteString(ident.Source)
					sig.PrettyPrint(builder)
				}
			}

			tc.ReportErrorf(ident, "%s", builder.String())
		}
		result.Sym = errorProcSym(ident)
		result.Args = checkedArgs

	case 1:
		sig := signatures[0]

		for _, arg := range sig.Params {
			if _, isGeneric := arg.Type.(*OpenGenericType); isGeneric {
				panic("internal error")
			}
		}
		if _, isGeneric := sig.ResultType.(*OpenGenericType); isGeneric {
			panic("internal error")
		}

		switch impl := sig.Impl.(type) {
		case *TcBuiltinProcDef, *TcProcDef:
			result.Sym = TcProcSymbol{Source: ident.Source, Impl: impl}
			result.Args = checkedArgs
			tc.ExpectType(call, sig.ResultType, expected)
		case *TcBuiltinGenericProcDef:
			// TODO: actually use sig.Substitutions to instantiate the generic proc def.
			// TODO: ensure that only one instance is generated per signature.
			// TODO: add back reference to original generic TcGenericProcDef
			//
			// currently replacing the signature alone is enough, but that is not a
			// general solution.
			implInstance := &TcBuiltinProcDef{
				Source:    impl.Source,
				Name:      impl.Name,
				Signature: sig,
				Prefix:    impl.Prefix,
				Infix:     impl.Infix,
				Postfix:   impl.Postfix,
			}
			result.Sym = TcProcSymbol{Source: ident.Source, Impl: implInstance}
			result.Args = checkedArgs
			tc.ExpectType(call, sig.ResultType, expected)
		case *TcTemplateDef:
			substitution := impl.Body
			tc.ExpectType(call, substitution.GetType(), expected)
			return substitution
		case *TcBuiltinMacroDef:
			result.Sym = TcProcSymbol{Source: ident.Source, Impl: impl}
			result.Args = checkedArgs
			result = impl.MacroFunc(tc, scope, result)
			tc.ExpectType(call, result.GetType(), expected)
		default:
			panic(fmt.Errorf("internal error: %T, call: %s", impl, AstFormat(call)))
		}
	default:
		tc.ReportErrorf(ident, "too many overloads: %s", ident.Source)
		result.Sym = errorProcSym(ident)
		result.Args = checkedArgs
	}

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
	case StructDef:
		expr2.Name.Comment = append(expr2.Name.Comment, doc.BaseDoc...)
	DOCSECTIONS2:
		for _, it := range doc.NamedDocSections {
			key := it.Name
			value := it.Lines
			for i, colonExpr := range expr2.Fields {
				if ident, ok := colonExpr.Lhs.(Ident); ok {
					if ident.Source == key {
						ident.Comment = append(ident.Comment, value...)
						colonExpr.Lhs = ident
						expr2.Fields[i] = colonExpr
						continue DOCSECTIONS2
					}
				}
			}
			tc.ReportInvalidDocCommentKey(it)
		}
		return expr2
	case EnumDef:
		expr2.Name.Comment = append(expr2.Name.Comment, doc.BaseDoc...)
	DOCSECTIONS3:
		for _, it := range doc.NamedDocSections {
			key := it.Name
			value := it.Lines
			for i, ident := range expr2.Values {
				if ident.Source == key {
					ident.Comment = append(ident.Comment, value...)
					expr2.Values[i] = ident
					continue DOCSECTIONS3
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

func (tc *TypeChecker) TypeCheckVariableDefStmt(scope Scope, arg VariableDefStmt) (result TcVariableDefStmt) {
	result.Source = arg.Source
	var expected Type = TypeUnspecified
	if arg.TypeExpr != nil {
		expected = tc.LookUpType(scope, arg.TypeExpr)
	}
	if arg.Value == nil {
		result.Value = expected.DefaultValue(tc, arg.Name)
		result.Sym = scope.NewSymbol(tc, arg.Name, arg.Kind, expected)
	} else {
		result.Value = tc.TypeCheckExpr(scope, arg.Value, expected)
		result.Sym = scope.NewSymbol(tc, arg.Name, arg.Kind, result.Value.GetType())
	}
	return result
}

func (tc *TypeChecker) TypeCheckReturnStmt(scope Scope, arg ReturnStmt) (result TcReturnStmt) {
	result.Value = tc.TypeCheckExpr(scope, arg.Value, scope.CurrentProc.Signature.ResultType)
	return
}

func (_ TcErrorNode) GetType() Type {
	return TypeError
}

func (block TcCodeBlock) GetType() Type {
	if len(block.Items) == 0 {
		return TypeVoid
	}
	return block.Items[len(block.Items)-1].GetType()
}

func (call TcCall) GetType() Type {
	return call.Sym.Impl.GetSignature().ResultType
}

func (lit StrLit) GetType() Type {
	return TypeStr
}

func (lit CStrLit) GetType() Type {
	return TypeCString
}

func (lit CharLit) GetType() Type {
	return TypeChar
}

func (lit *IntLit) GetType() Type {
	if lit.Type == nil {
		panic(fmt.Errorf("internal error: type of IntLit not set"))
	}
	return lit.Type
}

func (lit FloatLit) GetType() Type {
	if lit.Type == nil {
		panic(fmt.Errorf("internal error: type of FloatLit not set"))
	}
	return lit.Type
}

func (lit NilLit) GetType() Type {
	if lit.Type == nil {
		panic(fmt.Errorf("internal error: type of NullPtrLit not set"))
	}
	return lit.Type
}

func (lit TcArrayLit) GetType() Type {
	return GetArrayType(lit.ElemType, int64(len(lit.Items)))
}

func (lit TcEnumSetLit) GetType() Type {
	return GetEnumSetType(lit.ElemType)

}

func (lit TcStructLit) GetType() Type {
	return lit.Type
}

func (sym TcSymbol) GetType() Type {
	return sym.Type
}

func (stmt TcVariableDefStmt) GetType() Type {
	return TypeVoid
}

func (stmt TcForLoopStmt) GetType() Type {
	return TypeVoid
}

func (stmt TcWhileLoopStmt) GetType() Type {
	return TypeVoid
}

func (arg *TcBuiltinProcDef) GetType() Type {
	return TypeVoid
}

func (arg *TcBuiltinGenericProcDef) GetType() Type {
	return TypeVoid
}

func (arg *TcProcDef) GetType() Type {
	return TypeVoid
}

func (arg *TcTemplateDef) GetType() Type {
	return TypeVoid
}

func (arg *TcBuiltinMacroDef) GetType() Type {
	return TypeVoid
}

func (arg *TcErrorProcDef) GetType() Type {
	return TypeVoid // every proc/macro/template definition is of type void, even the one that doesn't exist
}

func UnifyType(a, b Type) Type {
	if a != b {
		panic("type incompatible")
	}
	return a
}

func (stmt TcIfStmt) GetType() Type {
	return TypeVoid
}

func (stmt TcIfElseExpr) GetType() Type {
	return UnifyType(stmt.Body.GetType(), stmt.Else.GetType())
}

func (returnStmt TcReturnStmt) GetType() Type {
	return TypeNoReturn
}

func (expr TcTypeContext) GetType() Type {
	return expr.Type
}

func (expr TcDotExpr) GetType() Type {
	return expr.Rhs.GetType()
}

func (field TcStructField) GetType() Type {
	return field.Type
}

func (tc *TypeChecker) TypeCheckColonExpr(scope Scope, arg ColonExpr, expected Type) TcExpr {
	typ := tc.LookUpType(scope, arg.Rhs)
	typ = tc.ExpectType(arg, typ, expected)
	return tc.TypeCheckExpr(scope, arg.Lhs, typ)
}

func (tc *TypeChecker) TypeCheckIntLit(scope Scope, arg *IntLit, expected Type) TcExpr {
	switch typ := tc.ExpectType(arg, TypeAnyNumber, expected).(type) {
	case *ErrorType:
		return &IntLit{
			Source: arg.Source,
			Type:   typ,
		}
	case *BuiltinType:
		if typ == TypeFloat32 || typ == TypeFloat64 {
			var lit FloatLit
			lit.Source = arg.Source
			lit.Value = float64(arg.Value)
			if typ == TypeFloat32 {
				lit.Type = TypeFloat32
				if int64(float32(lit.Value)) != arg.Value {
					tc.ReportErrorf(arg, "can't represent %d as float32 precisely", arg.Value)
				}
			} else if typ == TypeFloat64 {
				lit.Type = TypeFloat64
				if int64(lit.Value) != arg.Value {
					tc.ReportErrorf(arg, "can't represent %d as float64 precisely", arg.Value)
				}
			}
			return lit
		}
		return &IntLit{
			Source: arg.Source,
			Type:   typ,
			Value:  arg.Value,
		}
	default:
		panic(fmt.Errorf("internal error: %T", typ))
	}

}

func (tc *TypeChecker) TypeCheckStrLit(scope Scope, arg StrLit, expected Type) TcExpr {
	if expected == TypeCString {
		return CStrLit{arg.Source, arg.Value}
	}
	tc.ExpectType(arg, TypeStr, expected)
	return (TcExpr)(arg)
}

func (tc *TypeChecker) TypeCheckExpr(scope Scope, arg Expr, expected Type) TcExpr {
	switch arg := arg.(type) {
	case Call:
		// HACK: support for negative literals this should probably be done in a
		// different compiler pass that doesn't exist yet. Probably a prepass that
		// also is responsible to create ProcDef, StructDef, EnumDef
		if ident, kk := arg.Callee.(Ident); kk && ident.Source == "-" {
			if len(arg.Args) == 1 {
				if lit, ok := arg.Args[0].(*IntLit); ok {
					result := &IntLit{Source: arg.Source, Value: -lit.Value}
					result.Type = result
					return tc.TypeCheckIntLit(scope, result, expected)
				}
			}
		}
		return (TcExpr)(tc.TypeCheckCall(scope, arg, expected))
	case CodeBlock:
		return (TcExpr)(tc.TypeCheckCodeBlock(scope, arg, expected))
	case Ident:
		sym := tc.LookUpLetSym(scope, arg, expected)
		return (TcExpr)(sym)
	case StrLit:
		return tc.TypeCheckStrLit(scope, arg, expected)
	case CharLit:
		tc.ExpectType(arg, TypeChar, expected)
		return (TcExpr)(arg)
	case *IntLit:
		return (TcExpr)(tc.TypeCheckIntLit(scope, arg, expected))
	case FloatLit:
		typ := tc.ExpectType(arg, TypeAnyFloat, expected)
		arg.Type = typ.(*BuiltinType)
		return (TcExpr)(arg)
	case ReturnStmt:
		// ignoring expected type here, because the return as expression
		// never evaluates to anything
		return (TcExpr)(tc.TypeCheckReturnStmt(scope, arg))
	case TypeContext:
		var typ Type = GetTypeType(tc.LookUpType(scope, arg.Expr))
		var tcExpr TcTypeContext
		tcExpr.Source = arg.Source
		tcExpr.Type = typ
		return (TcExpr)(tcExpr)
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
	case NilLit:
		return (TcExpr)(tc.TypeCheckNilLit(scope, arg, expected))
	case ColonExpr:
		return (TcExpr)(tc.TypeCheckColonExpr(scope, arg, expected))
	case WhileLoopStmt:
		return (TcExpr)(tc.TypeCheckWhileLoopStmt(scope, arg))
	default:
		panic(fmt.Errorf("not implemented %T", arg))
	}
}

type ArrayTypeMapKey struct {
	elem Type
	len  int64
}

var arrayTypeMap map[ArrayTypeMapKey]*ArrayType

// is this safe, will this always look up a value? It is a pointer in a map
var enumSetTypeMap map[*EnumType]*EnumSetType
var ptrTypeMap map[Type]*PtrType
var typeTypeMap map[Type]*TypeType

func GetArrayType(elem Type, len int64) (result *ArrayType) {
	result, ok := arrayTypeMap[ArrayTypeMapKey{elem, len}]
	if !ok {
		result = &ArrayType{Elem: elem, Len: len}
		arrayTypeMap[ArrayTypeMapKey{elem, len}] = result
		registerBuiltin("[", "", ".arr[", "]", []Type{result, TypeInt64}, elem)
		registerSimpleTemplate("len", []Type{result}, TypeInt64, &IntLit{Source: "", Type: TypeInt64, Value: len})
	}
	return result
}

func GetEnumSetType(elem *EnumType) (result *EnumSetType) {
	result, ok := enumSetTypeMap[elem]
	if !ok {
		result = &EnumSetType{Elem: elem}
		enumSetTypeMap[elem] = result
	}
	return result
}

func GetPtrType(target Type) (result *PtrType) {
	result, ok := ptrTypeMap[target]
	if !ok {
		result = &PtrType{Target: target}
		ptrTypeMap[target] = result
	}
	return result
}

func GetTypeType(typ Type) (result *TypeType) {
	result, ok := typeTypeMap[typ]
	//result, ok := ptrTypeMap[typ]
	if !ok {
		result = &TypeType{Type: typ}
		typeTypeMap[typ] = result

	}
	return result
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
	case *StructType:
		result := TcStructLit{}
		result.Items = make([]TcExpr, len(exp.Impl.Fields))
		result.Source = arg.Source
		result.Type = exp

		if len(arg.Items) == 0 {
			for i := range result.Items {
				result.Items[i] = exp.Impl.Fields[i].Type.DefaultValue(tc, arg)
			}
			return result
		} else if _, _, isAssign0 := MatchAssign(arg.Items[0]); isAssign0 {
			lastIdx := -1
			for _, it := range arg.Items {
				lhs, rhs, isAssign := MatchAssign(it)
				if !isAssign {
					panic(isAssign)
				}
				lhsIdent := lhs.(Ident)
				field, idx := exp.Impl.GetField(lhsIdent.Source)
				if idx < 0 {
					tc.ReportErrorf(lhsIdent, "type %s has no field %s", exp.Impl.Name, lhsIdent.Source)
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
					result.Items[i] = exp.Impl.Fields[i].Type.DefaultValue(tc, arg)
				}
			}
			return result
		} else {
			// must have all fields of struct
			if len(arg.Items) != len(exp.Impl.Fields) {
				tc.ReportErrorf(arg, "literal has %d values, but %s needs %d values", len(arg.Items), exp.Impl.Name, len(exp.Impl.Fields))
			}
			for i := range result.Items {
				result.Items[i] = tc.TypeCheckExpr(scope, arg.Items[i], exp.Impl.Fields[i].Type)
			}
			return result
		}
	case *ErrorType:
		return TcErrorNode{SourceNode: arg}
	case *BuiltinType:
		panic(fmt.Errorf("I don't know about type %s!", exp.Name))
	default:
		panic(fmt.Errorf("I don't know about type %T!", exp))
	}
}

func (tc *TypeChecker) TypeCheckNilLit(scope Scope, arg NilLit, expected Type) NilLit {
	switch exp := expected.(type) {
	case *PtrType:
		return NilLit{Source: arg.Source, Type: exp}
	case *UnspecifiedType:
		return NilLit{Source: arg.Source, Type: TypeNilPtr}
	}
	tc.ReportUnexpectedType(arg, expected, TypeNilPtr)
	return NilLit{Source: arg.Source, Type: TypeError}
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
	switch typ := expr.GetType().(type) {
	case *ArrayType:
		return typ.Elem
	case *EnumSetType:
		return typ.Elem
	case *BuiltinType:
		if typ == TypeStr {
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

func (tc *TypeChecker) TypeCheckWhileLoopStmt(scope Scope, loopArg WhileLoopStmt) (result TcWhileLoopStmt) {
	scope = scope.NewSubScope()

	result.Source = loopArg.Source
	result.Condition = tc.TypeCheckExpr(scope, loopArg.Condition, TypeBoolean)
	result.Body = tc.TypeCheckExpr(scope, loopArg.Body, TypeVoid)
	return result
}

func (tc *TypeChecker) TypeCheckPackage(arg PackageDef, requiresMain bool) (result TcPackageDef) {
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

		case EnumDef:
			td := tc.TypeCheckEnumDef(scope, stmt)
			result.EnumDefs = append(result.EnumDefs, td)
		case StructDef:
			td := tc.TypeCheckStructDef(scope, stmt)
			result.StructDefs = append(result.StructDefs, td)
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
		case EmitStmt:
			result.EmitStatements = append(result.EmitStatements, stmt)
		default:
			panic(fmt.Errorf("internal error: %T", stmt))
		}
	}
	if requiresMain && result.Main == nil {
		tc.ReportErrorf(arg, "package %s misses main proc", result.Name)
	}
	return
}
