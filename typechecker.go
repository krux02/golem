package main

import (
	"fmt"
	"path/filepath"
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
	CurrentProgram *ProgramContext
	CurrentPackage *TcPackageDef
	CurrentProc    *TcProcDef
	Variables      map[string]TcSymbol
	Procedures     map[string][]*ProcSignature
	Types          map[string]Type
	Traits         map[string]*TcTraitDef
}

type Scope = *ScopeImpl

func NewSubScope(scope Scope) Scope {
	return &ScopeImpl{
		Parent:         scope,
		CurrentProgram: scope.CurrentProgram,
		CurrentPackage: scope.CurrentPackage,
		CurrentProc:    scope.CurrentProc,
		// TODO maybe do lazy initialization of some of these? Traits are rarely addad.
		Variables:  make(map[string]TcSymbol),
		Procedures: make(map[string][]*ProcSignature),
		Types:      make(map[string]Type),
		Traits:     make(map[string]*TcTraitDef),
	}
}

func RegisterType(tc *TypeChecker, scope Scope, name string, typ Type, context AstNode) {
	if _, ok := scope.Types[name]; ok {
		ReportErrorf(tc, context, "double definition of type '%s'", name)
		// TODO previously defined at ...
		return
	}
	scope.Types[name] = typ
}

func RegisterTrait(tc *TypeChecker, scope Scope, trait *TcTraitDef, context AstNode) {
	name := trait.Name
	if _, ok := scope.Traits[name]; ok {
		ReportErrorf(tc, context, "double definition of trait '%s'", trait.Name)
		// TODO previously defined at ...
		return
	}
	scope.Traits[name] = trait
}

func (scope Scope) NewSymbol(tc *TypeChecker, name Ident, kind SymbolKind, typ Type) TcSymbol {
	//result := TcSymbol{Name: name.source, Kind: kind, Typ: typ}
	rawName := name.Source
	result := TcSymbol{Source: rawName, Kind: kind, Value: nil, Type: typ}
	_, alreadyExists := scope.Variables[rawName]
	if alreadyExists {
		ReportErrorf(tc, name, "redefinition of %s", rawName)
	}
	scope.Variables[name.Source] = result
	return result
}

func (scope Scope) NewConstSymbol(tc *TypeChecker, name Ident, value TcExpr) TcSymbol {
	rawName := name.Source
	result := TcSymbol{Source: name.Source, Kind: SkConst, Value: value, Type: value.GetType()}
	_, alreadyExists := scope.Variables[rawName]
	if alreadyExists {
		ReportErrorf(tc, name, "redefinition of %s", rawName)
	}
	scope.Variables[name.Source] = result
	return result
}

func LookUpTrait(tc *TypeChecker, scope Scope, ident Ident) *TcTraitDef {
	name := ident.Source
	if typ, ok := scope.Traits[name]; ok {
		return typ
	}
	if scope.Parent != nil {
		return LookUpTrait(tc, scope.Parent, ident)
	}
	ReportErrorf(tc, ident, "Trait not found: %s", name)
	// TODO maybe return error trait to prevent nil error
	return nil
}

func LookUpType(tc *TypeChecker, scope Scope, expr TypeExpr) Type {
	switch x := expr.(type) {
	case VarExpr:
		typ := LookUpType(tc, scope, x.Expr)
		panic(fmt.Errorf("var expr not handled for: %s", AstFormat(typ)))
	case Call:
		ident, ok := x.Callee.(Ident)
		if !ok {
			ReportErrorf(tc, x.Callee, "identifier expected but got %T", x.Callee)
			return TypeError
		}
		switch ident.Source {
		case "array":
			if !ExpectArgsLen(tc, expr, len(x.Args), 2) {
				return TypeError
			}

			arg0 := LookUpType(tc, scope, x.Args[0])
			if arg0 == TypeError {
				// maybe fall back to unchecked array
				return TypeError
			}
			intLit, ok := arg0.(*IntLitType)
			if !ok {
				ReportErrorf(tc, x.Args[0], "expect int literal but got %T", x.Args[0])
				return TypeError
			}
			elem := LookUpType(tc, scope, x.Args[1])
			return GetArrayType(elem, intLit.Value)
		case "set":
			if !ExpectArgsLen(tc, expr, len(x.Args), 1) {
				return TypeError
			}
			arg0 := LookUpType(tc, scope, x.Args[0])
			if arg0 == TypeError {
				return TypeError
			}
			elem, ok := arg0.(*EnumType)
			if !ok {
				ReportErrorf(tc, x.Args[0], "expect enum type but got %T", arg0)
				return TypeError
			}
			return GetEnumSetType(elem)
		case "ptr":
			if !ExpectArgsLen(tc, expr, len(x.Args), 1) {
				return TypeError
			}
			targetType := LookUpType(tc, scope, x.Args[0])
			if targetType == TypeError {
				return TypeError
			}
			return GetPtrType(targetType)
		default:
			// TODO implement this
			ReportErrorf(tc, ident, "expected 'array', 'set' or 'ptr' here, but got '%s'", ident.Source)
			return TypeError
		}
	case Ident:
		name := x.Source
		if typ, ok := scope.Types[name]; ok {
			return typ
		}
		if scope.Parent != nil {
			return LookUpType(tc, scope.Parent, expr)
		}
		ReportErrorf(tc, expr, "Type not found: %s", name)
		return TypeError
	case IntLit:
		return GetIntLitType(x.Value)
		// case nil:
		// 	return TypeError
	}
	panic(fmt.Sprintf("unexpected ast node in type expr: %s type: %T", AstFormat(expr), expr))
}

func LookUpProc(scope Scope, ident Ident, numArgs int, signatures []*ProcSignature) []*ProcSignature {
	for scope != nil {
		if localSignatures, ok := scope.Procedures[ident.Source]; ok {
			if numArgs >= 0 { // num args may be set to -1 to get all signatures
				for _, sig := range localSignatures {
					// TODO, the varargs logic herer needs a proper test
					if len(sig.Params) == numArgs || sig.Varargs && len(sig.Params) <= numArgs {
						signatures = append(signatures, sig)
					}
				}
			} else {
				signatures = append(signatures, localSignatures...)
			}

		}
		scope = scope.Parent
	}
	return signatures
}

func LookUpLetSymRecursive(tc *TypeChecker, scope Scope, ident Ident) TcSymbol {
	if scope == nil {
		ReportErrorf(tc, ident, "let sym not found: %s", ident.Source)
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
	return LookUpLetSymRecursive(tc, scope.Parent, ident)
}

func LookUpLetSym(tc *TypeChecker, scope Scope, ident Ident, expected Type) (result TcSymbol) {

	if enumDef, ok := expected.(*EnumType); ok {
		for _, sym := range enumDef.Impl.Values {
			if sym.Source == ident.Source {
				// change line info
				sym.Source = ident.Source
				return sym
			}
		}
	}
	result = LookUpLetSymRecursive(tc, scope, ident)
	result.Type = ExpectType(tc, result, result.Type, expected)
	return
}

func TypeCheckStructDef(tc *TypeChecker, scope Scope, def StructDef) *TcStructDef {
	ValidNameCheck(tc, def.Name, "type")
	result := &TcStructDef{}
	structType := &StructType{Impl: result}
	result.Source = def.Source
	result.Name = def.Name.Source
	result.Importc = def.Annotations.Value == "importc"

	// TODO: test when Importc that all fields are also Importc (or importc compatible, like builtin integer types)

	for _, colonExpr := range def.Fields {
		if nameIdent, ok := colonExpr.Lhs.(Ident); !ok {
			ReportErrorf(tc, colonExpr.Lhs, "expect Ident, but got %T", colonExpr.Lhs)
		} else {
			ValidNameCheck(tc, nameIdent, "struct field")
			var tcField TcStructField
			tcField.Name = nameIdent.Source
			tcField.Type = LookUpType(tc, scope, colonExpr.Rhs)
			result.Fields = append(result.Fields, tcField)
		}
	}
	RegisterType(tc, scope, structType.Impl.Name, structType, def.Name)
	return result
}

func TypeCheckTraitDef(tc *TypeChecker, scope Scope, def *TraitDef) *TcTraitDef {
	ValidNameCheck(tc, def.Name, "trait")
	result := &TcTraitDef{}
	result.Source = def.Source
	result.Name = def.Name.Source
	result.Signatures = def.Signatures
	RegisterTrait(tc, scope, result, def.Name)
	return result
}

func TypeCheckEnumDef(tc *TypeChecker, scope Scope, def EnumDef) *TcEnumDef {
	ValidNameCheck(tc, def.Name, "type")
	result := &TcEnumDef{}
	enumType := &EnumType{Impl: result}
	result.Name = def.Name.Source
	if def.Annotations.Value != "" {
		if def.Annotations.Value == "importc" {
			result.Importc = true
		} else {
			ReportInvalidAnnotations(tc, def.Annotations)
		}
	}
	for _, ident := range def.Values {
		ValidNameCheck(tc, ident, "enum value")
		var sym TcSymbol
		sym.Source = ident.Source
		sym.Kind = SkEnum
		sym.Type = enumType
		result.Values = append(result.Values, sym)
	}
	RegisterType(tc, scope, enumType.Impl.Name, enumType, def.Name)
	registerBuiltin("string", fmt.Sprintf("%s_names_array[", result.Name), "", "]", []Type{enumType}, TypeStr, false)
	for _, intType := range TypeAnyInt.Items {
		builtinType := intType.(*BuiltinIntType)
		registerBuiltin(builtinType.Name, fmt.Sprintf("(%s)", builtinType.InternalName), "", "", []Type{enumType}, intType, false)
		registerBuiltin(result.Name, fmt.Sprintf("(%s)", result.Name), "", "", []Type{intType}, enumType, false)
	}
	registerBuiltin("contains", "(((", ") & (1 << (", "))) != 0)", []Type{GetEnumSetType(enumType), enumType}, TypeBoolean, false)
	return result
}

func TypeCheckProcDef(tc *TypeChecker, parentScope Scope, def ProcDef) (result *TcProcDef) {
	ValidNameCheck(tc, def.Name, "proc")
	procScope := NewSubScope(parentScope)

	result = &TcProcDef{}
	result.Signature = &ProcSignature{}
	result.Signature.Impl = result
	procScope.CurrentProc = result
	result.Name = def.Name.Source

	if def.Annotations.Source != "" {
		if def.Annotations.Value == "importc" {
			result.Importc = true
		} else {
			ReportInvalidAnnotations(tc, def.Annotations)
		}
	}

	for _, genericArg := range def.GenericArgs {
		// result.Signature = result.Signature.GenericParams
		constraint := LookUpTrait(tc, procScope, genericArg.TraitName)
		if constraint == nil {
			continue
		}
		name := genericArg.Name.Source
		ReportWarningf(tc, genericArg.TraitName, "generic constraints/traits are currently not yet implemented and ignored from the compiler")
		// TODO actually make trait usable as a type constraint, currently the trait/constraint is set to nil
		genTypeSym := &GenericTypeSymbol{Source: name, Name: name, Constraint: nil}
		result.Signature.GenericParams = append(result.Signature.GenericParams, genTypeSym)
		// make the generic type symbol available to look up in its body
		RegisterType(tc, procScope, name, genTypeSym, def)
	}

	for _, arg := range def.Args {
		ValidNameCheck(tc, arg.Name, "proc arg")
		typ := LookUpType(tc, procScope, arg.Type)
		symKind := SkProcArg
		if arg.Mutable {
			symKind = SkVarProcArg
		}
		tcArg := procScope.NewSymbol(tc, arg.Name, symKind, typ)
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
	var resultType Type
	if def.ResultType == nil {
		ReportErrorf(tc, def, "proc def needs result type specified")
		resultType = TypeError
	} else {
		resultType = LookUpType(tc, procScope, def.ResultType)
	}

	result.Signature.ResultType = resultType
	if result.Importc {
		if def.Body != nil {
			ReportErrorf(tc, def.Body, "proc is importc, it may not have a body")
		}
	} else {
		if def.Body == nil {
			ReportErrorf(tc, def, "proc def missas a body")
		} else {
			result.Body = TypeCheckExpr(tc, procScope, def.Body, resultType)
		}
	}
	parentScope.Procedures[result.Name] = append(parentScope.Procedures[result.Name], result.Signature)
	return result
}

func ReportMessagef(tc *TypeChecker, node AstNode, kind string, msg string, args ...interface{}) {
	newMsg := fmt.Sprintf(msg, args...)
	tc.errors = append(tc.errors, CompileError{node: node, msg: newMsg})
	if !tc.silentErrors {
		if node == nil {
			fmt.Println(msg)
		} else {
			line, columnStart, columnEnd := LineColumnStr(tc.code, node.GetSource())
			fmt.Printf("%s(%d, %d-%d) %s: %s\n", tc.filename, line, columnStart, columnEnd, kind, newMsg)
		}
	}
}

func ReportErrorf(tc *TypeChecker, node AstNode, msg string, args ...interface{}) {
	ReportMessagef(tc, node, "Error", msg, args...)
}

func ReportWarningf(tc *TypeChecker, node AstNode, msg string, args ...interface{}) {
	ReportMessagef(tc, node, "Warning", msg, args...)
}

func ReportUnexpectedType(tc *TypeChecker, context AstNode, expected, gotten Type) {
	ReportErrorf(tc, context, "expected type '%s' but got type '%s'", AstFormat(expected), AstFormat(gotten))
}

func ReportInvalidDocCommentKey(tc *TypeChecker, section NamedDocSection) {
	ReportErrorf(tc, section, "invalid doc comment key: %s", section.Name)
}

func ReportInvalidAnnotations(tc *TypeChecker, lit StrLit) {
	ReportErrorf(tc, lit, "invalid annotations string: %s", lit.Value)
}

func ReportIllegalDocComment(tc *TypeChecker, doc DocComment) {
	ReportErrorf(tc, doc, "doc comment is illegal here, use normal comment instead")
}

func ExpectType(tc *TypeChecker, node AstNode, gotten, expected Type) Type {
	// TODO this doesn't work for partial types (e.g. array[<unspecified>])
	// TODO this should have some cleanup, it has redundant code and seems to be error prone
	if expected == TypeError {
		return TypeError
	}
	if gotten == TypeError {
		return TypeError
	}
	if theIntLitType, gotIntLitType := gotten.(*IntLitType); gotIntLitType {
		if theBuiltinType, expectBuiltinType := expected.(*BuiltinIntType); expectBuiltinType {
			if theBuiltinType.MinValue <= theIntLitType.Value &&
				(theIntLitType.Value < 0 || uint64(theIntLitType.Value) <= theBuiltinType.MaxValue) {
				return theBuiltinType
			}
			ReportErrorf(tc, node, "integer literal %d out of range for type '%s' [%d..%d]",
				theIntLitType.Value, theBuiltinType.Name, theBuiltinType.MinValue, theBuiltinType.MaxValue)
			return TypeError
		}
		if floatType, isFloatType := expected.(*BuiltinFloatType); isFloatType {
			return floatType
		}
	}

	if expected == TypeUnspecified {
		if gotten == TypeUnspecified {
			ReportErrorf(tc, node, "expression type is unspecified")
			return TypeError
		}
		if gottenTg, ok := gotten.(*TypeGroup); ok {
			// TODO when is this branch active? it was used for AnyInt etc in type check int lit
			ReportErrorf(tc, node, "narrowing of type '%s' is required", AstFormat(gottenTg))
			return TypeError
		}
		return gotten
	}
	if expectedTg, ok := expected.(*TypeGroup); ok {
		if gotten == TypeUnspecified {
			ReportErrorf(tc, node, "narrowing of type '%s' is required", AstFormat(expectedTg))
			return TypeError
		}
		if gottenTg, ok := gotten.(*TypeGroup); ok {
			ReportErrorf(tc, node, "internal error: narrowing of type group '%s' and '%s' is not implemented yet", AstFormat(gottenTg), AstFormat(expectedTg))
			return TypeError
		}
		for _, it := range expectedTg.Items {
			if gotten == it {
				return gotten
			}
		}
		ReportUnexpectedType(tc, node, expected, gotten)
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

	ReportUnexpectedType(tc, node, expected, gotten)
	return TypeError
}

func ExpectArgsLen(tc *TypeChecker, node AstNode, gotten, expected int) bool {
	if expected != gotten {
		ReportErrorf(tc, node, "expected %d arguments, but got %d", expected, gotten)
		return false
	}
	return true
}

func ExpectMinArgsLen(tc *TypeChecker, node AstNode, gotten, expected int) bool {
	if gotten < expected {
		ReportErrorf(tc, node, "Expected at least %d arguments, but got %d.", expected, gotten)
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

func TypeCheckDotExpr(tc *TypeChecker, scope Scope, call Call, expected Type) TcExpr {
	if !ExpectArgsLen(tc, call, len(call.Args), 2) {
		return newErrorNode(call)
	}

	lhs := TypeCheckExpr(tc, scope, call.Args[0], TypeUnspecified)

	result := TcDotExpr{
		Source: call.Source,
		Lhs:    lhs,
	}
	// fmt.Printf("lhs: %T %+v\n", result.Lhs, result.Lhs)
	typ := result.Lhs.GetType()
	switch t := typ.(type) {
	case *StructType:
		rhs, isIdent := call.Args[1].(Ident)
		if !isIdent {
			ReportErrorf(tc, call.Args[1], "right of dot operator needs to be an identifier, but it is %T", call.Args[1])
			return result
		}
		var idx int
		result.Rhs, idx = t.Impl.GetField(rhs.Source)
		if idx < 0 {
			ReportErrorf(tc, rhs, "type %s has no field %s", t.Impl.Name, rhs.GetSource())
			return newErrorNode(call)
		}
		ExpectType(tc, rhs, result.Rhs.GetType(), expected)
		return result
	case *ErrorType:
		return newErrorNode(call)
	default:
		ReportErrorf(tc, lhs, "dot call is only supported on struct types, but got: %s", AstFormat(typ))
		return newErrorNode(call)
	}
}

func errorProcSym(ident Ident) TcProcSymbol {
	Impl := &TcErrorProcDef{Name: ident.Source, Signature: &ProcSignature{ResultType: TypeError}}
	Impl.Signature.Impl = Impl

	return TcProcSymbol{
		Source: ident.GetSource(),
		// maybe add some debug information here
		Impl: Impl,
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

func AppendToGroup(builder *TypeGroupBuilder, typ Type) (result bool) {
	switch typ := typ.(type) {
	case *TypeGroup:
		for _, it := range typ.Items {
			result = AppendToGroup(builder, it)
			// early return in case of TypeUnspecified
			if result {
				return result
			}
		}
		return result
	case *UnspecifiedType:
		builder.Items = nil
		return true
	case *GenericTypeSymbol:
		return AppendToGroup(builder, typ.Constraint)
	case *OpenGenericType:
		builder.Items = nil
		return true
	}
	// TODO not sure if *ErrorType should be special cased or not.
	//default
	builder.Items = AppendNoDuplicats(builder.Items, typ)
	return false
}

func (typ *UnspecifiedType) AppendToGroup(builder *TypeGroupBuilder) bool {
	builder.Items = nil
	return true
}

func argTypeGroupAtIndex(signatures []*ProcSignature, idx int) (result Type) {
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
		AppendToGroup(builder, typ)
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
	case *IntLitType:
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

func SignatureApplyTypeSubstitution(sig *ProcSignature, substitutions []Substitution) *ProcSignature {
	if len(substitutions) == 0 {
		return sig
	}
	newParams := make([]TcSymbol, len(sig.Params))
	for j, param := range sig.Params {
		param.Type = ApplyTypeSubstitutions(param.Type, substitutions)
		newParams[j] = param
	}

	result := &ProcSignature{}
	result.Params = newParams
	result.ResultType = ApplyTypeSubstitutions(sig.ResultType, substitutions)
	result.Substitutions = append(sig.Substitutions, substitutions...)
	result.Varargs = sig.Varargs
	result.Impl = sig.Impl
	return result
}

func (tc *TypeChecker) TypeCheckCall(scope Scope, call Call, expected Type) TcExpr {
	ident, isIdent := call.Callee.(Ident)
	if !isIdent {
		ReportErrorf(tc, call.Callee, "expected identifier but got %T (%s)", call.Callee, AstFormat(call.Callee))
		return newErrorNode(call.Callee)
	}
	// language level reserved calls
	switch ident.Source {
	case ".":
		return TypeCheckDotExpr(tc, scope, call, expected)
	case ":":
		if !ExpectArgsLen(tc, call, len(call.Args), 2) {
			return newErrorNode(call)
		}
		typ := LookUpType(tc, scope, TypeExpr(call.Args[1]))
		ExpectType(tc, call, typ, expected)
		result := TypeCheckExpr(tc, scope, call.Args[0], typ)
		return result
	}
	signatures := LookUpProc(scope, ident, len(call.Args), nil)
	var checkedArgs []TcExpr
	hasArgTypeError := false
	for i, arg := range call.Args {
		// TODO reuse TypeGroupBuilder here
		expectedArgType := argTypeGroupAtIndex(signatures, i)
		tcArg := TypeCheckExpr(tc, scope, arg, expectedArgType)
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
			signatures = LookUpProc(scope, ident, -1, nil)
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

			ReportErrorf(tc, ident, "%s", builder.String())
		}
		result.Sym = errorProcSym(ident)
		result.Args = checkedArgs

	case 1:
		sig := signatures[0]

		for i, arg := range sig.Params {
			if _, isGeneric := arg.Type.(*OpenGenericType); isGeneric {
				panic("internal error")
			}

			if arg.Kind == SkVarProcArg && !checkedArgs[i].GetMutable() {
				ReportErrorf(tc, checkedArgs[i], "argument must be mutable")
			}
		}
		if _, isGeneric := sig.ResultType.(*OpenGenericType); isGeneric {
			panic("internal error")
		}

		switch impl := sig.Impl.(type) {
		case *TcBuiltinProcDef, *TcProcDef:
			result.Sym = TcProcSymbol{Source: ident.Source, Impl: impl}
			result.Args = checkedArgs
			ExpectType(tc, call, sig.ResultType, expected)
		case *TcBuiltinGenericProcDef:
			// TODO: actually use sig.Substitutions to instantiate the generic proc def.
			// TODO: ensure that only one instance is generated per signature.
			// TODO: add back reference to original generic TcGenericProcDef
			//
			// currently replacing the signature alone is enough, but that is not a
			// general solution.
			implInstance := &TcBuiltinProcDef{
				Name:      impl.Name,
				Signature: sig,
				Prefix:    impl.Prefix,
				Infix:     impl.Infix,
				Postfix:   impl.Postfix,
			}
			result.Sym = TcProcSymbol{Source: ident.Source, Impl: implInstance}
			result.Args = checkedArgs
			ExpectType(tc, call, sig.ResultType, expected)
		case *TcTemplateDef:
			substitution := impl.Body
			ExpectType(tc, call, substitution.GetType(), expected)
			return substitution
		case *TcBuiltinMacroDef:
			result.Sym = TcProcSymbol{Source: ident.Source, Impl: impl}
			result.Args = checkedArgs
			newResult := impl.MacroFunc(tc, scope, result)
			ExpectType(tc, call, newResult.GetType(), expected)
			return newResult
		default:
			panic(fmt.Errorf("internal error: %T, call: %s", impl, AstFormat(call)))
		}
	default:
		ReportErrorf(tc, ident, "too many overloads: %s", ident.Source)
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
				ReportInvalidDocCommentKey(tc, it)
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
			ReportInvalidDocCommentKey(tc, it)
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
			ReportInvalidDocCommentKey(tc, it)
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
			ReportInvalidDocCommentKey(tc, it)
		}
		return expr2
	default:
		fmt.Printf("typ: %T\n", expr2)
		ReportIllegalDocComment(tc, doc)
		return expr2
	}
}
func (tc *TypeChecker) TypeCheckCodeBlock(scope Scope, arg CodeBlock, expected Type) (result TcCodeBlock) {
	result.Source = arg.Source
	scope = NewSubScope(scope)
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
					resultItems = append(resultItems, TypeCheckExpr(tc, scope, item, expected))
				} else {
					resultItems = append(resultItems, TypeCheckExpr(tc, scope, item, TypeVoid))
				}
			}
		}
		result.Items = resultItems
	} else {
		// empty block is type void
		ExpectType(tc, arg, TypeVoid, expected)
	}
	return
}

func IsValidIdentifier(name string) string {
	// double underscore __ is used in name mangling to concatenate identifires,
	// e.g. package and function name. To make this process reversible, the
	// identifier itself may not use start/end in _ or use __ within.
	//
	// This technical detail gives me the excuse to forbid the technique of
	// starting/ending identifiers with _ globally in the language. I think it is
	// extremely ugly and should never be done for anything.

	if len(name) == 0 {
		return "identifier may not be empty"
	}

	if strings.HasPrefix(name, "_") || strings.HasSuffix(name, "_") {
		return "identifier may not start or end with _ (underscore), reserved for internal usage"
	}
	if strings.Contains(name, "__") {
		return "identifier may not use __ (double underscore), reserved for internal usage"
	}
	return ""
}

func ValidNameCheck(tc *TypeChecker, ident Ident, extraword string) {
	if errMsg := IsValidIdentifier(ident.Source); errMsg != "" {
		ReportErrorf(tc, ident, "%s %s", extraword, errMsg)
	}
}

func TypeCheckVariableDefStmt(tc *TypeChecker, scope Scope, arg VariableDefStmt) (result TcVariableDefStmt) {
	ValidNameCheck(tc, arg.Name, "var")
	result.Source = arg.Source
	var expected Type = TypeUnspecified
	if arg.TypeExpr != nil {
		expected = LookUpType(tc, scope, arg.TypeExpr)
	}
	if arg.Value == nil {
		if arg.Kind != SkVar {
			ReportErrorf(tc, arg, "initial value required")
		}
		result.Value = expected.DefaultValue(tc, arg.Name)
		result.Sym = scope.NewSymbol(tc, arg.Name, arg.Kind, expected)
	} else {
		result.Value = TypeCheckExpr(tc, scope, arg.Value, expected)
		if arg.Kind == SkConst {
			// TODO: this needs proper checking if it can even computed
			result.Sym = scope.NewConstSymbol(tc, arg.Name, EvalExpr(tc, result.Value, scope))
		} else {
			result.Sym = scope.NewSymbol(tc, arg.Name, arg.Kind, result.Value.GetType())
		}
	}
	return result
}

func TypeCheckReturnExpr(tc *TypeChecker, scope Scope, arg ReturnExpr) (result TcReturnExpr) {
	result.Value = TypeCheckExpr(tc, scope, arg.Value, scope.CurrentProc.Signature.ResultType)
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

func (lit IntLit) GetType() Type {
	if lit.Type == nil {
		panic(fmt.Errorf("internal error: type of IntLit not set"))
	}
	return lit.Type
}

func (lit FloatLit) GetType() Type {
	if lit.Type == nil {
		panic(fmt.Errorf("internal error: type of FloatLit %s not set", lit.Source))
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

func (arg *TcPackageDef) GetType() Type {
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

func (returnExpr TcReturnExpr) GetType() Type {
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

func TypeCheckColonExpr(tc *TypeChecker, scope Scope, arg ColonExpr, expected Type) TcExpr {
	typ := LookUpType(tc, scope, arg.Rhs)
	typ = ExpectType(tc, arg, typ, expected)
	return TypeCheckExpr(tc, scope, arg.Lhs, typ)
}

func TypeCheckIntLit(tc *TypeChecker, scope Scope, arg IntLit, expected Type) TcExpr {
	switch typ := ExpectType(tc, arg, arg.Type, expected).(type) {
	case *ErrorType, *IntLitType, *BuiltinIntType:
		result := IntLit{
			Source: arg.Source,
			Type:   typ,
			Value:  arg.Value,
		}
		return result
	case *BuiltinFloatType:
		var lit FloatLit
		lit.Source = arg.Source
		lit.Value = float64(arg.Value)
		if typ == TypeFloat32 {
			lit.Type = TypeFloat32
			if int64(float32(lit.Value)) != arg.Value {
				ReportErrorf(tc, arg, "can't represent %d as float32 precisely", arg.Value)
			}
		} else if typ == TypeFloat64 {
			lit.Type = TypeFloat64
			if int64(lit.Value) != arg.Value {
				ReportErrorf(tc, arg, "can't represent %d as float64 precisely", arg.Value)
			}
		}
		return lit
	default:
		panic(fmt.Errorf("internal error: %T", typ))
	}
}

func (tc *TypeChecker) TypeCheckStrLit(scope Scope, arg StrLit, expected Type) TcExpr {
	if expected == TypeCString {
		return CStrLit{arg.Source, arg.Value}
	}
	ExpectType(tc, arg, TypeStr, expected)
	return (TcExpr)(arg)
}

func TypeCheckExpr(tc *TypeChecker, scope Scope, arg Expr, expected Type) TcExpr {
	switch arg := arg.(type) {
	case Call:
		return (TcExpr)(tc.TypeCheckCall(scope, arg, expected))
	case CodeBlock:
		return (TcExpr)(tc.TypeCheckCodeBlock(scope, arg, expected))
	case Ident:
		sym := LookUpLetSym(tc, scope, arg, expected)
		return (TcExpr)(sym)
	case StrLit:
		return tc.TypeCheckStrLit(scope, arg, expected)
	case CharLit:
		ExpectType(tc, arg, TypeChar, expected)
		return (TcExpr)(arg)
	case IntLit:
		return (TcExpr)(TypeCheckIntLit(tc, scope, arg, expected))
	case FloatLit:
		arg.Type = ExpectType(tc, arg, TypeAnyFloat, expected)
		return (TcExpr)(arg)
	case ReturnExpr:
		// ignoring expected type here, because the return as expression
		// never evaluates to anything
		return (TcExpr)(TypeCheckReturnExpr(tc, scope, arg))
	case TypeContext:
		var typ Type = GetTypeType(LookUpType(tc, scope, arg.Expr))
		var tcExpr TcTypeContext
		tcExpr.Source = arg.Source
		tcExpr.Type = typ
		return (TcExpr)(tcExpr)
	case VariableDefStmt:
		ExpectType(tc, arg, TypeVoid, expected)
		return (TcExpr)(TypeCheckVariableDefStmt(tc, scope, arg))
	case ForLoopStmt:
		ExpectType(tc, arg, TypeVoid, expected)
		return (TcExpr)(TypeCheckForLoopStmt(tc, scope, arg))
	case IfExpr:
		ExpectType(tc, arg, TypeVoid, expected)
		return (TcExpr)(TypeCheckIfStmt(tc, scope, arg))
	case IfElseExpr:
		return (TcExpr)(TypeCheckIfElseStmt(tc, scope, arg, expected))
	case ArrayLit:
		return (TcExpr)(TypeCheckArrayLit(tc, scope, arg, expected))
	case NilLit:
		return (TcExpr)(TypeCheckNilLit(tc, scope, arg, expected))
	case ColonExpr:
		return (TcExpr)(TypeCheckColonExpr(tc, scope, arg, expected))
	case WhileLoopStmt:
		return (TcExpr)(TypeCheckWhileLoopStmt(tc, scope, arg))
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
var packageMap map[string]*TcPackageDef
var intLitTypeMap map[int64]*IntLitType

func GetPackage(currentProgram *ProgramContext, workDir string, importPath string) (result *TcPackageDef, err error) {
	if !filepath.IsAbs(workDir) {
		panic(fmt.Errorf("internal error: workDir '%s' must be absolute", workDir))
	}
	fullpath := filepath.Clean(filepath.Join(workDir, importPath))
	var ok bool
	result, ok = packageMap[fullpath]
	if !ok {
		result, err = compileFileToPackage(currentProgram, fmt.Sprintf("%s.golem", fullpath), false)
		packageMap[fullpath] = result
	}
	return result, err
}

func GetArrayType(elem Type, len int64) (result *ArrayType) {
	result, ok := arrayTypeMap[ArrayTypeMapKey{elem, len}]
	if !ok {
		result = &ArrayType{Elem: elem, Len: len}
		arrayTypeMap[ArrayTypeMapKey{elem, len}] = result
		// TODO, this should be one generic builtin. Adding the overloads like here
		// does have a negative effect or error messages.
		//
		// TODO the array index operator needs mutability propagation of the first argument.
		// TODO this should be generic for better error messages on missing overloads, listing all currently known array types is a bit much
		registerBuiltin("[", "", ".arr[", "]", []Type{result, TypeInt64}, elem, false)
		registerSimpleTemplate("len", []Type{result}, TypeInt64, IntLit{Type: TypeInt64, Value: len})
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

func GetIntLitType(value int64) (result *IntLitType) {
	result, ok := intLitTypeMap[value]
	if !ok {
		result = &IntLitType{Value: value}
		intLitTypeMap[value] = result
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

func TypeCheckArrayLit(tc *TypeChecker, scope Scope, arg ArrayLit, expected Type) TcExpr {
	switch exp := expected.(type) {
	case *UnspecifiedType:
		var result TcArrayLit
		if len(arg.Items) == 0 {
			result.ElemType = TypeVoid
			return result
		}
		result.Items = make([]TcExpr, len(arg.Items))
		result.Items[0] = TypeCheckExpr(tc, scope, arg.Items[0], TypeUnspecified)
		result.ElemType = result.Items[0].GetType()
		for i := 1; i < len(arg.Items); i++ {
			result.Items[i] = TypeCheckExpr(tc, scope, arg.Items[i], result.ElemType)
		}
		return result
	case *ArrayType:
		var result TcArrayLit
		result.Items = make([]TcExpr, len(arg.Items))
		result.ElemType = exp.Elem
		for i, item := range arg.Items {
			result.Items[i] = TypeCheckExpr(tc, scope, item, exp.Elem)
		}
		ExpectArgsLen(tc, arg, len(arg.Items), int(exp.Len))
		return result
	case *EnumSetType:
		var result TcEnumSetLit
		result.Items = make([]TcExpr, len(arg.Items))
		result.ElemType = exp.Elem
		for i, item := range arg.Items {
			result.Items[i] = TypeCheckExpr(tc, scope, item, exp.Elem)
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
					ReportErrorf(tc, lhsIdent, "type %s has no field %s", exp.Impl.Name, lhsIdent.Source)
				} else {
					result.Items[idx] = TypeCheckExpr(tc, scope, rhs, field.Type)
					if idx < lastIdx {
						ReportErrorf(tc, lhsIdent, "out of order initialization is not allowed (yet?)")
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
				ReportErrorf(tc, arg, "literal has %d values, but %s needs %d values", len(arg.Items), exp.Impl.Name, len(exp.Impl.Fields))
			}
			for i := range result.Items {
				result.Items[i] = TypeCheckExpr(tc, scope, arg.Items[i], exp.Impl.Fields[i].Type)
			}
			return result
		}
	case *ErrorType:
		return newErrorNode(arg)
	case *BuiltinType:
		panic(fmt.Errorf("I don't know about type %s!", exp.Name))
	default:
		panic(fmt.Errorf("I don't know about type %T!", exp))
	}
}

func TypeCheckNilLit(tc *TypeChecker, scope Scope, arg NilLit, expected Type) NilLit {
	switch exp := expected.(type) {
	case *PtrType:
		return NilLit{Source: arg.Source, Type: exp}
	case *UnspecifiedType:
		return NilLit{Source: arg.Source, Type: TypeNilPtr}
	}
	ReportUnexpectedType(tc, arg, expected, TypeNilPtr)
	return NilLit{Source: arg.Source, Type: TypeError}
}

func TypeCheckIfStmt(tc *TypeChecker, scope Scope, stmt IfExpr) (result TcIfStmt) {
	// currently only iteration on strings in possible (of course that is not final)
	result.Condition = TypeCheckExpr(tc, scope, stmt.Condition, TypeBoolean)
	result.Body = TypeCheckExpr(tc, scope, stmt.Body, TypeVoid)
	return
}

func TypeCheckIfElseStmt(tc *TypeChecker, scope Scope, stmt IfElseExpr, expected Type) (result TcIfElseExpr) {
	// currently only iteration on strings in possible (of course that is not final)
	result.Condition = TypeCheckExpr(tc, scope, stmt.Condition, TypeBoolean)
	result.Body = TypeCheckExpr(tc, scope, stmt.Body, expected)
	result.Else = TypeCheckExpr(tc, scope, stmt.Else, expected)
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
	ReportErrorf(tc, expr, "expect type with elements to iterate over")
	return TypeError
}

func TypeCheckForLoopStmt(tc *TypeChecker, scope Scope, loopArg ForLoopStmt) (result TcForLoopStmt) {
	scope = NewSubScope(scope)
	// currently only iteration on strings in possible (of course that is not final)
	result.Collection = TypeCheckExpr(tc, scope, loopArg.Collection, TypeUnspecified)
	elementType := tc.ElementType(result.Collection)
	result.LoopSym = scope.NewSymbol(tc, loopArg.LoopIdent, SkLoopIterator, elementType)
	result.Body = TypeCheckExpr(tc, scope, loopArg.Body, TypeVoid)
	return
}

func TypeCheckWhileLoopStmt(tc *TypeChecker, scope Scope, loopArg WhileLoopStmt) (result TcWhileLoopStmt) {
	scope = NewSubScope(scope)

	result.Source = loopArg.Source
	result.Condition = TypeCheckExpr(tc, scope, loopArg.Condition, TypeBoolean)
	result.Body = TypeCheckExpr(tc, scope, loopArg.Body, TypeVoid)
	return result
}

func TypeCheckPackage(tc *TypeChecker, currentProgram *ProgramContext, arg PackageDef, mainPackage bool) (result *TcPackageDef) {
	result = &TcPackageDef{}
	importScope := NewSubScope(builtinScope)
	importScope.CurrentProgram = currentProgram
	pkgScope := NewSubScope(importScope)

	importScope.CurrentPackage = result //
	pkgScope.CurrentPackage = result
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
			td := TypeCheckEnumDef(tc, pkgScope, stmt)
			result.EnumDefs = append(result.EnumDefs, td)
		case StructDef:
			td := TypeCheckStructDef(tc, pkgScope, stmt)
			result.StructDefs = append(result.StructDefs, td)
		case ProcDef:
			procDef := TypeCheckProcDef(tc, pkgScope, stmt)
			result.ProcDefs = append(result.ProcDefs, procDef)
			if mainPackage && procDef.Name == "main" {
				currentProgram.Main = procDef
			}
		case VariableDefStmt:
			varDef := TypeCheckVariableDefStmt(tc, pkgScope, stmt)
			result.VarDefs = append(result.VarDefs, varDef)
		case DocComment:
			docComment = stmt
			hasDocComment = true
		case EmitStmt:
			result.EmitStatements = append(result.EmitStatements, stmt)
		case StaticExpr:
			// TODO: ensure this expression can be evaluated at compile time
			tcExpr := TypeCheckExpr(tc, pkgScope, stmt.Expr, TypeVoid)
			EvalExpr(tc, tcExpr, pkgScope)
		case ImportStmt:
			pkg, err := GetPackage(currentProgram, arg.WorkDir, stmt.StrLit.Value)
			if err != nil {
				ReportErrorf(tc, stmt.StrLit, "%s", err.Error())
			} else {
				for key, value := range pkg.ExportScope.Variables {
					// TODO solution for conflicts and actually find out where the conflicting symbol comes from.
					_, hasKey := importScope.Variables[key]
					if hasKey {
						panic(fmt.Errorf("name conflicts in imported variable not yet implemented: %s.%s conflicts with some other symbol", stmt.StrLit.Value, key))
					}
					importScope.Variables[key] = value
				}
				for key, value := range pkg.ExportScope.Types {
					_, hasKey := importScope.Types[key]
					if hasKey {
						panic(fmt.Errorf("name conflicts in imported type not yet implemented: %s.%s conflicts with some other symbol", stmt.StrLit.Value, key))
					}
					importScope.Types[key] = value
				}
				for key, value := range pkg.ExportScope.Procedures {
					procs, _ := importScope.Procedures[key]
					importScope.Procedures[key] = append(procs, value...)
				}
				result.Imports = append(result.Imports, pkg)
			}
		case *TraitDef:
			traitDef := TypeCheckTraitDef(tc, pkgScope, stmt)
			// stmt.Name
			result.TraitDefs = append(result.TraitDefs, traitDef)
		default:
			panic(fmt.Errorf("internal error: %T", stmt))
		}
	}
	if mainPackage && currentProgram.Main == nil {
		ReportErrorf(tc, arg, "package '%s' misses main proc", result.Name)
	}

	// TODO: only export what actually wants to be exported
	exportScope := NewSubScope(builtinScope)
	exportScope.CurrentProgram = currentProgram
	*exportScope = *pkgScope
	exportScope.Parent = builtinScope
	result.ExportScope = exportScope

	return result
}
