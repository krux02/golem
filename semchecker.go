package main

import (
	"fmt"
	"math/big"
	"path/filepath"
	"strings"
	"unicode/utf8"
)

type CompileError struct {
	node AstNode
	msg  string
}

type SemChecker struct {
	code         string
	filename     string
	silentErrors bool
	errors       []CompileError
}

func NewSemChecker(code, filename string) *SemChecker {
	return &SemChecker{code: code, filename: filename}
}

// ****
// index to refere to a (currently only builtin) type
// somehow unify this mess
type ScopeImpl struct {
	Parent *ScopeImpl
	// A return stmt needs to know which procedure it belongs to. This
	// pointer points to the corresponding procedure. This should
	// probably be redued to be just the proc signature.
	CurrentProgram  *ProgramContext
	CurrentPackage  *TcPackageDef
	CurrentProc     *TcProcDef
	CurrentTrait    *TcTraitDef
	Variables       map[string]*TcSymbol
	Signatures      map[string][]*Signature
	Types           map[string]Type
	TypeConstraints map[string]TypeConstraint
}

type Scope = *ScopeImpl

func NewSubScope(scope Scope) Scope {
	return &ScopeImpl{
		Parent:         scope,
		CurrentProgram: scope.CurrentProgram,
		CurrentPackage: scope.CurrentPackage,
		CurrentProc:    scope.CurrentProc,
		// TODO maybe do lazy initialization of some of these? Traits are rarely addad.
		Variables:       make(map[string]*TcSymbol),
		Signatures:      make(map[string][]*Signature),
		Types:           make(map[string]Type),
		TypeConstraints: make(map[string]TypeConstraint),
	}
}

func RegisterType(sc *SemChecker, scope Scope, name string, typ Type, context AstNode) {
	if _, ok := scope.Types[name]; ok {
		ReportErrorf(sc, context, "double definition of type '%s'", name)
		// TODO previously defined at ...
		return
	}
	scope.Types[name] = typ
}

func RegisterTrait(sc *SemChecker, scope Scope, trait *TcTraitDef, context AstNode) TypeConstraint {
	name := trait.Name
	if oldTrait, ok := scope.TypeConstraints[name]; ok {
		ReportErrorf(sc, context, "double definition of trait '%s'", trait.Name)
		// TODO previously defined at ...
		return oldTrait
	}
	constraint := &TypeTrait{Impl: trait}
	scope.TypeConstraints[name] = constraint
	return constraint
}

func RegisterProc(sc *SemChecker, scope Scope, proc *Signature, context AstNode) {
	name := proc.Name
	// TODO check for name collisions with the same signature
	scope.Signatures[name] = append(scope.Signatures[name], proc)
}

func (scope Scope) NewSymbol(sc *SemChecker, name *Ident, kind SymbolKind, typ Type) *TcSymbol {
	//result := TcSymbol{Name: name.source, Kind: kind, Typ: typ}
	rawName := name.Source
	result := &TcSymbol{Source: rawName, Kind: kind, Value: nil, Type: typ}
	_, alreadyExists := scope.Variables[rawName]
	if alreadyExists {
		ReportErrorf(sc, name, "redefinition of %s", rawName)
	}
	scope.Variables[name.Source] = result
	return result
}

func (scope Scope) NewConstSymbol(sc *SemChecker, name *Ident, value TcExpr) *TcSymbol {
	rawName := name.Source
	result := &TcSymbol{Source: name.Source, Kind: SkConst, Value: value, Type: value.GetType()}
	_, alreadyExists := scope.Variables[rawName]
	if alreadyExists {
		ReportErrorf(sc, name, "redefinition of %s", rawName)
	}
	scope.Variables[name.Source] = result
	return result
}

func LookUpTypeConstraint(sc *SemChecker, scope Scope, ident *Ident) TypeConstraint {
	name := ident.Source
	if typ, ok := scope.TypeConstraints[name]; ok {
		return typ
	}
	if scope.Parent != nil {
		return LookUpTypeConstraint(sc, scope.Parent, ident)
	}
	ReportErrorf(sc, ident, "type constraint not found: %s", name)
	// TODO add a test for this error message
	return UniqueTypeConstraint{TypeError}
}

func LookUpType(sc *SemChecker, scope Scope, expr TypeExpr) Type {
	switch x := expr.(type) {
	case *Call:
		ident, ok := x.Callee.(*Ident)
		if !ok {
			ReportErrorf(sc, x.Callee, "identifier expected but got %T", x.Callee)
			return TypeError
		}
		switch ident.Source {
		case "array":
			if !ExpectArgsLen(sc, expr, len(x.Args), 2) {
				return TypeError
			}

			arg0 := LookUpType(sc, scope, x.Args[0])
			if arg0 == TypeError {
				// maybe fall back to unchecked array
				return TypeError
			}
			intLit, ok := arg0.(*IntLitType)
			if !ok {
				ReportInvalidAstNode(sc, x.Args[0], "int literal")
				return TypeError
			}
			elem := LookUpType(sc, scope, x.Args[1])
			return GetArrayType(elem, intLit.Value.Int64())
		case "set":
			if !ExpectArgsLen(sc, expr, len(x.Args), 1) {
				return TypeError
			}
			arg0 := LookUpType(sc, scope, x.Args[0])
			if arg0 == TypeError {
				return TypeError
			}
			elem, ok := arg0.(*EnumType)
			if !ok {
				ReportErrorf(sc, x.Args[0], "expect enum type but got %T", arg0)
				return TypeError
			}
			return GetEnumSetType(elem)
		case "ptr":
			if !ExpectArgsLen(sc, expr, len(x.Args), 1) {
				return TypeError
			}
			targetType := LookUpType(sc, scope, x.Args[0])
			if targetType == TypeError {
				return TypeError
			}
			return GetPtrType(targetType)
		default:
			// TODO implement this
			ReportErrorf(sc, ident, "expected 'array', 'set' or 'ptr' here, but got '%s'", ident.Source)
			return TypeError
		}
	case *Ident:
		name := x.Source
		if typ, ok := scope.Types[name]; ok {
			return typ
		}
		if scope.Parent != nil {
			return LookUpType(sc, scope.Parent, expr)
		}
		ReportErrorf(sc, expr, "Type not found: %s", name)
		return TypeError
	case *IntLit:
		return GetIntLitType(x.Value)
		// case nil:
		// 	return TypeError
	}
	panic(fmt.Sprintf("unexpected ast node in type expr: %s type: %T", AstFormat(expr), expr))
}

func LookUpProc(scope Scope, ident *Ident, numArgs int, signatures []*Signature) []*Signature {
	for scope != nil {
		if localSignatures, ok := scope.Signatures[ident.Source]; ok {
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

func LookUpLetSymRecursive(sc *SemChecker, scope Scope, ident *Ident, expected TypeConstraint) *TcSymRef {
	if scope == nil {
		ReportErrorf(sc, ident, "let sym not found: %s", ident.Source)
		return &TcSymRef{
			Source: ident.Source,
			Type:   TypeError,
		}
	}

	if sym, ok := scope.Variables[ident.Source]; ok {
		return &TcSymRef{
			Source: ident.Source,
			Sym:    sym,
			Type:   ExpectType(sc, ident, sym.Type, expected),
		}
	}
	return LookUpLetSymRecursive(sc, scope.Parent, ident, expected)
}

func LookUpLetSym(sc *SemChecker, scope Scope, ident *Ident, expected TypeConstraint) *TcSymRef {

	if uniqueType, isUniqueType := expected.(UniqueTypeConstraint); isUniqueType {
		if enumDef, ok := uniqueType.Typ.(*EnumType); ok {
			for _, sym := range enumDef.Impl.Values {
				if sym.Source == ident.Source {
					return &TcSymRef{Source: ident.Source, Sym: sym, Type: enumDef}
				}
			}
		}
	}
	return LookUpLetSymRecursive(sc, scope, ident, expected)
}

func SemCheckTypeDef(sc *SemChecker, scope Scope, def *TypeDef) TcExpr {

	lhs, rhs, isAssignment := MatchAssign(def.Expr)
	if !isAssignment {
		ReportErrorf(sc, def.Expr, "expect assignment")
		return &TcErrorNode{Source: def.Source, SourceNode: def}
	}
	name, isIdent := lhs.(*Ident)
	if !isIdent {
		ReportErrorf(sc, lhs, "expect ident")
		return &TcErrorNode{Source: def.Source, SourceNode: def}
	}
	callee, body, isPrefixCall := MatchPrefixCall(rhs)
	if !isPrefixCall {
		ReportErrorf(sc, rhs, "expect prefix call")
		return &TcErrorNode{Source: def.Source, SourceNode: def}
	}
	block, isBlock := body.(*CodeBlock)
	if !isBlock {
		ReportErrorf(sc, body, "expect code block")
		return &TcErrorNode{Source: def.Source, SourceNode: def}
	}

	ValidNameCheck(sc, name, "type")

	var importc bool
	if def.Annotations != nil {
		if def.Annotations.Value == "importc" {
			importc = true
		} else {
			ReportInvalidAnnotations(sc, def.Annotations)
		}
	}

	switch callee.Source {
	case "struct":
		result := &TcStructDef{
			Source:  def.Source,
			Name:    name.Source,
			Importc: importc,
		}
		structType := &StructType{Impl: result}
		// TODO: test when Importc that all fields are also Importc (or importc compatible, like builtin integer types)

		for _, field := range block.Items {
			if lhs, rhs, ok := MatchColonExpr(field); !ok {
				ReportErrorf(sc, field, "expect colon expr")
			} else {
				if nameIdent, ok := lhs.(*Ident); !ok {
					ReportErrorf(sc, lhs, "expect Ident, but got %T", lhs)
				} else {
					ValidNameCheck(sc, nameIdent, "struct field")
					tcField := &TcStructField{
						Name: nameIdent.Source,
						Type: LookUpType(sc, scope, rhs),
					}
					result.Fields = append(result.Fields, tcField)
				}
			}
		}
		RegisterType(sc, scope, structType.Impl.Name, structType, name)
		return result
	case "enum":
		result := &TcEnumDef{
			Source:  def.Source,
			Name:    name.Source,
			Importc: importc,
		}
		enumType := &EnumType{Impl: result}
		for _, value := range block.Items {
			ident, isIdent := value.(*Ident)
			if !isIdent {
				ReportErrorf(sc, value, "enum entry must be an identifier")
				continue
			}

			ValidNameCheck(sc, ident, "enum value")
			sym := &TcSymbol{
				Source: ident.Source,
				Kind:   SkEnum,
				Type:   enumType,
			}
			result.Values = append(result.Values, sym)
		}
		RegisterType(sc, scope, enumType.Impl.Name, enumType, name)
		registerBuiltin("string", fmt.Sprintf("%s_names_array[", result.Name), "", "]", []Type{enumType}, TypeStr, 0)
		for _, intType := range TypeAnyInt.Items {
			builtinType := intType.(*BuiltinIntType)
			registerBuiltin(builtinType.Name, fmt.Sprintf("(%s)", builtinType.InternalName), "", "", []Type{enumType}, intType, 0)
			registerBuiltin(result.Name, fmt.Sprintf("(%s)", result.Name), "", "", []Type{intType}, enumType, 0)
		}
		registerBuiltin("contains", "(((", ") & (1 << (", "))) != 0)", []Type{GetEnumSetType(enumType), enumType}, TypeBoolean, 0)
		return result
	default:
		ReportErrorf(sc, callee, "type must be struct or enum")
	}
	return &TcErrorNode{Source: def.Source, SourceNode: def}
}

func ParseTraitDef(sc *SemChecker, def *TraitDef) (name *Ident, dependentTypes []*Ident, signatures []*ProcDef) {
	if lhs, rhs, isAssign := MatchAssign(def.Expr); isAssign {
		switch x := lhs.(type) {
		case *Call:
			switch n := x.Callee.(type) {
			case *Ident:
				name = n
			default:
				ReportErrorf(sc, n, "expected identifier here")
			}
			for _, arg := range x.Args {
				switch arg := arg.(type) {
				case *Ident:
					dependentTypes = append(dependentTypes, arg)
				default:
					ReportErrorf(sc, arg, "expected identifier here")
				}
			}
		default:
			ReportErrorf(sc, x, "expected call exper here")
		}

		if block, isBlock := rhs.(*CodeBlock); isBlock {
			for _, it := range block.Items {
				if procDef, isProcDef := it.(*ProcDef); isProcDef {
					signatures = append(signatures, procDef)
				} else {
					ReportErrorf(sc, it, "expect proc def here")
				}
			}
		} else {
			ReportErrorf(sc, rhs, "expect code block here")
		}
	} else {
		ReportErrorf(sc, def.Expr, "expect assignment")
	}

	return
}

func SemCheckTraitDef(sc *SemChecker, scope Scope, def *TraitDef) *TcTraitDef {
	name, dependentTypes, signatures := ParseTraitDef(sc, def)
	ValidNameCheck(sc, name, "trait")

	result := &TcTraitDef{}
	result.Source = def.Source
	result.Name = name.Source
	traitScope := NewSubScope(scope)
	traitScope.CurrentTrait = result
	trait := RegisterTrait(sc, scope, result, name)

	for _, typ := range dependentTypes {
		// TODO, support setting the Constraint here
		sym := &GenericTypeSymbol{Source: typ.Source, Name: typ.Source, Constraint: trait}
		result.DependentTypes = append(result.DependentTypes, sym)
		RegisterType(sc, traitScope, sym.Name, sym, typ)
	}

	for _, procDef := range signatures {
		tcProcDef := SemCheckProcDef(sc, traitScope, procDef)
		result.Signatures = append(result.Signatures, tcProcDef.Signature)
	}

	return result
}

func SemCheckProcDef(sc *SemChecker, parentScope Scope, def *ProcDef) (result *TcProcDef) {

	var body Expr
	var expr = def.Expr

	// validate syntax

	if lhs, rhs, isAssign := MatchAssign(expr); isAssign {
		body = rhs
		expr = lhs
	}

	var resultType TypeExpr
	if lhs, rhs, isColonExpr := MatchColonExpr(expr); isColonExpr {
		resultType = rhs
		expr = lhs
	}

	var argsRaw []Expr
	if call, isCall := expr.(*Call); isCall {
		argsRaw = call.Args
		expr = call.Callee
	} else {
		// TODO, test this
		ReportErrorf(sc, expr, "proc def requires an argument list")
	}

	type GenericArgument struct {
		Source    string
		Name      *Ident
		TraitName *Ident
	}

	type ProcArgument struct {
		Source  string
		Name    *Ident
		Mutable bool
		Type    TypeExpr
	}

	var genericArgs []GenericArgument
	if bracketExpr, isBracketExpr := expr.(*BracketExpr); isBracketExpr {
		expr = bracketExpr.Callee
		genericArgs = make([]GenericArgument, 0, len(bracketExpr.Args))
		// TODO do something with args
		for _, arg := range bracketExpr.Args {
			lhs, rhs, isColonExpr := MatchColonExpr(arg)
			if !isColonExpr {
				// TODO test error message
				ReportErrorf(sc, arg, "generic argument must be a colon expr")
				continue
			}
			name, isIdent := lhs.(*Ident)
			if !isIdent {
				// TODO test error message
				ReportErrorf(sc, lhs, "generic argument name must be an Identifire, but it is %T", lhs)
				continue
			}
			traitName, isIdent := rhs.(*Ident)
			if !isIdent {
				// TODO firstToken is not the right code location
				ReportErrorf(sc, rhs, "generic argument constraint must be an Identifire, but it is %T", rhs)
				continue
			}

			genericArg := GenericArgument{Source: arg.GetSource(), Name: name, TraitName: traitName}
			genericArgs = append(genericArgs, genericArg)
		}
	}

	var name *Ident
	if ident, isIdent := expr.(*Ident); isIdent {
		name = ident
	} else {
		// TODO firstToken is not the right code location
		ReportErrorf(sc, expr, "proc name must be an identifier, but it is %T", expr)
	}

	var args []ProcArgument
	var newArgs []ProcArgument
	for _, arg := range argsRaw {

		var newArg Expr
		var typeExpr TypeExpr
		var gotTypeExpr bool = false

		newArg, typeExpr, gotTypeExpr = MatchColonExpr(arg)
		if gotTypeExpr {
			arg = newArg
		}

		var mutable = false
		if varExpr, ok := arg.(*VarExpr); ok {
			mutable = true
			arg = varExpr.Expr
		}
		if ident, isIdent := arg.(*Ident); isIdent {
			newArgs = append(newArgs, ProcArgument{Source: ident.Source, Name: ident, Mutable: mutable})
		} else {
			ReportErrorf(sc, arg, "expected identifier but got %T (%s)", arg, AstFormat(arg))
			// TODO decide if this should be an error node or something in the arguments list
			newArgs = append(newArgs, ProcArgument{Source: arg.GetSource(), Name: &Ident{Source: "_"}})
		}

		if gotTypeExpr {
			for i := range newArgs {
				newArgs[i].Type = typeExpr
			}
			args = append(args, newArgs...)

			newArgs = newArgs[:0]
		}
	}
	if len(newArgs) > 0 {
		// TODO test this error
		ReportErrorf(sc, newArgs[0].Name, "arguments have no type: %+v", newArgs)
	}

	ValidNameCheck(sc, name, "proc")

	// apply doc section
	if def.DocComment != nil {
		name.Comment = append(name.Comment, def.DocComment.BaseDoc...)

	DOCSECTIONS1:
		for _, it := range def.DocComment.NamedDocSections {
			key := it.Name
			value := it.Lines

			for i := range args {
				if args[i].Name.Source == key {
					commentRef := &args[i].Name.Comment
					*commentRef = append(*commentRef, value...)
					continue DOCSECTIONS1
				}
			}
			ReportInvalidDocCommentKey(sc, it)
		}
	}
	procScope := NewSubScope(parentScope)
	// from here on old school type checking

	result = &TcProcDef{}
	procScope.CurrentProc = result

	if def.Annotations != nil {
		if def.Annotations.Value == "importc" {
			result.Importc = true
		} else {
			ReportInvalidAnnotations(sc, def.Annotations)
		}
	}

	var genericParams []*GenericTypeSymbol
	if parentScope.CurrentTrait != nil {
		genericParams = append(genericParams, parentScope.CurrentTrait.DependentTypes...)
	}

	for _, genericArg := range genericArgs {
		constraint := LookUpTypeConstraint(sc, parentScope, genericArg.TraitName)
		if (constraint == UniqueTypeConstraint{TypeError}) {
			continue
		}
		name := genericArg.Name

		genTypeSym := &GenericTypeSymbol{Source: name.Source, Name: name.Source, Constraint: constraint}
		genericParams = append(genericParams, genTypeSym)
		// make the generic type symbol available to look up in its body
		RegisterType(sc, procScope, name.Source, genTypeSym, def)

		switch constraint := constraint.(type) {
		case *TypeTrait:
			for _, sig := range constraint.Impl.Signatures {
				RegisterProc(sc, procScope, sig, genericArg.TraitName)
			}
		default:
			ReportWarningf(sc, genericArg.TraitName, "currently only traits are supported in the compiler as type constraints")
		}
	}

	var params []*TcSymbol
	for _, arg := range args {
		symKind := SkProcArg
		if arg.Mutable {
			symKind = SkVarProcArg
		}

		paramType := LookUpType(sc, procScope, arg.Type)

		typ := maybeWrapWithOpenGenericType(
			paramType,
			genericParams,
		)

		var tcArg *TcSymbol
		// TODO this is ugly. Refactoring `NewSymbol` to a simple `RegisterSymbol`
		// might be a better solution.
		if arg.Name.Source != "_" {
			ValidNameCheck(sc, arg.Name, "proc arg")
			tcArg = procScope.NewSymbol(sc, arg.Name, symKind, typ)
		} else {
			// parameters with the name "_" are explicity not put in the scope.
			tcArg = &TcSymbol{
				Source: arg.Name.Source,
				Kind:   symKind,
				Value:  nil,
				Type:   typ,
			}
		}
		params = append(params, tcArg)
	}

	// makeGenericSignature(def.Name.Source, genericParams, params, resultType, 0)
	signature := &Signature{
		Name:          name.Source,
		GenericParams: genericParams,
		Params:        params,
		Impl:          result,
	}
	result.Signature = signature

	if resultType == nil {
		ReportErrorf(sc, def, "proc def needs result type specified")
		signature.ResultType = TypeError
	} else {
		signature.ResultType = LookUpType(sc, procScope, resultType)
	}

	if result.Importc || name.Source == "main" {
		// TODO, don't special case `main` like this here
		result.MangledName = name.Source
	} else {
		mangledNameBuilder := &strings.Builder{}
		mangledNameBuilder.WriteString(name.Source)
		mangledNameBuilder.WriteRune('_')
		for _, arg := range result.Signature.Params {
			arg.Type.ManglePrint(mangledNameBuilder)
		}
		result.MangledName = mangledNameBuilder.String()
	}

	// register proc before type checking the body to allow recursion. (TODO needs a test)
	RegisterProc(sc, parentScope, signature, name)

	if result.Importc {
		if body != nil {
			ReportErrorf(sc, body, "proc is importc, it may not have a body")
		}
	} else if parentScope.CurrentTrait != nil {
		if body != nil {
			ReportErrorf(sc, body, "proc is importc, it may not have a body")
		}
	} else {
		if body == nil {
			ReportErrorf(sc, def, "proc def misses a body")
		} else {
			result.Body = SemCheckExpr(sc, procScope, body, UniqueTypeConstraint{signature.ResultType})
		}
	}

	return result
}

func ReportMessagef(sc *SemChecker, node AstNode, kind string, msg string, args ...interface{}) {
	newMsg := fmt.Sprintf(msg, args...)
	sc.errors = append(sc.errors, CompileError{node: node, msg: newMsg})
	if !sc.silentErrors {
		if node == nil {
			fmt.Println(msg)
		} else {
			line, columnStart, columnEnd := LineColumnStr(sc.code, node.GetSource())
			fmt.Printf("%s(%d, %d-%d) %s: %s\n", sc.filename, line, columnStart, columnEnd, kind, newMsg)
		}
	}
}

func ReportErrorf(sc *SemChecker, node AstNode, msg string, args ...interface{}) {
	ReportMessagef(sc, node, "Error", msg, args...)
}

func ReportWarningf(sc *SemChecker, node AstNode, msg string, args ...interface{}) {
	ReportMessagef(sc, node, "Warning", msg, args...)
}

func ReportUnexpectedType(sc *SemChecker, context AstNode, expected TypeConstraint, gotten Type) {
	ReportErrorf(sc, context, "expected type '%s' but got type '%s'", AstFormat(expected), AstFormat(gotten))
}

func ReportInvalidDocCommentKey(sc *SemChecker, section *NamedDocSection) {
	ReportErrorf(sc, section, "invalid doc comment key: %s", section.Name)
}

func ReportInvalidAnnotations(sc *SemChecker, lit *StrLit) {
	ReportErrorf(sc, lit, "invalid annotations string: %s", lit.Value)
}

func ReportInvalidAstNode(sc *SemChecker, node AstNode, expected string) {
	ReportErrorf(sc, node, "invalid ast node. Expected %s, but got %T", expected, node)
}

func ReportIllegalDocComment(sc *SemChecker, doc *PrefixDocComment) {
	ReportErrorf(sc, doc, "doc comment is illegal here, use normal comment instead")
}

func ReportMustBeMutable(sc *SemChecker, doc Expr) {
	ReportErrorf(sc, doc, "expression must be mutable")
}

func ExpectType(sc *SemChecker, node AstNode, gotten Type, expected TypeConstraint) Type {
	// TODO this doesn't work for partial types (e.g. array[<unspecified>])
	if gotten == TypeError {
		return TypeError
	}
	if expected == TypeUnspecified {
		return gotten
	}
	switch expected := expected.(type) {
	case *UnspecifiedType:
		return gotten
	case *TypeGroup:
		for _, it := range expected.Items {
			if gotten == it {
				return gotten
			}
		}
	case UniqueTypeConstraint:
		if expected.Typ == gotten {
			return gotten
		}
		if expected.Typ == TypeError {
			return TypeError
		}
	}

	ReportUnexpectedType(sc, node, expected, gotten)
	return TypeError
}

func (structDef *TcStructDef) GetField(name string) (resField *TcStructField, idx int) {
	for i, field := range structDef.Fields {
		if field.Name == name {
			return field, i
		}
	}
	return &TcStructField{Source: name, Name: name, Type: TypeError}, -1
}

func SemCheckDotExpr(sc *SemChecker, scope Scope, call *Call, expected TypeConstraint) TcExpr {
	if !ExpectArgsLen(sc, call, len(call.Args), 2) {
		return newErrorNode(call)
	}

	lhs := SemCheckExpr(sc, scope, call.Args[0], TypeUnspecified)

	result := &TcDotExpr{
		Source: call.Source,
		Lhs:    lhs,
	}
	// fmt.Printf("lhs: %T %+v\n", result.Lhs, result.Lhs)
	typ := result.Lhs.GetType()
	switch t := typ.(type) {
	case *StructType:
		rhs, isIdent := call.Args[1].(*Ident)
		if !isIdent {
			ReportErrorf(sc, call.Args[1], "right of dot operator needs to be an identifier, but it is %T", call.Args[1])
			return result
		}
		var idx int
		result.Rhs, idx = t.Impl.GetField(rhs.Source)
		if idx < 0 {
			ReportErrorf(sc, rhs, "type %s has no field %s", t.Impl.Name, rhs.GetSource())
			return newErrorNode(call)
		}
		ExpectType(sc, rhs, result.Rhs.GetType(), expected)
		return result
	case *ErrorType:
		return newErrorNode(call)
	default:
		ReportErrorf(sc, lhs, "dot call is only supported on struct types, but got: %s", AstFormat(typ))
		return newErrorNode(call)
	}
}

func errorProcSym(ident *Ident) TcProcSymbol {
	procDef := &TcErrorProcDef{}
	signature := &Signature{Name: ident.Source, ResultType: TypeError, Impl: procDef}
	procDef.Signature = signature

	return TcProcSymbol{
		Source: ident.GetSource(),
		// maybe add some debug information here
		Signature: signature,
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

func argTypeGroupAtIndex(signatures []*Signature, idx int) (result TypeConstraint) {
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
		builder.Items = AppendNoDuplicats(builder.Items, typ)
	}
	if len(builder.Items) == 1 {
		return UniqueTypeConstraint{builder.Items[0]}
	}
	if len(builder.Items) == 0 {
		return TypeUnspecified
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
			return GenericParamSignatureMatch(exprPtrType.Target, paramPtrType.Target, substitutions)
		}
	}

	{
		exprEnumSetType, exprIsEnumSetType := exprType.(*EnumSetType)
		paramEnumSetType, paramIsEnumSetType := paramType.(*EnumSetType) // TODO, this line is untested
		if exprIsEnumSetType && paramIsEnumSetType {
			return GenericParamSignatureMatch(exprEnumSetType.Elem, paramEnumSetType.Elem, substitutions)
		}
	}

	{
		exprTypeType, exprIsTypeType := exprType.(*TypeType)
		paramTypeType, paramIsTypeType := paramType.(*TypeType)
		if exprIsTypeType && paramIsTypeType {
			return GenericParamSignatureMatch(exprTypeType.WrappedType, paramTypeType.WrappedType, substitutions)
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
		return GetTypeType(RecursiveTypeSubstitution(typ.WrappedType, substitutions))
	case *PtrType:
		return GetPtrType(RecursiveTypeSubstitution(typ.Target, substitutions))
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

func SignatureApplyTypeSubstitution(sig *Signature, substitutions []Substitution) *Signature {
	if len(substitutions) == 0 {
		return sig
	}
	newParams := make([]*TcSymbol, len(sig.Params))
	for j, param := range sig.Params {
		newType := ApplyTypeSubstitutions(param.Type, substitutions)
		if newType != param.Type {
			newParams[j] = &TcSymbol{
				Source: param.Source,
				Kind:   param.Kind,
				Value:  param.Value, // only set when symbol is `const`
				Type:   newType,
			}
		} else {
			newParams[j] = param
		}
	}

	result := &Signature{}
	result.Params = newParams
	result.ResultType = ApplyTypeSubstitutions(sig.ResultType, substitutions)
	result.Substitutions = append(sig.Substitutions, substitutions...)
	result.Varargs = sig.Varargs
	result.Impl = sig.Impl
	return result
}

func SemCheckCall(sc *SemChecker, scope Scope, call *Call, expected TypeConstraint) TcExpr {
	ident, isIdent := call.Callee.(*Ident)
	if !isIdent {
		ReportErrorf(sc, call.Callee, "expected identifier but got %T (%s)", call.Callee, AstFormat(call.Callee))
		return newErrorNode(call.Callee)
	}
	// language level reserved calls
	switch ident.Source {
	case ".":
		return SemCheckDotExpr(sc, scope, call, expected)
	case ":":
		if !ExpectArgsLen(sc, call, len(call.Args), 2) {
			return newErrorNode(call)
		}
		typ := LookUpType(sc, scope, TypeExpr(call.Args[1]))
		typ = ExpectType(sc, call, typ, expected)
		return SemCheckExpr(sc, scope, call.Args[0], UniqueTypeConstraint{typ})
	}

	signatures := LookUpProc(scope, ident, len(call.Args), nil)

	var checkedArgs []TcExpr
	hasArgTypeError := false
	for i, arg := range call.Args {
		// TODO reuse TypeGroupBuilder here
		expectedArgType := argTypeGroupAtIndex(signatures, i)
		tcArg := SemCheckExpr(sc, scope, arg, expectedArgType)
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

	result := &TcCall{Source: call.Source}

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

			ReportErrorf(sc, call, "%s", builder.String())
		}
		result.Sym = errorProcSym(ident)
		result.Args = checkedArgs

	case 1:
		sig := signatures[0]

		for i, arg := range sig.Params {
			if _, isGeneric := arg.Type.(*OpenGenericType); isGeneric {
				panic(
					fmt.Sprintf(
						"internal error: generics arguments are expected to instanciated\n%s\n%s\n",
						DebugAstFormat(sig.Impl),
						DebugAstFormat(call),
					),
				)
			}

			if arg.Kind == SkVarProcArg {
				RequireMutable(sc, checkedArgs[i])
			}
		}
		if _, isGeneric := sig.ResultType.(*OpenGenericType); isGeneric {
			panic("internal error: generics are expected to instanciated")
		}

		switch impl := sig.Impl.(type) {
		case *TcBuiltinProcDef, *TcProcDef:
			result.Sym = TcProcSymbol{Source: ident.Source, Signature: sig}
			result.Args = checkedArgs
			ExpectType(sc, call, sig.ResultType, expected)
		case *TcBuiltinGenericProcDef:
			// TODO: actually use sig.Substitutions to instantiate the generic proc def.
			// TODO: ensure that only one instance is generated per signature.
			// TODO: add back reference to original generic TcGenericProcDef
			//
			// currently replacing the signature alone is enough, but that is not a
			// general solution.
			//
			sigInstance := &Signature{
				Name:          sig.Name,
				GenericParams: sig.GenericParams,
				Params:        sig.Params,
				ResultType:    sig.ResultType,
			}

			implInstance := &TcBuiltinProcDef{
				Signature: sigInstance,
				Prefix:    impl.Prefix,
				Infix:     impl.Infix,
				Postfix:   impl.Postfix,
			}

			sigInstance.Impl = implInstance

			result.Sym = TcProcSymbol{Source: ident.Source, Signature: sigInstance}
			result.Args = checkedArgs
			ExpectType(sc, call, sig.ResultType, expected)
		case *TcTemplateDef:
			sigParams := impl.Signature.Params
			if len(checkedArgs) != len(sigParams) {
				panic("internal error")
			}
			var substitutions []TemplateSubstitution
			for i := range sigParams {
				var value Expr = checkedArgs[i]
				if tmp, isWrappedUntyped := value.(*TcWrappedUntypedAst); isWrappedUntyped {
					value = tmp.Expr
				}
				substitutions = append(substitutions, TemplateSubstitution{Sym: sigParams[i], Value: value})
			}
			templateBodyScope := NewSubScope(scope)
			substitution := SemCheckExpr(sc,
				templateBodyScope,
				impl.Body.RecSubSyms(substitutions),
				UniqueTypeConstraint{impl.Signature.ResultType},
			)
			ExpectType(sc, call, substitution.GetType(), expected)
			return substitution
		case *TcBuiltinMacroDef:
			result.Sym = TcProcSymbol{Source: ident.Source, Signature: sig}
			result.Args = checkedArgs
			newResult := impl.MacroFunc(sc, scope, result)
			ExpectType(sc, call, newResult.GetType(), expected)
			return newResult
		default:
			panic(fmt.Errorf("internal error: %T, call: %s", impl, AstFormat(call)))
		}
	default:
		ReportErrorf(sc, ident, "too many overloads: %s", ident.Source)
		result.Sym = errorProcSym(ident)
		result.Args = checkedArgs
	}

	return result
}

func (sc *SemChecker) ApplyDocComment(expr Expr, doc *PrefixDocComment) Expr {
	switch expr2 := expr.(type) {
	case *VariableDefStmt:
		_, name, _, _, _ := MatchVariableDefStatement(sc, expr2)
		name.Comment = append(name.Comment, doc.BaseDoc...)
		for _, it := range doc.NamedDocSections {
			key := it.Name
			value := it.Lines

			if name.Source != key {
				ReportInvalidDocCommentKey(sc, it)
				continue
			}

			name.Comment = append(name.Comment, value...)
		}
		return expr2
	case *ProcDef:
		expr2.DocComment = doc
		return expr2
	case *TypeDef:
		// TODO this pattern matching code is code duplication from sem checkTypeDef. This is bad
		lhs, rhs, isAssignment := MatchAssign(expr2.Expr)
		if !isAssignment {
			return expr2
		}
		name, isIdent := lhs.(*Ident)
		if !isIdent {
			return expr2
		}
		_, body, isPrefixCall := MatchPrefixCall(rhs)
		if !isPrefixCall {
			return expr2
		}
		block, isBlock := body.(*CodeBlock)
		if !isBlock {
			return expr2
		}
		name.Comment = append(name.Comment, doc.BaseDoc...)
	DOCSECTIONS2:
		for _, it := range doc.NamedDocSections {
			key := it.Name
			value := it.Lines
			for _, it := range block.Items {
				if lhs, _, isColonExpr := MatchColonExpr(it); isColonExpr {
					it = lhs
				}
				ident, isIdent := it.(*Ident)
				if !isIdent {
					continue
				}
				if ident.Source == key {
					ident.Comment = append(ident.Comment, value...)
					continue DOCSECTIONS2
				}
			}
			ReportInvalidDocCommentKey(sc, it)
		}
		return expr2
	default:
		fmt.Printf("typ: %T\n", expr2)
		ReportIllegalDocComment(sc, doc)
		return expr2
	}
}

func SemCheckCodeBlock(sc *SemChecker, scope Scope, arg *CodeBlock, expected TypeConstraint) *TcCodeBlock {
	result := &TcCodeBlock{Source: arg.Source}
	scope = NewSubScope(scope)
	N := len(arg.Items)
	if N > 0 {
		resultItems := make([]TcExpr, 0, N)
		var docComment *PrefixDocComment
		for i, item := range arg.Items {
			if comment, ok := item.(*PrefixDocComment); ok {
				docComment = comment
				continue
			} else {
				if docComment != nil {
					item = sc.ApplyDocComment(item, docComment)
					docComment = nil
				}
				if i == N-1 {
					resultItems = append(resultItems, SemCheckExpr(sc, scope, item, expected))
				} else {
					resultItems = append(resultItems, SemCheckExpr(sc, scope, item, UniqueTypeConstraint{TypeVoid}))
				}
			}
		}
		result.Items = resultItems
	} else {
		// empty block is type void
		ExpectType(sc, arg, TypeVoid, expected)
	}
	return result
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

func ValidNameCheck(sc *SemChecker, ident *Ident, extraword string) {
	if errMsg := IsValidIdentifier(ident.Source); errMsg != "" {
		ReportErrorf(sc, ident, "%s %s", extraword, errMsg)
	}
}

func SemCheckVariableDefStmt(sc *SemChecker, scope Scope, arg *VariableDefStmt) *TcVariableDefStmt {
	kind, name, typeExpr, value, ok := MatchVariableDefStatement(sc, arg)
	if !ok {
		// TODO return somtehing that indicades an error
		return nil
	}
	ValidNameCheck(sc, name, "var")
	result := &TcVariableDefStmt{Source: arg.Source}
	if value == nil {
		typ := LookUpType(sc, scope, typeExpr)
		if kind != SkVar {
			ReportErrorf(sc, arg, "initial value required")
		}
		result.Value = typ.DefaultValue(sc, name)
		result.Sym = scope.NewSymbol(sc, name, kind, typ)
	} else {
		var expected TypeConstraint = TypeUnspecified
		if typeExpr != nil {
			expected = UniqueTypeConstraint{LookUpType(sc, scope, typeExpr)}
		}
		result.Value = SemCheckExpr(sc, scope, value, expected)
		if kind == SkConst {
			// TODO: this needs proper checking if it can even computed
			result.Sym = scope.NewConstSymbol(sc, name, EvalExpr(sc, result.Value, scope))
		} else {
			result.Sym = scope.NewSymbol(sc, name, kind, result.Value.GetType())
		}
	}

	return result
}

func SemCheckReturnExpr(sc *SemChecker, scope Scope, arg *ReturnExpr) *TcReturnExpr {
	result := &TcReturnExpr{Value: SemCheckExpr(sc, scope, arg.Value, UniqueTypeConstraint{scope.CurrentProc.Signature.ResultType})}
	result.Source = arg.Source
	return result
}

func UnifyType(a, b Type) Type {
	if a != b {
		panic("type incompatible")
	}
	return a
}

func (block *TcCodeBlock) GetType() Type {
	if len(block.Items) == 0 {
		return TypeVoid
	}
	return block.Items[len(block.Items)-1].GetType()
}

func (lit *TcIntLit) GetType() Type {
	if lit.Type == nil {
		panic(fmt.Errorf("internal error: type of IntLit not set"))
	}
	return lit.Type
}

func (lit *TcFloatLit) GetType() Type {
	if lit.Type == nil {
		panic(fmt.Errorf("internal error: type of FloatLit %s not set", lit.Source))
	}
	return lit.Type
}

func (lit *NilLit) GetType() Type {
	if lit.Type == nil {
		panic(fmt.Errorf("internal error: type of NullPtrLit not set"))
	}
	return lit.Type
}

func (_ *TcErrorNode) GetType() Type               { return TypeError }
func (call *TcCall) GetType() Type                 { return call.Sym.Signature.ResultType }
func (lit *TcStrLit) GetType() Type                { return lit.Type }
func (lit *TcArrayLit) GetType() Type              { return GetArrayType(lit.ElemType, int64(len(lit.Items))) }
func (lit *TcEnumSetLit) GetType() Type            { return GetEnumSetType(lit.ElemType) }
func (lit *TcStructLit) GetType() Type             { return lit.Type }
func (sym *TcSymbol) GetType() Type                { return sym.Type }
func (sym *TcSymRef) GetType() Type                { return sym.Type }
func (stmt *TcVariableDefStmt) GetType() Type      { return TypeVoid }
func (stmt *TcStructDef) GetType() Type            { return TypeVoid }
func (stmt *TcEnumDef) GetType() Type              { return TypeVoid }
func (stmt *TcForLoopStmt) GetType() Type          { return TypeVoid }
func (stmt *TcWhileLoopStmt) GetType() Type        { return TypeVoid }
func (arg *TcBuiltinProcDef) GetType() Type        { return TypeVoid }
func (arg *TcBuiltinGenericProcDef) GetType() Type { return TypeVoid }
func (arg *TcProcDef) GetType() Type               { return TypeVoid }
func (arg *TcTemplateDef) GetType() Type           { return TypeVoid }
func (arg *TcPackageDef) GetType() Type            { return TypeVoid }
func (arg *TcBuiltinMacroDef) GetType() Type       { return TypeVoid }
func (arg *TcErrorProcDef) GetType() Type          { return TypeVoid }
func (stmt *TcIfStmt) GetType() Type               { return TypeVoid }
func (stmt *TcIfElseExpr) GetType() Type           { return UnifyType(stmt.Body.GetType(), stmt.Else.GetType()) }
func (returnExpr *TcReturnExpr) GetType() Type     { return TypeNoReturn }
func (expr *TcTypeContext) GetType() Type          { return GetTypeType(expr.WrappedType) }
func (expr *TcDotExpr) GetType() Type              { return expr.Rhs.GetType() }
func (field *TcStructField) GetType() Type         { return field.Type }
func (expr *TcWrappedUntypedAst) GetType() Type    { return TypeUntyped }

func SemCheckIntLit(sc *SemChecker, scope Scope, arg *IntLit, expected TypeConstraint) TcExpr {
	uniqueConstraint, isUniqueConstraint := expected.(UniqueTypeConstraint)
	if !isUniqueConstraint {
		ReportErrorf(sc, arg, "int literal needs to have a unique type constraint, but got '%s'", AstFormat(expected))
		return &TcIntLit{
			Source: arg.Source,
			Type:   TypeError,
			Value:  arg.Value,
		}
	}

	switch typ := uniqueConstraint.Typ.(type) {
	case *BuiltinFloatType:

		if !arg.Value.IsInt64() {
			ReportErrorf(sc, arg, "can't represent %d as %s precisely", arg.Value, typ.Name)
			goto error
		}
		int64Value := arg.Value.Int64()
		if typ == TypeFloat32 {
			if int64(float32(int64Value)) != int64Value {
				ReportErrorf(sc, arg, "can't represent %d as %s precisely", arg.Value, typ.Name)
				goto error
			}
		} else if typ == TypeFloat64 {
			if int64(float64(int64Value)) != int64Value {
				ReportErrorf(sc, arg, "can't represent %d as %s precisely", arg.Value, typ.Name)
				goto error
			}
		}
		return &TcFloatLit{
			Source: arg.Source,
			Type:   typ,
			Value:  big.NewFloat(0).SetInt64(int64Value),
		}
	case *BuiltinIntType:
		if (arg.Value.Cmp(typ.MinValue) < 0) || (typ.MaxValue.Cmp(arg.Value) < 0) {
			ReportErrorf(sc, arg, "integer literal %d out of range for type '%s' [%d..%d]",
				arg.Value, typ.Name, typ.MinValue, typ.MaxValue)
			goto error
		}
		return &TcIntLit{
			Source: arg.Source,
			Type:   typ,
			Value:  arg.Value,
		}
	case *IntLitType:
		if arg.Value != typ.Value {
			ReportUnexpectedType(sc, arg, expected, typ)
			goto error
		}
		return &TcIntLit{
			Source: arg.Source,
			Type:   typ,
			Value:  arg.Value,
		}
	case *ErrorType:
		// skipping rereporting here
		goto error
	}

	ReportUnexpectedType(sc, arg, expected, GetIntLitType(arg.Value))
error:
	return &TcIntLit{
		Source: arg.Source,
		Type:   TypeError,
		Value:  arg.Value,
	}
}

func SemCheckFloatLit(sc *SemChecker, scope Scope, arg *FloatLit, expected TypeConstraint) TcExpr {
	uniqueConstraint, isUniqueConstraint := expected.(UniqueTypeConstraint)
	if !isUniqueConstraint {
		ReportErrorf(sc, arg, "float literal needs to have a unique type constraint, but got '%s'", AstFormat(expected))
		return &TcFloatLit{
			Source: arg.Source,
			Type:   TypeError,
			Value:  arg.Value,
		}
	}
	switch typ := uniqueConstraint.Typ.(type) {
	case *BuiltinFloatType:
		return &TcFloatLit{
			Source: arg.Source,
			Type:   typ,
			Value:  arg.Value,
		}
	case *ErrorType:
		// skipping rereporting here
		goto error
	}
	ReportUnexpectedType(sc, arg, expected, GetFloatLitType(arg.Value))
error:
	return &TcFloatLit{
		Source: arg.Source,
		Type:   TypeError,
		Value:  arg.Value,
	}
}

func SemCheckStrLit(sc *SemChecker, scope Scope, arg *StrLit, expected TypeConstraint) TcExpr {
	uniqueConstraint, isUniqueConstraint := expected.(UniqueTypeConstraint)
	if !isUniqueConstraint {
		ReportErrorf(sc, arg, "string literal needs to have a unique type constraint, but got '%s'", AstFormat(expected))
		return &TcStrLit{
			Source: arg.Source,
			Type:   TypeError,
			Value:  arg.Value,
		}
	}

	switch typ := uniqueConstraint.Typ.(type) {
	case *BuiltinStringType:
		if typ == TypeChar {
			runeCount := utf8.RuneCountInString(arg.Value)
			if runeCount != 1 {
				ReportErrorf(sc, arg, "char lit must have exactly one rune, but it has %d runes", runeCount)
				goto error
			}
		}
		return &TcStrLit{
			Source: arg.Source,
			Type:   typ,
			Value:  arg.Value,
		}
	case *ErrorType:
		goto error
	}
	ReportUnexpectedType(sc, arg, expected, GetStringLitType(arg.Value))
error:
	return &TcStrLit{
		Source: arg.Source,
		Type:   TypeError,
		Value:  arg.Value,
	}
}

func SemCheckExpr(sc *SemChecker, scope Scope, arg Expr, expected TypeConstraint) TcExpr {
	if (expected == UniqueTypeConstraint{TypeUntyped}) {
		return &TcWrappedUntypedAst{arg}
	}

	switch arg := arg.(type) {
	case *Call:
		return (TcExpr)(SemCheckCall(sc, scope, arg, expected))
	case *BracketExpr:
		newArgs := make([]Expr, 0, len(arg.Args)+1)
		newArgs = append(newArgs, arg.Callee)
		newArgs = append(newArgs, arg.Args...)
		call := &Call{Source: arg.Source, Callee: &Ident{Source: "indexOp"}, Args: newArgs}
		return (TcExpr)(SemCheckCall(sc, scope, call, expected))
	case *CodeBlock:
		return (TcExpr)(SemCheckCodeBlock(sc, scope, arg, expected))
	case *Ident:
		return (TcExpr)(LookUpLetSym(sc, scope, arg, expected))
	case *StrLit:
		return (TcExpr)(SemCheckStrLit(sc, scope, arg, expected))
	case *IntLit:
		return (TcExpr)(SemCheckIntLit(sc, scope, arg, expected))
	case *FloatLit:
		return (TcExpr)(SemCheckFloatLit(sc, scope, arg, expected))
	case *ReturnExpr:
		// ignoring expected type here, because the return as expression
		// never evaluates to anything
		return (TcExpr)(SemCheckReturnExpr(sc, scope, arg))
	case *TypeContext:
		return (TcExpr)(&TcTypeContext{
			Source:      arg.Source,
			WrappedType: LookUpType(sc, scope, arg.Expr),
		})
	case *VariableDefStmt:
		ExpectType(sc, arg, TypeVoid, expected)
		return (TcExpr)(SemCheckVariableDefStmt(sc, scope, arg))
	case *ForLoopStmt:
		ExpectType(sc, arg, TypeVoid, expected)
		return (TcExpr)(SemCheckForLoopStmt(sc, scope, arg))
	case *IfExpr:
		ExpectType(sc, arg, TypeVoid, expected)
		return (TcExpr)(SemCheckIfStmt(sc, scope, arg))
	case *IfElseExpr:
		return (TcExpr)(SemCheckIfElseStmt(sc, scope, arg, expected))
	case *ArrayLit:
		return (TcExpr)(SemCheckArrayLit(sc, scope, arg, expected))
	case *NilLit:
		return (TcExpr)(SemCheckNilLit(sc, scope, arg, expected))
	case *WhileLoopStmt:
		return (TcExpr)(SemCheckWhileLoopStmt(sc, scope, arg))
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

// TODO test that this actually work big big number types
var intLitTypeMap map[string]*IntLitType
var floatLitTypeMap map[string]*FloatLitType
var stringLitTypeMap map[string]*StringLitType

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
		registerBuiltin("indexOp", "", ".arr[", "]", []Type{result, TypeInt64}, elem, 0)

		argSym := &TcSymbol{Source: "_", Kind: SkProcArg, Type: result}
		registerSimpleTemplate("len", []*TcSymbol{argSym}, TypeInt64, &IntLit{Value: big.NewInt(len)})
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

func GetIntLitType(value *big.Int) (result *IntLitType) {
	// TODO test this
	strValue := value.String()
	result, ok := intLitTypeMap[strValue]
	if !ok {
		result = &IntLitType{Value: big.NewInt(0).Set(value)}
		intLitTypeMap[strValue] = result
	}
	return result
}

func GetFloatLitType(value *big.Float) (result *FloatLitType) {
	// TODO test this with nan values
	// TODO WARNING IMPORTANT CRITICAL this really needs to be tested if it actually works. I doubt it
	strValue := value.String()
	result, ok := floatLitTypeMap[strValue]
	if !ok {
		result = &FloatLitType{Value: big.NewFloat(0).Set(value)}
		floatLitTypeMap[strValue] = result
	}
	return result
}

func GetStringLitType(value string) (result *StringLitType) {
	result, ok := stringLitTypeMap[value]
	if !ok {
		result = &StringLitType{Value: value}
		stringLitTypeMap[value] = result
	}
	return result
}

func GetTypeType(typ Type) (result *TypeType) {
	result, ok := typeTypeMap[typ]
	//result, ok := ptrTypeMap[typ]
	if !ok {
		result = &TypeType{WrappedType: typ}
		typeTypeMap[typ] = result
	}
	return result
}

func SemCheckArrayLit(sc *SemChecker, scope Scope, arg *ArrayLit, expected TypeConstraint) TcExpr {
	switch exp := expected.(type) {
	case *UnspecifiedType:
		result := &TcArrayLit{}
		if len(arg.Items) == 0 {
			result.ElemType = TypeVoid
			return result
		}
		result.Items = make([]TcExpr, len(arg.Items))
		result.Items[0] = SemCheckExpr(sc, scope, arg.Items[0], TypeUnspecified)
		result.ElemType = result.Items[0].GetType()
		for i := 1; i < len(arg.Items); i++ {
			result.Items[i] = SemCheckExpr(sc, scope, arg.Items[i], UniqueTypeConstraint{result.ElemType})
		}
		return result
	case UniqueTypeConstraint:
		switch exp := exp.Typ.(type) {
		case *ArrayType:
			result := &TcArrayLit{}
			result.Items = make([]TcExpr, len(arg.Items))
			result.ElemType = exp.Elem
			for i, item := range arg.Items {
				result.Items[i] = SemCheckExpr(sc, scope, item, UniqueTypeConstraint{exp.Elem})
			}
			ExpectArgsLen(sc, arg, len(arg.Items), int(exp.Len))
			return result
		case *EnumSetType:
			result := &TcEnumSetLit{}
			result.Items = make([]TcExpr, len(arg.Items))
			result.ElemType = exp.Elem
			for i, item := range arg.Items {
				result.Items[i] = SemCheckExpr(sc, scope, item, UniqueTypeConstraint{exp.Elem})
			}
			return result
		case *StructType:
			result := &TcStructLit{}
			result.Items = make([]TcExpr, len(exp.Impl.Fields))
			result.Source = arg.Source
			result.Type = exp

			if len(arg.Items) == 0 {
				for i := range result.Items {
					result.Items[i] = exp.Impl.Fields[i].Type.DefaultValue(sc, arg)
				}
				return result
			} else if _, _, isAssign0 := MatchAssign(arg.Items[0]); isAssign0 {
				lastIdx := -1
				for _, it := range arg.Items {
					lhs, rhs, isAssign := MatchAssign(it)
					if !isAssign {
						panic(isAssign)
					}
					lhsIdent := lhs.(*Ident)
					field, idx := exp.Impl.GetField(lhsIdent.Source)
					if idx < 0 {
						ReportErrorf(sc, lhsIdent, "type %s has no field %s", exp.Impl.Name, lhsIdent.Source)
					} else {
						result.Items[idx] = SemCheckExpr(sc, scope, rhs, UniqueTypeConstraint{field.Type})
						if idx < lastIdx {
							ReportErrorf(sc, lhsIdent, "out of order initialization is not allowed (yet?)")
						}
					}
					lastIdx = idx
				}
				// fill up all unset fields with default values
				for i := range result.Items {
					if result.Items[i] == nil {
						result.Items[i] = exp.Impl.Fields[i].Type.DefaultValue(sc, arg)
					}
				}
				return result
			} else {
				// must have all fields of struct
				if len(arg.Items) != len(exp.Impl.Fields) {
					ReportErrorf(sc, arg, "literal has %d values, but %s needs %d values", len(arg.Items), exp.Impl.Name, len(exp.Impl.Fields))
				}
				for i := range result.Items {
					result.Items[i] = SemCheckExpr(sc, scope, arg.Items[i], UniqueTypeConstraint{exp.Impl.Fields[i].Type})
				}
				return result
			}
		case *BuiltinType:
			panic(fmt.Errorf("I don't know about type %s!", exp.Name))
		default:
			panic(fmt.Errorf("I don't know about type %T!", exp))
		}
	case *TypeGroup:
		// TODO no test yet
		ReportErrorf(sc, arg, "array literal needs to have a unique type constraint, but it has the following options: %s", AstFormat(exp))
		return newErrorNode(arg)
	default:
		panic(fmt.Errorf("I don't know about type %T!", exp))
	}
}

func SemCheckNilLit(sc *SemChecker, scope Scope, arg *NilLit, expected TypeConstraint) *NilLit {
	switch exp := expected.(type) {
	case UniqueTypeConstraint:
		if _, isPtrType := exp.Typ.(*PtrType); isPtrType {
			return &NilLit{Source: arg.Source, Type: exp.Typ}
		}
	case *UnspecifiedType:
		return &NilLit{Source: arg.Source, Type: TypeNilPtr}
	}
	ReportUnexpectedType(sc, arg, expected, TypeNilPtr)
	return &NilLit{Source: arg.Source, Type: TypeError}
}

func SemCheckIfStmt(sc *SemChecker, scope Scope, stmt *IfExpr) *TcIfStmt {
	// currently only iteration on strings in possible (of course that is not final)
	return &TcIfStmt{
		Source:    stmt.Source,
		Condition: SemCheckExpr(sc, scope, stmt.Condition, UniqueTypeConstraint{TypeBoolean}),
		Body:      SemCheckExpr(sc, scope, stmt.Body, UniqueTypeConstraint{TypeVoid}),
	}
}

func SemCheckIfElseStmt(sc *SemChecker, scope Scope, stmt *IfElseExpr, expected TypeConstraint) *TcIfElseExpr {
	// currently only iteration on strings in possible (of course that is not final)
	return &TcIfElseExpr{
		Source:    stmt.Source,
		Condition: SemCheckExpr(sc, scope, stmt.Condition, UniqueTypeConstraint{TypeBoolean}),
		Body:      SemCheckExpr(sc, scope, stmt.Body, expected),
		Else:      SemCheckExpr(sc, scope, stmt.Else, expected),
	}
}

// TODO ElementType should be some form of language feature
func (sc *SemChecker) ElementType(expr TcExpr) Type {
	switch typ := expr.GetType().(type) {
	case *ArrayType:
		return typ.Elem
	case *EnumSetType:
		return typ.Elem
	case *BuiltinStringType:
		if typ == TypeStr {
			return TypeChar
		}
	}
	ReportErrorf(sc, expr, "expect type with elements to iterate over")
	return TypeError
}

func SemCheckForLoopStmt(sc *SemChecker, scope Scope, loopArg *ForLoopStmt) *TcForLoopStmt {
	scope = NewSubScope(scope)
	// currently only iteration on strings in possible (of course that is not final)
	collection := SemCheckExpr(sc, scope, loopArg.Collection, TypeUnspecified)
	elementType := sc.ElementType(collection)
	return &TcForLoopStmt{
		Source:     loopArg.Source,
		Collection: collection,
		LoopSym:    scope.NewSymbol(sc, loopArg.LoopIdent, SkLoopIterator, elementType),
		Body:       SemCheckExpr(sc, scope, loopArg.Body, UniqueTypeConstraint{TypeVoid}),
	}
}

func SemCheckWhileLoopStmt(sc *SemChecker, scope Scope, loopArg *WhileLoopStmt) *TcWhileLoopStmt {
	scope = NewSubScope(scope)

	return &TcWhileLoopStmt{
		Source:    loopArg.Source,
		Condition: SemCheckExpr(sc, scope, loopArg.Condition, UniqueTypeConstraint{TypeBoolean}),
		Body:      SemCheckExpr(sc, scope, loopArg.Body, UniqueTypeConstraint{TypeVoid}),
	}
}

func SemCheckImportStmt(sc *SemChecker, importScope Scope, currentProgram *ProgramContext, workDir string, stmt *ImportStmt) *TcImportStmt {

	strLit, isStrLit := stmt.Expr.(*StrLit)
	if !isStrLit {
		ReportInvalidAstNode(sc, stmt.Expr, "string literal")
	}

	pkg, err := GetPackage(currentProgram, workDir, strLit.Value)
	if err != nil {
		ReportErrorf(sc, stmt.Expr, "%s", err.Error())
	} else {
		for key, value := range pkg.ExportScope.Variables {
			// TODO solution for conflicts and actually find out where the conflicting symbol comes from.
			_, hasKey := importScope.Variables[key]
			if hasKey {
				panic(fmt.Errorf("name conflicts in imported variable not yet implemented: %s.%s conflicts with some other symbol", strLit.Value, key))
			}
			importScope.Variables[key] = value
		}
		for key, value := range pkg.ExportScope.Types {
			_, hasKey := importScope.Types[key]
			if hasKey {
				panic(fmt.Errorf("name conflicts in imported type not yet implemented: %s.%s conflicts with some other symbol", strLit.Value, key))
			}
			importScope.Types[key] = value
		}
		for key, value := range pkg.ExportScope.Signatures {
			procs, _ := importScope.Signatures[key]
			importScope.Signatures[key] = append(procs, value...)
		}
	}
	return &TcImportStmt{Source: pkg.Source, Value: strLit, Package: pkg}
}

func SemCheckPackage(sc *SemChecker, currentProgram *ProgramContext, arg *PackageDef, mainPackage bool) (result *TcPackageDef) {
	result = &TcPackageDef{}
	importScope := NewSubScope(builtinScope)
	importScope.CurrentProgram = currentProgram
	pkgScope := NewSubScope(importScope)

	importScope.CurrentPackage = result //
	pkgScope.CurrentPackage = result
	result.Name = arg.Name

	var docComment *PrefixDocComment = nil
	for _, stmt := range arg.TopLevelStmts {
		if docComment != nil {
			stmt = sc.ApplyDocComment(stmt, docComment)
			docComment = nil
		}
		switch stmt := stmt.(type) {
		case *TypeDef:
			switch td := SemCheckTypeDef(sc, pkgScope, stmt).(type) {
			case *TcStructDef:
				result.StructDefs = append(result.StructDefs, td)
			case *TcEnumDef:
				result.EnumDefs = append(result.EnumDefs, td)
			default:
				panic("internal error")
			}
		case *ProcDef:
			procDef := SemCheckProcDef(sc, pkgScope, stmt)
			result.ProcDefs = append(result.ProcDefs, procDef)
			// TODO, verify compatible signature for main
			if mainPackage && procDef.Signature.Name == "main" {
				currentProgram.Main = procDef
			}
		case *VariableDefStmt:
			varDef := SemCheckVariableDefStmt(sc, pkgScope, stmt)
			result.VarDefs = append(result.VarDefs, varDef)
		case *PrefixDocComment:
			docComment = stmt
		case *EmitStmt:
			switch expr := stmt.Expr.(type) {
			case *StrLit:
				result.EmitStatements = append(result.EmitStatements, expr)
			default:
				ReportInvalidAstNode(sc, expr, "string literal")
			}
		case *StaticExpr:
			// TODO: ensure this expression can be evaluated at compile time
			tcExpr := SemCheckExpr(sc, pkgScope, stmt.Expr, UniqueTypeConstraint{TypeVoid})
			EvalExpr(sc, tcExpr, pkgScope)
		case *ImportStmt:
			tcImportStmt := SemCheckImportStmt(sc, importScope, currentProgram, arg.WorkDir, stmt)
			result.Imports = append(result.Imports, tcImportStmt)
		case *TraitDef:
			traitDef := SemCheckTraitDef(sc, pkgScope, stmt)
			// stmt.Name
			result.TraitDefs = append(result.TraitDefs, traitDef)
		default:
			panic(fmt.Errorf("internal error: %T", stmt))
		}
	}
	if mainPackage && currentProgram.Main == nil {
		ReportErrorf(sc, arg, "package '%s' misses main proc", result.Name)
	}

	// TODO: only export what actually wants to be exported
	exportScope := NewSubScope(builtinScope)
	exportScope.CurrentProgram = currentProgram
	*exportScope = *pkgScope
	exportScope.Parent = builtinScope
	result.ExportScope = exportScope

	return result
}
