package main

import (
	"fmt"
	"math/big"
	"path/filepath"
	"strings"
	"unicode/utf8"
)

type CompileError struct {
	node Expr
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
	CurrentProgram       *ProgramContext
	CurrentPackage       *TcPackageDef
	CurrentProcSignature *Signature // used for `return` statements
	CurrentTrait         *TcTraitDef
	Variables            map[string]*TcSymbol
	Overloadables        map[string][]Overloadable
	Types                map[string]Type
	TypeConstraints      map[string]TypeConstraint
}

type Scope = *ScopeImpl

func NewSubScope(scope Scope) Scope {
	return &ScopeImpl{
		Parent:               scope,
		CurrentProgram:       scope.CurrentProgram,
		CurrentPackage:       scope.CurrentPackage,
		CurrentProcSignature: scope.CurrentProcSignature,
		// TODO maybe do lazy initialization of some of these? Traits are rarely addad.
		Variables:       make(map[string]*TcSymbol),
		Overloadables:   make(map[string][]Overloadable),
		Types:           make(map[string]Type),
		TypeConstraints: make(map[string]TypeConstraint),
	}
}

func RegisterType(sc *SemChecker, scope Scope, name string, typ Type, context Expr) {
	if _, ok := scope.Types[name]; ok {
		ReportErrorf(sc, context, "double definition of type '%s'", name)
		// TODO previously defined at ...
		return
	}
	scope.Types[name] = typ
}

func RegisterNamedType(sc *SemChecker, scope Scope, typ NamedType, context Expr) {
	RegisterType(sc, scope, typ.GetName(), typ, context)
}

func RegisterTypeGroup(sc *SemChecker, scope Scope, tg *TypeGroup, context Expr) TypeConstraint {
	if _, ok := builtinScope.TypeConstraints[tg.Name]; ok {
		ReportErrorf(sc, context, "double definition of type group %s", AstFormat(tg))
	}
	builtinScope.TypeConstraints[tg.Name] = tg
	return tg
}

func RegisterTrait(sc *SemChecker, scope Scope, trait *TcTraitDef, context Expr) TypeConstraint {
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

func SignatureEquals(lhs, rhs *Signature) bool {
	if len(lhs.Params) != len(rhs.Params) {
		return false
	}
	for i := range lhs.Params {
		typ1 := lhs.Params[i].Type
		typ2 := rhs.Params[i].Type
		if _, isGeneric := typ1.(*GenericTypeSymbol); isGeneric {
			continue
		}
		if _, isGeneric := typ2.(*GenericTypeSymbol); isGeneric {
			continue
		}
		if typ1 != typ2 {
			return false
		}
	}
	return true
}

func RegisterProc(sc *SemChecker, scope Scope, proc Overloadable, context Expr) {
	sig := proc.GetSignature()
	name := sig.Name
	// TODO check for name collisions with the same signature
	overloadables := scope.Overloadables[name]
	for _, it := range overloadables {
		if SignatureEquals(it.GetSignature(), sig) {
			ReportErrorf(sc, proc, "definition with colliding signature: %s", AstFormat(sig))
			ReportErrorf(sc, it, "previous definition here")
		}
	}
	scope.Overloadables[name] = append(overloadables, proc)
}

func (scope Scope) NewSymbol(sc *SemChecker, name *Ident, kind SymbolKind, typ Type) *TcSymbol {
	//result := TcSymbol{Name: name.source, Kind: kind, Typ: typ}
	rawName := name.Source
	result := &TcSymbol{Source: rawName, Kind: kind, Value: nil, Type: typ, Comment: name.Comment}
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

func LookUpType(sc *SemChecker, scope Scope, expr Expr) Type {
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

func LookUpProc(scope Scope, name string, numArgs int, overloadables []Overloadable) []Overloadable {
	for scope != nil {
		if localOverloadables, ok := scope.Overloadables[name]; ok {
			if numArgs >= 0 { // num args may be set to -1 to get all signatures
				for _, overloadable := range localOverloadables {
					sig := overloadable.GetSignature()
					// TODO, the varargs logic herer needs a proper test
					if len(sig.Params) == numArgs || sig.Varargs != nil && len(sig.Params) <= numArgs {
						overloadables = append(overloadables, overloadable)
					}
				}
			} else {
				overloadables = append(overloadables, localOverloadables...)
			}

		}
		scope = scope.Parent
	}
	return overloadables
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

func ParseTraitDef(sc *SemChecker, expr Expr) (name *Ident, dependentTypes []*Ident, signatures []*Call) {
	if lhs, rhs, isAssign := MatchAssign(expr); isAssign {
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
				if procDef, isProcDef := it.(*Call); isProcDef && procDef.Command && procDef.Callee.GetSource() == "proc" {
					signatures = append(signatures, procDef)
				} else {
					ReportErrorf(sc, it, "expect proc def here")
				}
			}
		} else {
			ReportErrorf(sc, rhs, "expect code block here")
		}
	} else {
		ReportErrorf(sc, expr, "expect assignment")
	}

	return
}

func NewProcPrototype(sc *SemChecker, sig *Signature) *TcProcDef {
	// is this enough?
	//
	// this prototypes may not actually be uned in later compilation stages, it is
	// only used, that the body of generic procedurs may pass the semchecking
	// phase.
	return &TcProcDef{
		Signature: *sig,
	}
}

// TODO, it is a code smell that sem check signature actively injects its
// symbols into the body scope instead of just focussing on the signature.
func SemCheckSignature(sc *SemChecker, bodyScope Scope, name string, genericArgs []GenericArgument, args []ProcArgument, resultType Expr) *Signature {
	var genericParams []*GenericTypeSymbol

	parentScope := bodyScope.Parent
	signatureScope := NewSubScope(parentScope)

	if parentScope.CurrentTrait != nil {
		genericParams = append(genericParams, parentScope.CurrentTrait.DependentTypes...)
	}

	for _, genericArg := range genericArgs {
		constraint := LookUpTypeConstraint(sc, parentScope, genericArg.TraitName)
		if (constraint == UniqueTypeConstraint{TypeError}) {
			continue
		}
		name := genericArg.Name

		genTypeSym := NewGenericTypeSymbol(name.Source, name.Source, constraint)
		abstractTypeSym := NewAbstractTypeSymbol(name.Source)
		genTypeSym.AbsTypSym = abstractTypeSym
		genericParams = append(genericParams, genTypeSym)
		// make the generic type symbol available to look up in its body
		RegisterType(sc, bodyScope, name.Source, abstractTypeSym, name)
		RegisterType(sc, signatureScope, name.Source, genTypeSym, name)

		switch constraint := constraint.(type) {
		case *TypeTrait:
			traitDef := constraint.Impl
			if len(traitDef.DependentTypes) != 1 {
				ReportErrorf(sc, name, "only constraints with a single argument implemented")
			}
			sym := traitDef.DependentTypes[0]
			subs := &Substitutions{
				typeSubs: []TypeSubstitution{{sym, abstractTypeSym}},
			}
			traitInst := &TraitInstance{}
			genTypeSym.TraitInst = traitInst
			//cacheKey := ComputeInstanceCacheKey(traitDef.DependentTypes, subs.typeSubs)
			//traitDef.InstanceCache.Set(cacheKey, traitInst)
			for _, sig := range traitDef.Signatures {
				newSig, _ := SignatureApplyTypeSubstitution(sig, subs)
				// fmt.Printf("i: %d\nold: %s\nnew: %s\n", j, AstFormat(&sig), AstFormat(&newSig))
				// traitDef.InstanceCache.Init(len(traitDef.DependentTypes))
				overloadable := NewProcPrototype(sc, &newSig)
				traitInst.ProcDefs = append(traitInst.ProcDefs, overloadable)
				RegisterProc(sc, bodyScope, overloadable, genericArg.TraitName)
			}
		default:
			ReportWarningf(sc, genericArg.TraitName, "currently only traits are supported in the compiler as type constraints")
		}
	}

	var params []*TcSymbol
	var paramsForBody []*TcSymbol
	for _, arg := range args {
		symKind := SkProcArg
		if arg.Mutable {
			symKind = SkVarProcArg
		}

		// the same, unless a generac parameter
		sigTyp := LookUpType(sc, signatureScope, arg.Type)
		var bodyTyp Type
		// prevent double reporting of error messages
		if sigTyp != TypeError {
			bodyTyp = LookUpType(sc, bodyScope, arg.Type)
		} else {
			bodyTyp = TypeError
		}
		var sigParam *TcSymbol
		var bodyParam *TcSymbol
		// TODO this is ugly. Refactoring `NewSymbol` to a simple `RegisterSymbol`
		// might be a better solution.
		if arg.Name.Source != "_" {
			ValidNameCheck(sc, arg.Name, "proc arg")
			sigParam = signatureScope.NewSymbol(sc, arg.Name, symKind, sigTyp)
			bodyParam = bodyScope.NewSymbol(sc, arg.Name, symKind, bodyTyp)
		} else {
			// parameters with the name "_" are explicity not put in the scope.
			sigParam = &TcSymbol{
				Source: arg.Name.Source,
				Kind:   symKind,
				Value:  nil,
				Type:   sigTyp,
			}
			bodyParam = &TcSymbol{
				Source: arg.Name.Source,
				Kind:   symKind,
				Value:  nil,
				Type:   sigTyp,
			}
		}
		params = append(params, sigParam)
		paramsForBody = append(paramsForBody, bodyParam)
	}

	signature := &Signature{
		Name:          name,
		GenericParams: genericParams,
		Params:        params,
		ParamsForBody: paramsForBody,
	}

	if resultType == nil {
		ReportErrorf(sc, &Ident{Source: name}, "proc def needs result type specified")
		signature.ResultType = TypeError
		signature.ResultTypeForBody = TypeError
	} else {
		signature.ResultType = LookUpType(sc, signatureScope, resultType)
		if signature.ResultType != TypeError {
			signature.ResultTypeForBody = LookUpType(sc, bodyScope, resultType)
		} else {
			signature.ResultTypeForBody = TypeError
		}
	}

	return signature
}

func MangleSignature(signature *Signature) string {
	mangledNameBuilder := &strings.Builder{}
	mangledNameBuilder.WriteString(signature.Name)
	mangledNameBuilder.WriteRune('_')
	for _, arg := range signature.Params {
		arg.Type.ManglePrint(mangledNameBuilder)
	}
	return mangledNameBuilder.String()
}

func ReportMessagef(sc *SemChecker, node Expr, kind string, msg string, args ...interface{}) {
	newMsg := fmt.Sprintf(msg, args...)
	sc.errors = append(sc.errors, CompileError{node: node, msg: newMsg})
	if !sc.silentErrors {
		if node == nil {
			fmt.Println(msg)
		} else {
			line, columnStart, columnEnd := LineColumnStr(sc.code, node.GetSource())
			if line < 0 {
				fmt.Printf("%s %s: %s\n", sc.filename, kind, newMsg)
			} else if columnEnd < 0 {
				fmt.Printf("%s(%d, %d) %s: %s\n", sc.filename, line, columnStart, kind, newMsg)
			} else {
				fmt.Printf("%s(%d, %d-%d) %s: %s\n", sc.filename, line, columnStart, columnEnd, kind, newMsg)
			}
		}
	}
}

func ReportErrorf(sc *SemChecker, node Expr, msg string, args ...interface{}) {
	ReportMessagef(sc, node, "Error", msg, args...)
}

func ReportWarningf(sc *SemChecker, node Expr, msg string, args ...interface{}) {
	ReportMessagef(sc, node, "Warning", msg, args...)
}

func ReportUnexpectedType(sc *SemChecker, context Expr, expected TypeConstraint, gotten Type) {
	ReportErrorf(sc, context, "expected type '%s' but got type '%s'", AstFormat(expected), AstFormat(gotten))
}

func ReportInvalidDocCommentKey(sc *SemChecker, section *NamedDocSection) {
	ReportErrorf(sc, section, "invalid doc comment key: %s", section.Name)
}

func ReportInvalidAnnotations(sc *SemChecker, lit *StrLit) {
	ReportErrorf(sc, lit, "invalid annotations string: %s", lit.Value)
}

func ReportInvalidAstNode(sc *SemChecker, node Expr, expected string) {
	ReportErrorf(sc, node, "invalid ast node. Expected %s, but got %T", expected, node)
}

func ReportIllegalDocComment(sc *SemChecker, doc *PrefixDocComment) {
	ReportErrorf(sc, doc, "doc comment is illegal here, use normal comment instead")
}

func ReportMustBeMutable(sc *SemChecker, doc Expr) {
	ReportErrorf(sc, doc, "expression must be mutable")
}

func ExpectType(sc *SemChecker, node Expr, gotten Type, expected TypeConstraint) Type {
	// TODO this doesn't work for partial types (e.g. array[<unspecified>])
	// TODO this doesn't work for TypeTrait
	if gotten == TypeError {
		return TypeError
	}
	if gotten == TypeNoReturn {
		// TypeNoReturn is the wildcard for every constraint here
		return TypeNoReturn
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
	case *TypeTrait:
		// fmt.Printf("got type trait: %s\n", AstFormat(expected))
		// fmt.Println(AstFormat(expected.Impl))
		return gotten
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

func errorProcSym(ident *Ident) *TcProcRef {
	procDef := &TcErrorProcDef{
		Signature: Signature{Name: ident.Source, ResultType: TypeError},
	}

	return &TcProcRef{
		Source: ident.GetSource(),
		// maybe add some debug information here
		Overloadable: procDef,
	}
}

type TypeGroupBuilder TypeGroup

func AppendNoDuplicats[T comparable](types []T, typ T) (result []T) {
	for _, it := range types {
		if it == typ {
			return types
		}
	}
	return append(types, typ)
}

func argTypeGroupAtIndex(signatures []Signature, idx int) (result TypeConstraint) {
	builder := &TypeGroupBuilder{}
	for _, sig := range signatures {
		if sig.Varargs != nil && idx >= len(sig.Params) {
			return sig.Varargs
		}
		typ := sig.Params[idx].Type

		if sym, isSym := typ.(*GenericTypeSymbol); isSym {
			if len(signatures) == 1 {
				return sym.Constraint
			}
		}

		if len(openGenericsMap[typ]) > 0 {
			// TODO this can be more precise than the most generic `TypeUnspecified`
			return TypeUnspecified
		}
		builder.Items = AppendNoDuplicats(builder.Items, typ)
	}
	if len(builder.Items) == 1 {

		typ := builder.Items[0]
		if signatures[0].Name == "pointless" {
			fmt.Printf("%s %T\n", AstFormat(typ), typ)
		}

		return UniqueTypeConstraint{builder.Items[0]}
	}
	if len(builder.Items) == 0 {
		return TypeUnspecified
	}
	return (*TypeGroup)(builder)
}

type ProcSubstitution = struct {
	sig    Overloadable
	newSig Overloadable
}

type SymbolSubstitution = struct {
	sym    *TcSymbol
	newSym *TcSymbol
}

type TypeSubstitution = struct {
	sym     Type
	newType Type
}

func GenericParamSignatureMatch(scope Scope, exprType, paramType Type, substitutions *Substitutions) (ok bool, outSubstitutions *Substitutions) {
	if typeSym, isTypeSym := paramType.(*GenericTypeSymbol); isTypeSym {
		// check if the type is somehow in the substitutions list
		for _, sub := range substitutions.typeSubs {
			if typeSym == sub.sym {
				if sub.sym == exprType {
					return true, substitutions
				} else {
					return false, nil
				}
			}
		}

		outSubstitutions = substitutions // TODO is this correct? It modifies input substitutions
		outSubstitutions.typeSubs = append(outSubstitutions.typeSubs,
			TypeSubstitution{typeSym, exprType},
			TypeSubstitution{typeSym.AbsTypSym, exprType},
		)

		switch constraint := typeSym.Constraint.(type) {
		case UniqueTypeConstraint:
			if constraint.Typ == exprType {
				return true, outSubstitutions
			}
			return false, nil
		case *TypeGroup:
			for _, it := range constraint.Items {
				if it == exprType {
					return true, outSubstitutions
				}
			}
			return false, nil
		case *TypeTrait:
			// fmt.Printf("dependent types: [")
			// for i, typ := range constraint.Impl.DependentTypes {
			// 	if i != 0 {
			// 		fmt.Print(", ")
			// 	}
			// 	fmt.Print(typ.Name)
			// }
			// fmt.Printf("]\n")
			// fmt.Printf("expr type: %s\n", AstFormat(exprType))
			// fmt.Printf("signatures:\n")
			// for _, sig := range constraint.Impl.Signatures {
			// 	fmt.Printf("  %s\n", AstFormat(&sig))
			// }

			if len(constraint.Impl.DependentTypes) != 1 {
				panic("not implemented")
			}
			traitInst := typeSym.TraitInst
			// fmt.Printf("proc defs:\n")
			// for _, def := range traitInst.ProcDefs {
			// 	fmt.Printf("   %s\n", AstFormat(def))
			// }
			//
			// trait CanDoPointlessStuff(U) = {
			//   proc pointlessStuff(_: U): void
			// }

			// to convert the signatures listed in the trait to the current usage, we
			// create a substitutions object here. in the example it would be U -> f32

			typeSym := constraint.Impl.DependentTypes[0]

			traitSubs := &Substitutions{
				typeSubs: []TypeSubstitution{
					{typeSym, exprType},
				},
			}
			// fmt.Printf("traitSubs: %s\n", AstFormat(traitSubs))
			var procSubs []ProcSubstitution
			for i, sig := range constraint.Impl.Signatures {
				// sig :: pointlessStuff(U): void
				newSig, _ := SignatureApplyTypeSubstitution(sig, traitSubs) // pointlessStuff(f32): void
				// fmt.Printf("traitSubs: %s\n", AstFormat(traitSubs))
				// fmt.Printf("sig: %s\nnewsig: %s\n", AstFormat(&sig), AstFormat(&newSig))
				candidates := LookUpProc(scope, newSig.Name, -1, nil)
				var substitutionProc Overloadable
			candidatesLoop:
				for _, it := range candidates {

					itSig := it.GetSignature()
					for i := range newSig.Params {
						// TODO this needs to be generic parameter signature matching
						if itSig.Params[i].Type != newSig.Params[i].Type {
							continue candidatesLoop
						}
					}
					// this doesn't work for any generic type
					substitutionProc = it
					break candidatesLoop
				}

				if substitutionProc == nil {
					return false, nil
				}

				traitProc := traitInst.ProcDefs[i]
				//fmt.Printf("subst: %s\nwith: %s\n", AstFormat(traitProc), AstFormat(substitutionProc))
				procSubs = append(procSubs, ProcSubstitution{traitProc, substitutionProc})
			}

			outSubstitutions.procSubs = append(outSubstitutions.procSubs, procSubs...)
			return true, outSubstitutions

		case *UnspecifiedType:
			return true, outSubstitutions
		}
	}

	{
		exprArrType, exprIsArrType := exprType.(*ArrayType)
		paramArrType, paramIsArrType := paramType.(*ArrayType)
		if exprIsArrType && paramIsArrType {
			if exprArrType.Len != paramArrType.Len {
				return false, nil
			}
			return GenericParamSignatureMatch(scope, exprArrType.Elem, paramArrType.Elem, substitutions)
		}
	}

	{
		exprPtrType, exprIsPtrType := exprType.(*PtrType)
		paramPtrType, paramIsPtrType := paramType.(*PtrType)
		if exprIsPtrType && paramIsPtrType {
			return GenericParamSignatureMatch(scope, exprPtrType.Target, paramPtrType.Target, substitutions)
		}
	}

	{
		exprEnumSetType, exprIsEnumSetType := exprType.(*EnumSetType)
		paramEnumSetType, paramIsEnumSetType := paramType.(*EnumSetType) // TODO, this line is untested
		if exprIsEnumSetType && paramIsEnumSetType {
			return GenericParamSignatureMatch(scope, exprEnumSetType.Elem, paramEnumSetType.Elem, substitutions)
		}
	}

	{
		exprTypeType, exprIsTypeType := exprType.(*TypeType)
		paramTypeType, paramIsTypeType := paramType.(*TypeType)
		if exprIsTypeType && paramIsTypeType {
			return GenericParamSignatureMatch(scope, exprTypeType.WrappedType, paramTypeType.WrappedType, substitutions)
		}
	}

	{
		exprTypeSym, exprIsAbstractType := exprType.(*AbstractTypeSymbol)
		paramTypeSym, paramIsAbstractType := paramType.(*AbstractTypeSymbol)
		if exprIsAbstractType && paramIsAbstractType {
			return exprTypeSym == paramTypeSym, substitutions
		}
	}

	// TODO apply type substitutions to struct type
	return false, nil
}

func ParamSignatureMatch(scope Scope, exprType, paramType Type) (ok bool, typeSubs []TypeSubstitution, procSubs []ProcSubstitution) {
	if len(openGenericsMap[paramType]) > 0 {
		ok, result := GenericParamSignatureMatch(scope, exprType, paramType, &Substitutions{})
		if ok {
			return true, result.typeSubs, result.procSubs
		} else {
			return false, nil, nil
		}
	}
	// fast non recursive pass
	return exprType == paramType, nil, nil
}

func RecursiveTypeSubstitution(typ Type, substitutions []TypeSubstitution) Type {
	switch typ := typ.(type) {
	case *GenericTypeSymbol:
		for _, it := range substitutions {
			if it.sym == typ {
				return it.newType
			}
		}
		return typ
	case *AbstractTypeSymbol:
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
	default:
		panic("internal error")
	}
}

func Contains(theSet []Type, item Type) bool {
	for _, it := range theSet {
		if it == item {
			return true
		}
	}
	return false
}

func Contains2(theSet []TypeSubstitution, item Type) bool {
	for _, it := range theSet {
		if it.sym == item {
			return true
		}
	}
	return false
}

func ApplyProcSubstitutions(ref *TcProcRef, subs []ProcSubstitution) *TcProcRef {
	for _, sub := range subs {
		if ref.Overloadable == sub.sig {
			return &TcProcRef{
				ref.Source,
				sub.newSig,
			}
		}
	}
	return ref
}

func ApplySymbolSubstitutions(sym *TcSymbol, subs []SymbolSubstitution) *TcSymbol {
	for _, sub := range subs {
		if sym == sub.sym {
			return sub.newSym
		}
	}
	return sym
}

func ApplyTypeSubstitutions(argType Type, substitutions []TypeSubstitution) Type {
	openSymbols := openGenericsMap[argType]
	if len(openSymbols) == 0 {
		// not a type with open generic symbols. nothing to substitute here
		return argType
	}

	// all substitutions that are actively part of the open symbols
	var filteredSubstitutions []TypeSubstitution
	for _, sub := range substitutions {
		if Contains(openSymbols, sub.sym) {
			filteredSubstitutions = append(filteredSubstitutions, sub)
		}
	}

	if len(filteredSubstitutions) == 0 {
		// substitutions do not apply to this type, nothing to do here
		return argType
	}

	// compute new open symbols
	var newOpenSymbols []Type = nil
	for _, sym := range openSymbols {
		if !Contains2(filteredSubstitutions, sym) {
			newOpenSymbols = append(newOpenSymbols, sym)
		}
	}
	return RecursiveTypeSubstitution(argType, filteredSubstitutions)
}

func FindSubstitution(substitutions []TypeSubstitution, sym *GenericTypeSymbol) (newTypo Type, ok bool) {
	for _, sub := range substitutions {
		if sym == sub.sym {
			return sub.newType, true
		}
	}
	return nil, false
}

// TODO this function modifies the input argument `subs`, and appends to it new
// symbol substitutions that should be applied. This is really bad code smell in
// probably the reason for bugs.
func SignatureApplyTypeSubstitution(sig Signature, subs *Substitutions) (Signature, []SymbolSubstitution) {
	if len(subs.typeSubs) == 0 {
		return sig, nil
	}

	newParams := make([]*TcSymbol, len(sig.Params))
	symSubs := make([]SymbolSubstitution, 0)
	for j, param := range sig.Params {
		newType := ApplyTypeSubstitutions(param.Type, subs.typeSubs)
		if newType != param.Type {
			newSym := &TcSymbol{
				Source: param.Source,
				Kind:   param.Kind,
				Value:  param.Value, // only set when symbol is `const`
				Type:   newType,
			}
			if len(sig.ParamsForBody) > j {
				symSubs = append(subs.symSubs, SymbolSubstitution{sig.ParamsForBody[j], newSym})
			} else {
				symSubs = append(subs.symSubs, SymbolSubstitution{param, newSym})
			}
			newParams[j] = newSym
		} else {
			newParams[j] = param
		}
	}

	newGen := make([]*GenericTypeSymbol, 0, len(sig.GenericParams))
	for _, it := range sig.GenericParams {
		if newType, foundNewType := FindSubstitution(subs.typeSubs, it); foundNewType {
			openGenericSyms := openGenericsMap[newType]
			for _, openSym := range openGenericSyms {
				if genSym, isGenSym := openSym.(*GenericTypeSymbol); isGenSym {
					newGen = AppendNoDuplicats(newGen, genSym)
				}
			}
		} else {
			// generic parameter isn't substituted. keep it unchanged
			newGen = AppendNoDuplicats(newGen, it)
		}
	}

	result := Signature{
		Name:          sig.Name,
		GenericParams: newGen,
		Params:        newParams,
		ParamsForBody: sig.ParamsForBody,
		ResultType:    ApplyTypeSubstitutions(sig.ResultType, subs.typeSubs),
		Varargs:       sig.Varargs,
	}

	return result, symSubs
}

type InstanceCache[T any] struct {
	Map any
}

func (this *InstanceCache[T]) Debug() string {
	builder := &strings.Builder{}
	switch m := this.Map.(type) {
	case map[[1]Type]T:
		for key, value := range m {
			fmt.Fprintf(builder, "[%s] -> %#+v\n",
				AstFormat(key[0]),
				value,
			)
		}
	case map[[2]Type]T:
		for key, value := range m {
			fmt.Fprintf(builder, "[%s, %s] -> %#+v\n",
				AstFormat(key[0]),
				AstFormat(key[1]),
				value,
			)
		}
	case map[[3]Type]T:
		for key, value := range m {
			fmt.Fprintf(builder, "[%s, %s, %s] -> %v\n",
				AstFormat(key[0]),
				AstFormat(key[1]),
				AstFormat(key[2]),
				value,
			)
		}
	case map[[4]Type]T:
		for key, value := range m {
			fmt.Fprintf(builder, "[%s, %s, %s, %s] -> %#+v\n",
				AstFormat(key[0]),
				AstFormat(key[1]),
				AstFormat(key[2]),
				AstFormat(key[3]),
				value,
			)
		}
	case map[[5]Type]T:
		for key, value := range m {
			fmt.Fprintf(builder, "[%s, %s, %s, %s, %s] -> %#+v\n",
				AstFormat(key[0]),
				AstFormat(key[1]),
				AstFormat(key[2]),
				AstFormat(key[3]),
				AstFormat(key[4]),
				value,
			)
		}
	case map[[6]Type]T:
		for key, value := range m {

			fmt.Fprintf(builder, "[%s, %s, %s, %s, %s, %s] -> %#+v\n",
				AstFormat(key[0]),
				AstFormat(key[1]),
				AstFormat(key[2]),
				AstFormat(key[3]),
				AstFormat(key[4]),
				AstFormat(key[5]),
				value,
			)
		}

	default:
		panic("not implemented")
	}

	return builder.String()
}

func (this *InstanceCache[T]) Init(N int) {
	switch N {
	case 1:
		this.Map = make(map[[1]Type]T)
	case 2:
		this.Map = make(map[[2]Type]T)
	case 3:
		this.Map = make(map[[3]Type]T)
	case 4:
		this.Map = make(map[[4]Type]T)
	case 5:
		this.Map = make(map[[5]Type]T)
	case 6:
		this.Map = make(map[[6]Type]T)
	default:
		panic(fmt.Errorf("not implemented %d", N))
	}
}

func NewInstanceCache[T any](N int) (result *InstanceCache[T]) {
	if N > 0 {
		result = &InstanceCache[T]{}
		result.Init(N)
	}
	return result
}

func (this *InstanceCache[T]) Set(key []Type, value T) {
	switch len(key) {
	case 1:
		key2 := [...]Type{key[0]}
		this.Map.(map[[1]Type]T)[key2] = value
	case 2:
		key2 := [...]Type{key[0], key[1]}
		this.Map.(map[[2]Type]T)[key2] = value
	case 3:
		key2 := [...]Type{key[0], key[1], key[2]}
		this.Map.(map[[3]Type]T)[key2] = value
	case 4:
		key2 := [...]Type{key[0], key[1], key[2], key[3]}
		this.Map.(map[[4]Type]T)[key2] = value
	case 5:
		key2 := [...]Type{key[0], key[1], key[2], key[3], key[4]}
		this.Map.(map[[5]Type]T)[key2] = value
	case 6:
		key2 := [...]Type{key[0], key[1], key[2], key[3], key[4], key[5]}
		this.Map.(map[[6]Type]T)[key2] = value
	default:
		panic(fmt.Errorf("not implemented %d", key))
	}
}

func (this *InstanceCache[T]) LookUp(key []Type) T {
	switch len(key) {
	case 1:
		key2 := [...]Type{key[0]}
		return this.Map.(map[[1]Type]T)[key2]
	case 2:
		key2 := [...]Type{key[0], key[1]}
		return this.Map.(map[[2]Type]T)[key2]
	case 3:
		key2 := [...]Type{key[0], key[1], key[2]}
		return this.Map.(map[[3]Type]T)[key2]
	case 4:
		key2 := [...]Type{key[0], key[1], key[2], key[3]}
		return this.Map.(map[[4]Type]T)[key2]
	case 5:
		key2 := [...]Type{key[0], key[1], key[2], key[3], key[4]}
		return this.Map.(map[[5]Type]T)[key2]
	case 6:
		key2 := [...]Type{key[0], key[1], key[2], key[3], key[4], key[5]}
		return this.Map.(map[[6]Type]T)[key2]
	default:
		panic(fmt.Errorf("not implemented %d", len(key)))
	}
}

func ComputeInstanceCacheKey(genericParams []*GenericTypeSymbol, typeSubs []TypeSubstitution) []Type {
	cacheKey := make([]Type, len(genericParams))
genericParams:
	for i, sym := range genericParams {
		for _, sub := range typeSubs {
			if sym == sub.sym {
				cacheKey[i] = sub.newType
				continue genericParams
			}
		}
		panic(fmt.Errorf("symbol has no substitudion: %s", AstFormat(sym)))
	}
	return cacheKey
}

func SemCheckCall(sc *SemChecker, scope Scope, call *Call, expected TypeConstraint) TcExpr {
	ident, isIdent := call.Callee.(*Ident)
	if !isIdent {
		ReportErrorf(sc, call.Callee, "expected identifier but got %T (%s)", call.Callee, AstFormat(call.Callee))
		return newErrorNode(call.Callee)
	}

	overloadables := LookUpProc(scope, ident.Source, len(call.Args), nil)
	signatures := make([]Signature, len(overloadables))
	sigSubstitutions := make([]Substitutions, len(overloadables))

	// copy of the signature matches, because they are mutated is signature
	// matching
	for i, it := range overloadables {
		signatures[i] = *it.GetSignature()
	}

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
			overloadables = nil
			signatures = nil
			sigSubstitutions = nil
			continue
		}

		// in place filter procedures for compatilbes
		n := 0
		for j, sig := range signatures {
			subs := sigSubstitutions[j]
			overloadable := overloadables[j]

			if sig.Varargs != nil && i >= len(sig.Params) {
				overloadables[n] = overloadable
				signatures[n] = sig
				sigSubstitutions[n] = subs
				n++
				continue
			}

			typ := sig.Params[i].Type
			if ok, typeSubs, procSubs := ParamSignatureMatch(scope, argType, typ); ok {
				// instantiate generic
				subs.typeSubs = append(subs.typeSubs, typeSubs...)
				subs.procSubs = append(subs.procSubs, procSubs...)
				overloadables[n] = overloadable
				signatures[n], _ = SignatureApplyTypeSubstitution(sig, &subs)
				sigSubstitutions[n] = subs
				n++
				continue
			}
		}
		overloadables = overloadables[:n]
		signatures = signatures[:n]
		sigSubstitutions = sigSubstitutions[:n]
	}

	result := &TcCall{Source: call.Source, Braced: call.Braced}

	switch len(overloadables) {
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
			overloadables = LookUpProc(scope, ident.Source, -1, nil)
			if len(overloadables) > 0 {
				builder.NewlineAndIndent()
				builder.WriteString("available overloads: ")
				for _, overloadable := range overloadables {
					builder.NewlineAndIndent()
					builder.WriteString("proc ")
					builder.WriteNode(overloadable.GetSignature())
				}
			}

			ReportErrorf(sc, call, "%s", builder.String())
		}
		result.Sym = errorProcSym(ident)
		result.Args = checkedArgs

	case 1:
		sig := signatures[0]
		// some sanity checks. Technically this part is unnecessary if the compiler
		// has no bugs. And if it has bugs. This test is also incomplete.
		//
		// What is done here is to check that there are no Open generic types left anymore in non-generic code.
		// In the body of a generic function, there could be a better sanity check here.
		if scope.CurrentProcSignature == nil || len(scope.CurrentProcSignature.GenericParams) == 0 {
			for i, arg := range sig.Params {
				openGenericSymbols := openGenericsMap[arg.Type]
				if len(openGenericSymbols) > 0 {
					ReportErrorf(sc, checkedArgs[i], "generics arguments are expected to instanciated %s", AstFormat(checkedArgs[i].GetType()))
					panic(
						fmt.Sprintf(
							"internal error: generics arguments are expected to instanciated\n%s\n",
							AstFormat(call),
						),
					)
				}

				if arg.Kind == SkVarProcArg {
					RequireMutable(sc, checkedArgs[i])
				}
			}
			if len(openGenericsMap[sig.ResultType]) > 0 {
				panic("internal error: generics are expected to instanciated")
			}
		}

		switch impl := overloadables[0].(type) {
		case *TcProcDef:
			instance := InstanciateGenericProc(impl, &sigSubstitutions[0])
			result.Sym = &TcProcRef{Source: ident.Source, Overloadable: instance}
			result.Args = checkedArgs
			ExpectType(sc, call, sig.ResultType, expected)

			// ReportErrorf(sc, ident, "instanciating generic functions is not yet implemented")
			// just as a reminder to implement this.
			// `sig` is the concrete signature of this call. All symbols resolved.
			// `impl.Signature` is the generic signature from the definition with generic types in it.
		case *TcBuiltinProcDef:
			instance := InstanciateBuiltinGenericProc(impl, &sigSubstitutions[0])
			result.Sym = &TcProcRef{Source: ident.Source, Overloadable: instance}
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
				substitutions = append(substitutions, TemplateSubstitution{SymOrIdent: sigParams[i], Value: value})
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
			result.Sym = &TcProcRef{Source: ident.Source, Overloadable: impl}
			result.Args = checkedArgs
			newResult := impl.MacroFunc(sc, scope, result, expected)
			// TODO, only do this, if the macro has no errors?
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

func (sc *SemChecker) ApplyDocComment(expr TcExpr, doc *PrefixDocComment) TcExpr {
	switch expr2 := expr.(type) {
	case *TcVariableDefStmt:
		//_, name, _, _, _ := MatchVariableDefStatement(sc, expr2)
		//
		name := expr2.Sym
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
	case *TcProcDef:
		// validate doc section
		// name.Comment = append(name.Comment, def.DocComment.BaseDoc...)
	DOCSECTIONS1:
		for _, it := range doc.NamedDocSections {
			key := it.Name
			value := it.Lines
			for _, arg := range expr2.Signature.Params {
				if arg.Source == key {
					arg.Comment = append(arg.Comment, value...)
					continue DOCSECTIONS1
				}
			}
			ReportInvalidDocCommentKey(sc, it)
		}
		expr2.DocComment = doc
		return expr2
	case *TcStructDef:
		expr2.DocComment = doc
	DOCSECTIONS2:
		for _, it := range doc.NamedDocSections {
			key := it.Name
			// value := it.Lines
			for _, it := range expr2.Fields {
				if it.Name == key {
					// TODO do something with value ? Store it in the field?
					continue DOCSECTIONS2
				}
			}
			ReportInvalidDocCommentKey(sc, it)
		}
		return expr2
	case *TcEnumDef:
		expr2.DocComment = doc
	DOCSECTIONS3:
		for _, it := range doc.NamedDocSections {
			key := it.Name
			// value := it.Lines
			for _, it := range expr2.Values {
				if it.Source == key {
					// TODO do something with value ? Store it in the field?
					continue DOCSECTIONS3
				}
			}
			ReportInvalidDocCommentKey(sc, it)
		}
		return expr2
	case *TcTypeAlias:
		ReportErrorf(sc, expr2, "type alias has no support for doc comments yet")
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
				var tcItem TcExpr
				if i == N-1 {
					tcItem = SemCheckExpr(sc, scope, item, expected)
				} else {
					tcItem = SemCheckExpr(sc, scope, item, UniqueTypeConstraint{TypeVoid})
				}
				if docComment != nil {
					item = sc.ApplyDocComment(tcItem, docComment)
					docComment = nil
				}
				resultItems = append(resultItems, tcItem)
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
	// check for reserved words
	switch ident.Source {
	case "emit", "import", "static", "|", "break", "continue":
		ReportErrorf(sc, ident, "%s name is a reserved word", extraword)
	}
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

func (lit *AbstractDefaultValue) GetType() Type { return lit.Type }
func (_ *TcErrorNode) GetType() Type            { return TypeError }
func (call *TcCall) GetType() Type              { return call.Sym.Overloadable.GetSignature().ResultType }
func (lit *TcStrLit) GetType() Type             { return lit.Type }
func (lit *TcArrayLit) GetType() Type           { return lit.Type }
func (lit *TcEnumSetLit) GetType() Type         { return GetEnumSetType(lit.ElemType) }
func (lit *TcStructLit) GetType() Type          { return lit.Type }
func (sym *TcSymbol) GetType() Type             { return sym.Type }
func (sym *TcSymRef) GetType() Type             { return sym.Type }
func (stmt *TcVariableDefStmt) GetType() Type   { return TypeVoid }
func (stmt *TcStructDef) GetType() Type         { return TypeVoid }
func (stmt *TcEnumDef) GetType() Type           { return TypeVoid }
func (stmt *TcTypeAlias) GetType() Type         { return TypeVoid }
func (stmt *TcTraitDef) GetType() Type          { return TypeVoid }
func (stmt *TcForLoopStmt) GetType() Type       { return TypeVoid }
func (stmt *TcWhileLoopStmt) GetType() Type     { return TypeVoid }
func (arg *TcBuiltinProcDef) GetType() Type     { return TypeVoid }
func (arg *TcProcDef) GetType() Type            { return TypeVoid }
func (arg *TcTemplateDef) GetType() Type        { return TypeVoid }
func (arg *TcPackageDef) GetType() Type         { return TypeVoid }
func (arg *TcBuiltinMacroDef) GetType() Type    { return TypeVoid }
func (arg *TcErrorProcDef) GetType() Type       { return TypeVoid }
func (stmt *TcIfStmt) GetType() Type            { return TypeVoid }
func (stmt *TcIfElseExpr) GetType() Type        { return stmt.Else.GetType() }
func (returnExpr *TcReturnStmt) GetType() Type  { return TypeNoReturn }
func (expr *TcTypeContext) GetType() Type       { return GetTypeType(expr.WrappedType) }
func (expr *TcDotExpr) GetType() Type           { return expr.Rhs.GetType() }
func (field *TcStructField) GetType() Type      { return field.Type }
func (expr *TcWrappedUntypedAst) GetType() Type { return TypeUntyped }
func (expr *TcEmitExpr) GetType() Type          { return expr.Type }
func (expr *TcConvExpr) GetType() Type          { return expr.Type }
func (expr *TcCastExpr) GetType() Type          { return expr.Type }

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
		if arg.Value.Cmp(typ.Value) != 0 {
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
			Raw:    arg.Raw,
		}
	}

	switch typ := uniqueConstraint.Typ.(type) {
	case *BuiltinIntType:
		if typ == TypeChar {
			runeCount := utf8.RuneCountInString(arg.Value)
			if runeCount != 1 {
				ReportErrorf(sc, arg, "char lit must have exactly one rune, but it has %d runes", runeCount)
				goto error
			}
			rune, _ := utf8.DecodeRuneInString(arg.Value)
			value := big.NewInt(int64(rune))
			return &TcIntLit{arg.Source, typ, value}
		}
	case *BuiltinStringType:
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
	case *ArrayLit:
		return (TcExpr)(SemCheckArrayLit(sc, scope, arg, expected))
	case *NilLit:
		return (TcExpr)(SemCheckNilLit(sc, scope, arg, expected))
	case TcExpr:
		ExpectType(sc, arg, arg.GetType(), expected)
		return arg
	default:
		panic(fmt.Errorf("not implemented %T", arg))
	}
}

type ArrayTypeMapKey struct {
	elem Type
	len  int64
}

// Stores information that a type still has unresolved generics.
// example  array(2,array(2,T)) -> [T]
var openGenericsMap map[Type][]Type

var arrayTypeMap map[ArrayTypeMapKey]*ArrayType
var simdVectorTypeMap map[ArrayTypeMapKey]*SimdVectorType

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
		openGenericsMap[result] = openGenericsMap[elem]
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

func GetSimdVectorType(elem NamedType, len int64) (result *SimdVectorType) {
	result, ok := simdVectorTypeMap[ArrayTypeMapKey{elem, len}]
	if !ok {
		result = &SimdVectorType{Elem: elem, Len: len}
		simdVectorTypeMap[ArrayTypeMapKey{elem, len}] = result
		openGenericsMap[result] = openGenericsMap[elem]
		// TODO, this should be one generic builtin. Adding the overloads like here
		// does have a negative effect or error messages.
		//
		// TODO the array index operator needs mutability propagation of the first argument.
		// TODO this should be generic for better error messages on missing overloads, listing all currently known array types is a bit much
		registerBuiltin("indexOp", "", "[", "]", []Type{result, TypeInt64}, elem, 0)
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
		openGenericsMap[result] = openGenericsMap[elem]
	}
	return result
}

func GetPtrType(target Type) (result *PtrType) {
	result, ok := ptrTypeMap[target]
	if !ok {
		result = &PtrType{Target: target}
		ptrTypeMap[target] = result
		openGenericsMap[result] = openGenericsMap[target]
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
		openGenericsMap[result] = openGenericsMap[typ]
	}
	return result
}

func SemCheckArrayLit(sc *SemChecker, scope Scope, arg *ArrayLit, expected TypeConstraint) TcExpr {
	switch exp := expected.(type) {
	case *UnspecifiedType, *TypeTrait:
		result := &TcArrayLit{}
		if len(arg.Items) == 0 {
			result.ElemType = TypeVoid
			result.Type = GetArrayType(TypeVoid, 0)
			return result
		}
		result.Items = make([]TcExpr, len(arg.Items))
		result.Items[0] = SemCheckExpr(sc, scope, arg.Items[0], TypeUnspecified)
		result.ElemType = result.Items[0].GetType()
		for i := 1; i < len(arg.Items); i++ {
			result.Items[i] = SemCheckExpr(sc, scope, arg.Items[i], UniqueTypeConstraint{result.ElemType})
		}
		result.Type = GetArrayType(result.ElemType, int64(len(result.Items)))
		return result
	case UniqueTypeConstraint:
		switch exp := exp.Typ.(type) {
		case *ArrayType:
			result := &TcArrayLit{
				Items:    make([]TcExpr, len(arg.Items)),
				ElemType: exp.Elem,
				Type:     exp,
			}
			for i, item := range arg.Items {
				result.Items[i] = SemCheckExpr(sc, scope, item, UniqueTypeConstraint{exp.Elem})
			}
			ExpectArgsLen(sc, arg, len(arg.Items), int(exp.Len))
			return result
		case *SimdVectorType:
			// TODO this code is identical to ArratType, maybe it can be compressed
			result := &TcArrayLit{
				Items:    make([]TcExpr, len(arg.Items)),
				ElemType: exp.Elem,
				Type:     exp,
			}
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

func SemCheckPackage(sc *SemChecker, currentProgram *ProgramContext, arg *PackageDef, mainPackage bool) (result *TcPackageDef) {
	result = &TcPackageDef{WorkDir: arg.WorkDir}
	importScope := NewSubScope(builtinScope)
	importScope.CurrentProgram = currentProgram
	pkgScope := NewSubScope(importScope)

	importScope.CurrentPackage = result //
	pkgScope.CurrentPackage = result
	result.Name = arg.Name

	var docComment *PrefixDocComment = nil
	for _, stmt := range arg.TopLevelStmts {
		var tcStmt TcExpr
		switch stmt := stmt.(type) {
		case *PrefixDocComment:
			if docComment != nil {
				panic("internal error, doc comment should be nil here")
			}
			docComment = stmt
			// continue preventsn that the doc comment will be applied to nothing
			continue
		case *Call:
			tcStmt = SemCheckCall(sc, pkgScope, stmt, UniqueTypeConstraint{TypeVoid})
			switch x := tcStmt.(type) {
			case *TcVariableDefStmt:
				result.VarDefs = append(result.VarDefs, x)
			case *TcEmitExpr:
				result.EmitStatements = append(result.EmitStatements, x)
			case *TcTraitDef:
				result.TraitDefs = append(result.TraitDefs, x)
			case *TcStructDef:
				result.StructDefs = append(result.StructDefs, x)
			case *TcEnumDef:
				result.EnumDefs = append(result.EnumDefs, x)
			case *TcTypeAlias:
				result.TypeAliases = append(result.TypeAliases, x)
			case *TcProcDef:
				result.ProcDefs = append(result.ProcDefs, x)
				if mainPackage && x.Signature.Name == "main" {
					if len(x.Signature.GenericParams) > 0 {
						ReportErrorf(sc, x, "main proc may not be generic")
					}
					if currentProgram.Main != nil {
						ReportErrorf(sc, x, "double definition of main proc")
						ReportErrorf(sc, currentProgram.Main, "previously defined here")
					}
					currentProgram.Main = x
					// TODO, verify compatible signature for main
				}
			case *TcTemplateDef:
				result.TemplateDefs = append(result.TemplateDefs, x)
			case *TcErrorNode:
				// ignore, error already reported
			case *TcCodeBlock:
				if len(x.Items) != 0 {
					ReportErrorf(sc, tcStmt, "top level code blocks are not allowed")
				}
			case *TcCall:
				ReportErrorf(sc, tcStmt, "top level function calls are not allowed: %s", AstFormat(tcStmt))
			default:
				ReportErrorf(sc, tcStmt, "internal error, node type not handled: %T", tcStmt)
			}
		default:
			ReportInvalidAstNode(sc, stmt, "top level statement")
		}
		if docComment != nil {
			sc.ApplyDocComment(tcStmt, docComment)
			docComment = nil
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
