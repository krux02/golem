package main

import (
	"math/big"
	"strings"
)

// The ast after beeing processed by the typechecker. Tc prefix is for
// type checked.

type Type interface {
	PrettyPrint(*AstPrettyPrinter)
	ManglePrint(*strings.Builder) // print for name magling
	DefaultValue(sc *SemChecker, context Expr) TcExpr
}

type NamedType interface {
	Type
	GetName() string
}

type TypeConstraint interface {
	PrettyPrint(*AstPrettyPrinter)
	typeconstraint()
	// UniqueTypeConstraint
	// TypeGroup
	// TypeTrait
	// UnspecifiedType
	//
	// or normal type if only that type is valid
}

func (UniqueTypeConstraint) typeconstraint() {}
func (*TypeGroup) typeconstraint()           {}
func (*TypeTrait) typeconstraint()           {}
func (*UnspecifiedType) typeconstraint()     {}

type UniqueTypeConstraint struct {
	Typ Type
}

type TcExpr interface {
	Expr
	GetType() Type
}

// node to wrap code that can't be typechecked
type TcErrorNode struct {
	Source     string
	SourceNode Expr
}

func newErrorNode(source Expr) *TcErrorNode {
	return &TcErrorNode{Source: source.GetSource(), SourceNode: source}
}

type TcStructField struct {
	Source string
	Name   string
	Type   Type
}

type TcEnumDef struct {
	Source  string
	Name    string
	Values  []*TcSymbol
	Importc bool
}

type TcStructDef struct {
	Source  string
	Name    string
	Fields  []*TcStructField
	Importc bool
}

type TcTypeAlias struct {
	Source string
	Name   string
	Type   Type
}

type TraitInstance struct {
	// to semcheck a generic proc, prototypes for each signature are required. The
	// signature uses the generic type symbols from the trait def, while the
	// prototypes uses the generic types signatures from the generic proc def.
	//
	// Each Signature maps to one proc prototype. It is represented here as a
	// normal proc def, but without actual body.
	//
	// `Signatures[i]` maps to `ProcDefs[i]`.

	ProcDefs []*TcProcDef
}

type TcTraitDef struct {
	Source         string
	Name           string
	DependentTypes []*GenericTypeSymbol

	Signatures []Signature
}

// abstract TypeSymbol and GenericTypeSymbol are generated in pairs for generic
// parameters in generic procedures. The genericTypeSymbol is for the Signature
// that holds the type constraint. The Abstract Type Symbol is more like a
// temporary concrete type symbol that will be injected in the scope of the
// procedure body, so that there is a concret type for overload resolution.

type AbstractTypeSymbol struct {
	Name string
}

type GenericTypeSymbol struct {
	// a symbol that needs later substitution on generic instantiation
	Source     string
	Name       string
	Constraint TypeConstraint      // TypeGroup, TypeTrait, TypeUnspecified
	TraitInst  *TraitInstance      // if the Constraint is `TypeTrait` this is non nil
	AbsTypSym  *AbstractTypeSymbol // if the Constraint is `TypeTrait` this is non nil
}

func NewAbstractTypeSymbol(Name string) *AbstractTypeSymbol {
	result := &AbstractTypeSymbol{Name: Name}
	openGenericsMap[result] = []Type{result}
	return result
}

func NewGenericTypeSymbol(Source string, Name string, constraint TypeConstraint) *GenericTypeSymbol {
	result := &GenericTypeSymbol{
		Source:     Source,
		Name:       Name,
		Constraint: constraint,
	}
	openGenericsMap[result] = []Type{result}
	return result
}

type TcCodeBlock struct {
	Source string
	Items  []TcExpr
}

// TODO maybe unify TcProcSym with SymRef
type TcProcRef struct {
	Source       string
	Overloadable Overloadable
}

type SymbolKind int

// not the type or properties of a symbol, just where it has been declared
const (
	SkInvalid SymbolKind = iota
	SkConst
	SkLet
	SkVar
	SkProcArg
	SkVarProcArg
	SkLoopIterator
	SkVarLoopIterator
	SkEnum
)

type TcSymbol struct {
	Source string
	Kind   SymbolKind
	Value  TcExpr // only set when symbol is `const`
	Type   Type
}

// whenever a variable is used
type TcSymRef struct {
	Source string
	Sym    *TcSymbol
	Type   Type
}

type TcForLoopStmt struct {
	Source     string
	LoopSym    *TcSymbol
	Collection TcExpr
	Body       TcExpr
}

type TcWhileLoopStmt struct {
	Source    string
	Condition TcExpr
	Body      TcExpr
}

type TcIfStmt struct {
	Source    string
	Condition TcExpr
	Body      TcExpr
}

type TcIfElseExpr struct {
	Source    string
	Condition TcExpr
	Body      TcExpr
	Else      TcExpr
}

type TcDotExpr struct {
	Source string
	Lhs    TcExpr
	Rhs    *TcStructField
}

type TcCall struct {
	Source string
	Sym    *TcProcRef
	Args   []TcExpr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type TcVariableDefStmt struct {
	Source string
	Sym    *TcSymbol
	Value  TcExpr
}

type TcReturnStmt struct {
	Source string
	Value  TcExpr
}

type TcTypeContext struct {
	Source      string
	WrappedType Type
}

// TODO rename this, this is an OverloadableSignature
type Signature struct {
	// type placeholders within this procedure that need to be substituted with
	// real types in generic instantiations.
	Name          string
	GenericParams []*GenericTypeSymbol
	Params        []*TcSymbol
	ResultType    Type

	// NOTE: Varargs currently only used for printf.
	Varargs bool
}

type Overloadable interface {
	TcExpr
	GetSignature() *Signature
}

type TcBuiltinProcDef struct {
	Signature Signature
	//Body      TcExpr

	// TODO these are fields to specifiy how to generate a call
	// example1 "foo(", ", ", ")"        function call
	// example2 "(", " + ", ")"          operator+ call
	// example3 "somearray[", "][", "]"  array access
	Prefix, Infix, Postfix string
	InstanceCache          *InstanceCache[Overloadable]
}

type TcBuiltinStaticProcDef struct {
	Signature Signature
	ApplyFunc func(context *TcPackageDef, args []TcExpr) TcExpr
}

type TcProcDef struct {
	Source        string
	MangledName   string
	Signature     Signature
	Body          TcExpr
	Importc       bool
	InstanceCache *InstanceCache[Overloadable]
}

type TcBuiltinMacroDef struct {
	Signature Signature
	MacroFunc BuiltinMacroFunc // this function transforms the call, post function resolution
}

type TcTemplateDef struct {
	Source    string
	Signature Signature
	Body      Expr
}

type TcErrorProcDef struct {
	// this is just a placeholder to set a TcProcSymbol to something
	Signature Signature
}

type TcStrLit struct {
	Source string
	Type   Type
	Value  string
	Raw    bool // was the original string literal a raw string literal, used for nothing but PrettyPrinting
}

type TcIntLit struct {
	Source string
	Type   Type
	Value  *big.Int
}

type TcFloatLit struct {
	Source string
	Type   Type
	Value  *big.Float
}

type TcArrayLit struct {
	Source   string
	Items    []TcExpr
	ElemType Type
}

type TcStructLit struct {
	Source string
	Items  []TcExpr
	Type   *StructType
}

type TcEnumSetLit struct {
	Source   string
	Items    []TcExpr
	ElemType *EnumType
}

type ProgramContext struct {
	LinkerFlags []string
	Main        *TcProcDef
}

type TcPackageDef struct {
	Source         string
	Name           string
	WorkDir        string
	CFlags         []string
	EmitStatements []*TcEmitExpr
	StructDefs     []*TcStructDef
	EnumDefs       []*TcEnumDef
	TypeAliases    []*TcTypeAlias
	TraitDefs      []*TcTraitDef
	VarDefs        []*TcVariableDefStmt
	ProcDefs       []*TcProcDef
	TemplateDefs   []*TcTemplateDef
	ExportScope    *ScopeImpl
}

type SymSourcePair struct {
	EmitSource string
	Sym        TcExpr
}

type TcCastExpr struct {
	Source string
	Expr   TcExpr
	Type   Type
}

type TcConvExpr struct {
	Source string
	Expr   TcExpr
	Type   Type
}

type TcEmitExpr struct {
	Source         string
	Type           Type
	SourceSymPairs []SymSourcePair
	EmitSource     string
}

type TcWrappedUntypedAst struct {
	Expr Expr
}

func (arg *TcErrorNode) GetSource() string       { return arg.Source }
func (arg *TcDotExpr) GetSource() string         { return arg.Source }
func (arg *TcStructField) GetSource() string     { return arg.Source }
func (arg *TcSymbol) GetSource() string          { return arg.Source }
func (arg *TcSymRef) GetSource() string          { return arg.Source }
func (arg *TcProcRef) GetSource() string         { return arg.Source }
func (arg *TcVariableDefStmt) GetSource() string { return arg.Source }
func (arg *TcReturnStmt) GetSource() string      { return arg.Source }
func (arg *TcTypeContext) GetSource() string     { return arg.Source }
func (arg *TcForLoopStmt) GetSource() string     { return arg.Source }
func (arg *TcWhileLoopStmt) GetSource() string   { return arg.Source }
func (arg *TcIfStmt) GetSource() string          { return arg.Source }
func (arg *TcIfElseExpr) GetSource() string      { return arg.Source }
func (arg *TcCodeBlock) GetSource() string       { return arg.Source }
func (arg *TcCall) GetSource() string            { return arg.Source }
func (arg *TcIntLit) GetSource() string          { return arg.Source }
func (arg *TcFloatLit) GetSource() string        { return arg.Source }
func (arg *TcStrLit) GetSource() string          { return arg.Source }
func (arg *TcArrayLit) GetSource() string        { return arg.Source }
func (arg *TcEnumSetLit) GetSource() string      { return arg.Source }
func (arg *TcProcDef) GetSource() string         { return arg.Source }
func (arg *TcBuiltinProcDef) GetSource() string  { return "" } // builtins have no source location
func (arg *TcTemplateDef) GetSource() string     { return arg.Source }
func (arg *TcStructLit) GetSource() string       { return arg.Source }
func (arg *TcPackageDef) GetSource() string      { return arg.Source }
func (arg *TcEmitExpr) GetSource() string        { return arg.Source }
func (arg *TcCastExpr) GetSource() string        { return arg.Source }
func (arg *TcConvExpr) GetSource() string        { return arg.Source }

func (arg *TcStructDef) GetSource() string         { return arg.Source }
func (arg *TcEnumDef) GetSource() string           { return arg.Source }
func (arg *TcTypeAlias) GetSource() string         { return arg.Source }
func (arg *TcTraitDef) GetSource() string          { return arg.Source }
func (arg *GenericTypeSymbol) GetSource() string   { return arg.Source }
func (arg *TcBuiltinMacroDef) GetSource() string   { return "" } // builtins have no source
func (arg *TcErrorProcDef) GetSource() string      { return "" } // non existing proc def has no source
func (arg *TcWrappedUntypedAst) GetSource() string { return arg.Expr.GetSource() }

func (arg *TcBuiltinProcDef) GetSignature() *Signature  { return &arg.Signature }
func (arg *TcProcDef) GetSignature() *Signature         { return &arg.Signature }
func (arg *TcTemplateDef) GetSignature() *Signature     { return &arg.Signature }
func (arg *TcBuiltinMacroDef) GetSignature() *Signature { return &arg.Signature }
func (arg *TcErrorProcDef) GetSignature() *Signature    { return &arg.Signature }

func RequireMutable(sc *SemChecker, expr TcExpr) {
	switch arg := expr.(type) {
	case *TcDotExpr:
		RequireMutable(sc, arg.Lhs)
	case *TcSymRef:
		switch arg.Sym.Kind {
		case SkInvalid, SkVar, SkVarProcArg, SkLoopIterator, SkEnum:
		case SkConst, SkLet, SkProcArg, SkVarLoopIterator:
			ReportMustBeMutable(sc, arg)
		}
	case *TcCall:
		// TODO, actually do the mutability inference, this is a real hack
		if arg.Sym.Source == "indexOp" {
			switch len(arg.Args) {
			case 1:
				// pointer deref
			case 2:
				// index operator
				RequireMutable(sc, arg.Args[0])
			}
		}
	case *TcCodeBlock:
		N := len(arg.Items)
		if N > 0 {
			RequireMutable(sc, arg.Items[N-1])
		}
	case *TcErrorNode:
		// To prevent noise from error nodes do not report here
	case *TcIfElseExpr:
		RequireMutable(sc, arg.Body)
		RequireMutable(sc, arg.Else)
	default:
		ReportMustBeMutable(sc, arg)
	}
}
