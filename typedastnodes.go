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
	DefaultValue(sc *SemChecker, context AstNode) TcExpr
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
	AstNode
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
	Values  []TcSymbol
	Importc bool
}

type TcStructDef struct {
	Source  string
	Name    string
	Fields  []TcStructField
	Importc bool
}

type TcTraitDef struct {
	Source         string
	Name           string
	DependentTypes []*GenericTypeSymbol
	Signatures     []*Signature
}

type GenericTypeSymbol struct {
	// a symbol that needs later substitution on generic instantiation
	Source     string
	Name       string
	Constraint TypeConstraint // TypeGroup, TypeTrait, TypeUnspecified
}

type TcCodeBlock struct {
	Source string
	Items  []TcExpr
}

// TODO maybe unify TcProcSym with the othe symbol types
type TcProcSymbol struct {
	Source    string
	Signature *Signature
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
	Value  TcExpr // only set when symbol in `const`
	Type   Type
}

type TcForLoopStmt struct {
	Source     string
	LoopSym    TcSymbol
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
	Rhs    TcStructField
}

type TcCall struct {
	Source string
	Sym    TcProcSymbol
	Args   []TcExpr
	// other properties
	Braced bool // true for (a+b) +(a,b), false for a+b
}

type TcVariableDefStmt struct {
	Source string
	Sym    TcSymbol
	Value  TcExpr
}

type TcReturnExpr struct {
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
	Params        []TcSymbol
	ResultType    Type

	// NOTE: Varargs currently only used for printf.
	Varargs bool
	// current list of substitutions that are not yet applied to `Impl`
	Substitutions []Substitution
	Impl          Overloadable
}

type Overloadable interface {
	TcExpr
	GetSignature() *Signature
}

type TcBuiltinProcDef struct {
	Signature *Signature
	//Body      TcExpr

	// TODO these are fields to specifiy how to generate a call
	// example1 "foo(", ", ", ")"        function call
	// example2 "(", " + ", ")"          operator+ call
	// example3 "somearray[", "][", "]"  array access
	Prefix, Infix, Postfix string
}

type TcBuiltinGenericProcDef struct {
	Signature *Signature
	//Body      TcExpr

	// TODO these are fields to specifiy how to generate a call
	// example1 "foo(", ", ", ")"        function call
	// example2 "(", " + ", ")"          operator+ call
	// example3 "somearray[", "][", "]"  array access
	Prefix, Infix, Postfix string
}

type TcBuiltinStaticProcDef struct {
	Signature *Signature

	ApplyFunc func(context *TcPackageDef, args []TcExpr) TcExpr
}

type TcProcDef struct {
	Source      string
	MangledName string
	Signature   *Signature
	Body        TcExpr
	Importc     bool
	// TODO: find a better solution for tagging other than mutuble setting a value
	// this value is set to true in the code generator to mark this proc as
	// already scheduled for code generation. This flag is used to prevent
	// generating the same type multiple times.
	scheduledforgeneration bool
}

type TcBuiltinMacroDef struct {
	Signature *Signature
	MacroFunc BuiltinMacroFunc // this function transforms the call, post function resolution
}

type TcTemplateDef struct {
	Source    string
	Signature *Signature
	Body      TcExpr
}

type TcErrorProcDef struct {
	// this is just a placeholder to set a TcProcSymbol to something
	Signature *Signature
}

type TcStrLit struct {
	Source string
	Type   Type
	Value  string
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
	CFlags         []string
	Imports        []TcImportStmt
	EmitStatements []*EmitStmt // TODO not a TC node
	StructDefs     []*TcStructDef
	EnumDefs       []*TcEnumDef
	TraitDefs      []*TcTraitDef
	VarDefs        []TcVariableDefStmt
	ProcDefs       []*TcProcDef
	ExportScope    *ScopeImpl
}

type TcImportStmt struct {
	Source  string
	Value   *StrLit
	Package *TcPackageDef
}

func (sym TcDotExpr) expression()          {}
func (sym TcSymbol) expression()           {}
func (stmt TcVariableDefStmt) expression() {}
func (stmt TcReturnExpr) expression()      {}
func (stmt TcForLoopStmt) expression()     {}
func (stmt TcIfStmt) expression()          {}
func (stmt TcIfElseExpr) expression()      {}
func (block TcCodeBlock) expression()      {}
func (call TcCall) expression()            {}
func (call TcArrayLit) expression()        {}
func (call TcEnumSetLit) expression()      {}
func (expr TcStructLit) expression()       {}

func (arg TcErrorNode) GetSource() string              { return arg.Source }
func (arg TcDotExpr) GetSource() string                { return arg.Source }
func (arg TcStructField) GetSource() string            { return arg.Source }
func (arg TcSymbol) GetSource() string                 { return arg.Source }
func (arg TcProcSymbol) GetSource() string             { return arg.Source }
func (arg TcVariableDefStmt) GetSource() string        { return arg.Source }
func (arg TcReturnExpr) GetSource() string             { return arg.Source }
func (arg TcTypeContext) GetSource() string            { return arg.Source }
func (arg TcForLoopStmt) GetSource() string            { return arg.Source }
func (arg TcWhileLoopStmt) GetSource() string          { return arg.Source }
func (arg TcIfStmt) GetSource() string                 { return arg.Source }
func (arg TcIfElseExpr) GetSource() string             { return arg.Source }
func (arg TcCodeBlock) GetSource() string              { return arg.Source }
func (arg TcCall) GetSource() string                   { return arg.Source }
func (arg TcIntLit) GetSource() string                 { return arg.Source }
func (arg TcFloatLit) GetSource() string               { return arg.Source }
func (arg TcStrLit) GetSource() string                 { return arg.Source }
func (arg TcArrayLit) GetSource() string               { return arg.Source }
func (arg TcEnumSetLit) GetSource() string             { return arg.Source }
func (arg *TcProcDef) GetSource() string               { return arg.Source }
func (arg *TcBuiltinProcDef) GetSource() string        { return "" } // builtins have no source location
func (arg *TcBuiltinGenericProcDef) GetSource() string { return "" } // builtins have no source location
func (arg *TcTemplateDef) GetSource() string           { return arg.Source }
func (arg TcStructLit) GetSource() string              { return arg.Source }
func (arg TcPackageDef) GetSource() string             { return arg.Source }

func (arg *TcStructDef) GetSource() string       { return arg.Source }
func (arg *TcEnumDef) GetSource() string         { return arg.Source }
func (arg *GenericTypeSymbol) GetSource() string { return arg.Source }
func (arg *TcBuiltinMacroDef) GetSource() string { return "" } // builtins have no source
func (arg *TcErrorProcDef) GetSource() string    { return "" } // non existing proc def has no source

// func (arg *EnumSetType) GetSource() string { return arg.Source }
// func (arg TypeGroup) GetSource() string    { return arg.Source }
// func (arg *BuiltinType) GetSource() string { return arg.Source }
// func (arg *ArrayType) GetSource() string   { return arg.Source }

func (arg *TcBuiltinProcDef) GetSignature() *Signature        { return arg.Signature }
func (arg *TcBuiltinGenericProcDef) GetSignature() *Signature { return arg.Signature }
func (arg *TcProcDef) GetSignature() *Signature               { return arg.Signature }
func (arg *TcTemplateDef) GetSignature() *Signature           { return arg.Signature }
func (arg *TcBuiltinMacroDef) GetSignature() *Signature       { return arg.Signature }
func (arg *TcErrorProcDef) GetSignature() *Signature          { return arg.Signature }

func RequireMutable(sc *SemChecker, expr TcExpr) {
	switch arg := expr.(type) {

	case TcDotExpr:
		RequireMutable(sc, arg.Lhs)
	case TcSymbol:
		switch arg.Kind {
		case SkInvalid, SkVar, SkVarProcArg, SkLoopIterator, SkEnum:
		case SkConst, SkLet, SkProcArg, SkVarLoopIterator:
			ReportMustBeMutable(sc, arg)
		}
	case TcCall:
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
	case TcCodeBlock:
		N := len(arg.Items)
		if N > 0 {
			RequireMutable(sc, arg.Items[N-1])
		}
	case TcErrorNode:
		// To prevent noise from error nodes do not report here
	case TcIfElseExpr:
		RequireMutable(sc, arg.Body)
		RequireMutable(sc, arg.Else)
	default:
		ReportMustBeMutable(sc, arg)
	}
}
