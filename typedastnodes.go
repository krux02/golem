package main

import (
	"strings"
)

// The ast after beeing processed by the typechecker. Tc prefix is for
// type checked.

type Type interface {
	PrettyPrint(*AstPrettyPrinter)
	ManglePrint(*strings.Builder) // print for name magling
	DefaultValue(tc *TypeChecker, context AstNode) TcExpr
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
	GetMutable() bool
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
	Signatures     []*ProcSignature
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
	Source string
	Impl   Overloadable // TcProcDef, TcBuiltinProcDef
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
type ProcSignature struct {
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
	GetSignature() *ProcSignature
}

type TcBuiltinProcDef struct {
	Signature *ProcSignature
	//Body      TcExpr

	// TODO these are fields to specifiy how to generate a call
	// example1 "foo(", ", ", ")"        function call
	// example2 "(", " + ", ")"          operator+ call
	// example3 "somearray[", "][", "]"  array access
	Prefix, Infix, Postfix string
}

type TcBuiltinGenericProcDef struct {
	Signature *ProcSignature
	//Body      TcExpr

	// TODO these are fields to specifiy how to generate a call
	// example1 "foo(", ", ", ")"        function call
	// example2 "(", " + ", ")"          operator+ call
	// example3 "somearray[", "][", "]"  array access
	Prefix, Infix, Postfix string
}

type TcBuiltinStaticProcDef struct {
	Source    string // TODO: this can't be correct, it's builtin there is no source
	Name      string
	Signature *ProcSignature
	//Body      TcExpr

	ApplyFunc func(context *TcPackageDef, args []TcExpr) TcExpr
}

type TcProcDef struct {
	Source      string
	MangledName string
	Signature   *ProcSignature
	Body        TcExpr
	Importc     bool
	// TODO: find a better solution for tagging other than mutuble setting a value
	// this value is set to true in the code generator to mark this proc as
	// already scheduled for code generation. This flag is used to prevent
	// generating the same type multiple times.
	scheduledforgeneration bool
}

type TcBuiltinMacroDef struct {
	Source    string // TODO: this can't be correct, it's builtin there is no source
	Signature *ProcSignature
	MacroFunc BuiltinMacroFunc // this function transforms the call, post function resolution
}

type TcTemplateDef struct {
	Source    string
	Signature *ProcSignature
	Body      TcExpr
}

type TcErrorProcDef struct {
	// this is just a placeholder to set a TcProcSymbol to something
	Signature *ProcSignature
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
	EmitStatements []EmitStmt
	StructDefs     []*TcStructDef
	EnumDefs       []*TcEnumDef
	TraitDefs      []*TcTraitDef
	VarDefs        []TcVariableDefStmt
	ProcDefs       []*TcProcDef
	ExportScope    *ScopeImpl
}

type TcImportStmt struct {
	Source  string
	Value   StrLit
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
func (arg *TcBuiltinMacroDef) GetSource() string { return arg.Source }
func (arg *TcErrorProcDef) GetSource() string    { return "" }

// func (arg *EnumSetType) GetSource() string { return arg.Source }
// func (arg TypeGroup) GetSource() string    { return arg.Source }
// func (arg *BuiltinType) GetSource() string { return arg.Source }
// func (arg *ArrayType) GetSource() string   { return arg.Source }

func (arg *TcBuiltinProcDef) GetSignature() *ProcSignature        { return arg.Signature }
func (arg *TcBuiltinGenericProcDef) GetSignature() *ProcSignature { return arg.Signature }
func (arg *TcProcDef) GetSignature() *ProcSignature               { return arg.Signature }
func (arg *TcTemplateDef) GetSignature() *ProcSignature           { return arg.Signature }
func (arg *TcBuiltinMacroDef) GetSignature() *ProcSignature       { return arg.Signature }
func (arg *TcErrorProcDef) GetSignature() *ProcSignature          { return arg.Signature }

func (arg TcDotExpr) GetMutable() bool   { return arg.Lhs.GetMutable() }
func (arg TcErrorNode) GetMutable() bool { return true } // just to prevent noice from error nodes
func (arg TcSymbol) GetMutable() bool {
	k := arg.Kind
	return k == SkVar || k == SkVarProcArg || k == SkVarLoopIterator || k == SkInvalid
}
func (call TcCall) GetMutable() bool {
	// TODO, actually do the mutability inference, this is a real hack
	if call.Sym.Source == "[" {
		switch len(call.Args) {
		case 1:
			// pointer deref
			return true
		case 2:
			// index operator
			return call.Args[0].GetMutable()
		}
	}

	return false
}

func (arg TcCodeBlock) GetMutable() bool {
	if len(arg.Items) > 0 {
		return arg.Items[len(arg.Items)-1].GetMutable()
	}
	return false
}

func (arg TcIfElseExpr) GetMutable() bool { return arg.Body.GetMutable() && arg.Else.GetMutable() }

func (arg TcStructField) GetMutable() bool { return false }

func (arg TcProcSymbol) GetMutable() bool      { return false }
func (arg TcVariableDefStmt) GetMutable() bool { return false }
func (arg TcReturnExpr) GetMutable() bool      { return false }
func (arg TcTypeContext) GetMutable() bool     { return false }
func (arg TcForLoopStmt) GetMutable() bool     { return false }
func (arg TcWhileLoopStmt) GetMutable() bool   { return false }
func (arg TcIfStmt) GetMutable() bool          { return false }

func (arg TcArrayLit) GetMutable() bool   { return false }
func (arg TcEnumSetLit) GetMutable() bool { return false }
func (arg IntLit) GetMutable() bool       { return false }
func (arg FloatLit) GetMutable() bool     { return false }
func (arg CStrLit) GetMutable() bool      { return false }
func (arg StrLit) GetMutable() bool       { return false }
func (arg CharLit) GetMutable() bool      { return false }
func (arg NilLit) GetMutable() bool       { return false }

func (arg *TcProcDef) GetMutable() bool               { return false }
func (arg *TcBuiltinProcDef) GetMutable() bool        { return false }
func (arg *TcBuiltinGenericProcDef) GetMutable() bool { return false }
func (arg *TcTemplateDef) GetMutable() bool           { return false }
func (arg TcStructLit) GetMutable() bool              { return false }
func (arg TcPackageDef) GetMutable() bool             { return false }

func (arg *TcStructDef) GetMutable() bool       { return false }
func (arg *TcEnumDef) GetMutable() bool         { return false }
func (arg *GenericTypeSymbol) GetMutable() bool { return false }
func (arg *TcBuiltinMacroDef) GetMutable() bool { return false }
func (arg *TcErrorProcDef) GetMutable() bool    { return false }
