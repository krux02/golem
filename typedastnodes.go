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
	AppendToGroup(*TypeGroupBuilder) bool // return true if the type group became `TypeUnspecified`
}

type TcExpr interface {
	AstNode
	GetType() Type
}

// node to wrap code that can't be typechecked
type TcErrorNode struct {
	SourceNode Expr
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

type GenericTypeSymbol struct {
	// a symbol that needs later substitution on generic instantiation
	Source     string
	Name       string
	Constraint Type // TypeGroup or TypeUnspecified
}

type TcCodeBlock struct {
	Source string
	Items  []TcExpr
}

// TODO unify TcProcSym with the othe symbol types
type TcProcSymbol struct {
	Source string
	Impl   Overloadable // TcProcDef, TcBuiltinProcDef
}

type SymbolKind int

// not the type or properties of a symbol, just where it has been declared
const (
	SkInvalid SymbolKind = iota
	SkLet
	SkVar
	SkConst
	SkProcArg
	SkLoopIterator
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
	Rhs    TcExpr
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

type TcReturnStmt struct {
	Source string
	Value  TcExpr
}

type TcTypeContext struct {
	Source string
	Type   Type
}

// TODO rename this, this is an OverloadableSignature
type ProcSignature struct {

	// type placeholders within this procedure that need to be substituted with
	// real types in generic instantiations.
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
	GetName() string
	GetSignature() ProcSignature
}

type TcBuiltinProcDef struct {
	Source    string // TODO: this can't be correct, it's builtin there is no source
	Name      string
	Signature ProcSignature
	//Body      TcExpr

	// TODO these are fields to specifiy how to generate a call
	// example1 "foo(", ", ", ")"        function call
	// example2 "(", " + ", ")"          operator+ call
	// example3 "somearray[", "][", "]"  array access
	Prefix, Infix, Postfix string
}

type TcBuiltinGenericProcDef struct {
	Source    string // TODO: this can't be correct, it's builtin there is no source
	Name      string
	Signature ProcSignature
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
	Signature ProcSignature
	//Body      TcExpr

	ApplyFunc func(context *TcPackageDef, args []TcExpr) TcExpr
}

type TcProcDef struct {
	Source      string
	Name        string
	MangledName string
	Signature   ProcSignature
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
	Name      string
	Signature ProcSignature
	MacroFunc BuiltinMacroFunc // this function transforms the call, post function resolution
}

type TcTemplateDef struct {
	Source    string
	Name      string
	Signature ProcSignature
	Body      TcExpr
}

type TcErrorProcDef struct {
	// this is just a placeholder to set a TcProcSymbol to something
	Name string
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

type TcPackageDef struct {
	Source         string
	Name           string
	CFlags         []string
	Imports        []*TcPackageDef
	EmitStatements []EmitStmt
	StructDefs     []*TcStructDef
	EnumDefs       []*TcEnumDef
	VarDefs        []TcVariableDefStmt
	ProcDefs       []*TcProcDef
	Main           *TcProcDef // main entry point
}

func (sym TcDotExpr) expression()          {}
func (sym TcSymbol) expression()           {}
func (stmt TcVariableDefStmt) expression() {}
func (stmt TcReturnStmt) expression()      {}
func (stmt TcForLoopStmt) expression()     {}
func (stmt TcIfStmt) expression()          {}
func (stmt TcIfElseExpr) expression()      {}
func (block TcCodeBlock) expression()      {}
func (call TcCall) expression()            {}
func (call TcArrayLit) expression()        {}
func (call TcEnumSetLit) expression()      {}
func (expr TcStructLit) expression()       {}

func (arg TcErrorNode) GetSource() string              { return arg.SourceNode.GetSource() }
func (arg TcDotExpr) GetSource() string                { return arg.Source }
func (arg TcStructField) GetSource() string            { return arg.Source }
func (arg TcSymbol) GetSource() string                 { return arg.Source }
func (arg TcProcSymbol) GetSource() string             { return arg.Source }
func (arg TcVariableDefStmt) GetSource() string        { return arg.Source }
func (arg TcReturnStmt) GetSource() string             { return arg.Source }
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
func (arg *TcBuiltinProcDef) GetSource() string        { return arg.Source }
func (arg *TcBuiltinGenericProcDef) GetSource() string { return arg.Source }
func (arg *TcTemplateDef) GetSource() string           { return arg.Source }
func (arg TcStructLit) GetSource() string              { return arg.Source }

func (arg TcPackageDef) GetSource() string { return arg.Source }

func (arg *TcStructDef) GetSource() string       { return arg.Source }
func (arg *TcEnumDef) GetSource() string         { return arg.Source }
func (arg *GenericTypeSymbol) GetSource() string { return arg.Source }
func (arg *TcBuiltinMacroDef) GetSource() string { return arg.Source }
func (arg *TcErrorProcDef) GetSource() string    { return "" }

// func (arg *EnumSetType) GetSource() string { return arg.Source }
// func (arg TypeGroup) GetSource() string    { return arg.Source }
// func (arg *BuiltinType) GetSource() string { return arg.Source }
// func (arg *ArrayType) GetSource() string   { return arg.Source }

func (arg *TcBuiltinProcDef) GetName() string        { return arg.Name }
func (arg *TcBuiltinGenericProcDef) GetName() string { return arg.Name }
func (arg *TcProcDef) GetName() string               { return arg.Name }
func (arg *TcTemplateDef) GetName() string           { return arg.Name }
func (arg *TcBuiltinMacroDef) GetName() string       { return arg.Name }
func (arg *TcErrorProcDef) GetName() string          { return arg.Name }

func (arg *TcBuiltinProcDef) GetSignature() ProcSignature        { return arg.Signature }
func (arg *TcBuiltinGenericProcDef) GetSignature() ProcSignature { return arg.Signature }
func (arg *TcProcDef) GetSignature() ProcSignature               { return arg.Signature }
func (arg *TcTemplateDef) GetSignature() ProcSignature           { return arg.Signature }
func (arg *TcBuiltinMacroDef) GetSignature() ProcSignature       { return arg.Signature }
func (arg *TcErrorProcDef) GetSignature() ProcSignature          { panic("not thought through") }
