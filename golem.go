package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"syscall"
)

func extractErrorAnnotations(pak PackageDef) (errors []CompileError) {
	return
}

func errortest(filename string) {
	filename, err := filepath.Abs(filename)
	if err != nil {
		panic(err)
	}
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	source := string(bytes)
	pak := parsePackage(source, filename)

	validateSourceSet(pak.source, pak)

	errors := extractErrorAnnotations(pak)

	println("-------------------- expected errors -------------------------")
	for _, error := range errors {
		fmt.Printf("%v\n", error)
	}

	println("-------------------- gotten errors ---------------------------")
	tc := NewTypeChecker(source, filename)
	_ = tc.TypeCheckPackage(pak)
	for _, error := range tc.errors {
		fmt.Printf("%v\n", error)
	}
	println("--------------------------------------------------------------")
}

func compileAndRunFile(filename string, useExec bool) {
	filename, err := filepath.Abs(filename)
	if err != nil {
		panic(err)
	}
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	fmt.Println("----------------------- parsed  code -----------------------")
	source := string(bytes)
	pak := parsePackage(source, filename)
	validateSourceSet(pak.source, pak)
	fmt.Println(AstFormat(pak))
	fmt.Println("--------------------- typechecked code ---------------------")
	tc := NewTypeChecker(source, filename)
	typedPak := tc.TypeCheckPackage(pak)
	if len(tc.errors) > 0 {
		os.Exit(1)
	}
	fmt.Println(AstFormat(typedPak))
	fmt.Println("-------------------------- C code --------------------------")
	sourceCodeC := compilePackageToC(typedPak)
	fmt.Println(sourceCodeC)
	tempDir := path.Join(os.TempDir(), "golem")
	base := filepath.Base(filename)
	if base[len(base)-6:] != ".golem" {
		panic("Input file must end on .golem")
	}
	base = base[:len(base)-6]
	fileName := fmt.Sprintf("%s.c", base)
	absFilename := path.Join(tempDir, fileName)
	err = os.MkdirAll(tempDir, os.ModePerm)
	if err != nil {
		log.Fatal(err)
	}
	err = os.WriteFile(absFilename, []byte(sourceCodeC), 0666)
	if err != nil {
		log.Fatal(err)
	}
	binaryAbsFilename := path.Join(tempDir, base)
	cmd := exec.Command("gcc", absFilename, "-o", binaryAbsFilename)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err = cmd.Run()
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("=========================== exec ===========================")
	var argv []string = nil
	// exec should not return
	if useExec {
		log.Fatal(syscall.Exec(binaryAbsFilename, argv, nil))
		return // dead code
	} else {
		err = exec.Command(binaryAbsFilename, argv...).Run()
		if err != nil {
			log.Fatal(err)
		}
		return
	}
}
