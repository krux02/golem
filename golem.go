package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"testing"
)

func errorTest(t *testing.T, filename string) {
	bytes, err := os.ReadFile(filename)
	if err != nil {
		t.Fatal(err)
	}

	source := string(bytes)
	// TODO this is not safe as it does not check for string content
	//pattern := regexp.MustCompilePOSIX(`# ((:?Error|Warning):.*)$`)

	// a map from Line to error message
	var expectedErrors = make(map[int]string)
	var currentLine = 1
	{
		src := source
		for idx1 := strings.Index(src, "# Error:"); idx1 >= 0; idx1 = strings.Index(src, "# Error:") {
			idx2 := strings.IndexRune(src[idx1:], '\n')
			if idx2 >= 0 {
				idx2 = idx1 + idx2
			} else {
				idx2 = len(src)
			}
			currentLine += strings.Count(src[:idx1], "\n")

			expectedErrors[currentLine] = src[idx1+9 : idx2]
			src = src[idx2:]
		}
	}
	var expectedNumErrors = -1
	{
		src := source
		idx1 := strings.Index(src, "# NumErrors:")
		if idx1 < 0 {
			t.Fatalf("no '# NumErrors:' comment found")
		}
		idx1 += 12
		src = src[idx1:]
		idx2 := strings.IndexRune(src, '\n')
		if idx2 >= 0 {
			src = src[:idx2]
		}

		for len(src) > 0 && (strings.HasPrefix(src, " ") || strings.HasPrefix(src, "\t")) {
			src = src[1:]
		}

		expectedNumErrors, err = strconv.Atoi(src)
		if err != nil {
			t.Fatal(err)
		}
	}
	numErrors := len(expectedErrors)
	if numErrors != expectedNumErrors {
		t.Fatalf("%d number of errors doesn't match the expected %d number of errors", numErrors, expectedNumErrors)
	}
	pak := parsePackage(source, filename)
	validateSourceSet(pak.Source, pak)

	tc := NewTypeChecker(source, filename)
	tc.silentErrors = true
	_ = tc.TypeCheckPackage(pak, true)

	for _, error := range tc.errors {
		line, _, _ := LineColumnStr(source, error.node.GetSource())
		expectedError, ok := expectedErrors[line]
		if !ok {
			t.Fatalf("unexpected error at line %d:\n  %s", line, error.msg)
		}
		if expectedError != error.msg {
			t.Fatalf("errormessage does not match, got '%s' but expected '%s'\n", error.msg, expectedError)
		}
		delete(expectedErrors, line)
	}
	for line, expectedError := range expectedErrors {
		t.Fatalf("expected error '%s' at line %d not triggered by the compiler", expectedError, line)
	}
	return
}

const debugPrintParesedCode = false
const debugPrintTypecheckedCode = false
const debugPrintGeneratedCode = false

func normalTest(t *testing.T, filename string) {
	binaryAbsFilename, err := compile(filename)
	if err != nil {
		t.Fail()
	}
	err = exec.Command(binaryAbsFilename).Run()
	if err != nil {
		t.Fatal(err)
	}
}

func runTestFile(t *testing.T, filename string) {
	if strings.HasPrefix(filepath.Base(filename), "test_error_") {
		errorTest(t, filename)
	} else {
		normalTest(t, filename)
	}
}

func compile(filename string) (string, error) {
	bytes, err := os.ReadFile(filename)
	if err != nil {
		log.Fatal(err)
	}

	source := string(bytes)
	pak := parsePackage(source, filename)
	validateSourceSet(pak.Source, pak)
	if debugPrintParesedCode {
		fmt.Println("----------------------- parsed  code -----------------------")
		fmt.Println(AstFormat(pak))
	}
	tc := NewTypeChecker(source, filename)
	typedPak := tc.TypeCheckPackage(pak, true)
	if debugPrintTypecheckedCode {
		fmt.Println("--------------------- typechecked code ---------------------")
		fmt.Println(AstFormat(typedPak))
	}
	if len(tc.errors) > 0 {
		return "", fmt.Errorf("compilation has errors")
	}

	sourceCodeC := compilePackageToC(typedPak)
	if debugPrintGeneratedCode {
		fmt.Println("-------------------------- C code --------------------------")
		fmt.Println(sourceCodeC)
	}
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
	return binaryAbsFilename, nil
}

func compileAndRunFile(filename string, useExec bool) {
	binaryAbsFilename, err := compile(filename)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
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
