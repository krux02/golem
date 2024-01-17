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

			expectedErrors[currentLine] = strings.TrimSpace(src[idx1+8 : idx2])
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

	tokenizer := NewTokenizer(source, filename)
	tokenizer.silentErrors = true
	pak, parseErrors := parsePackage(tokenizer)
	validateSourceSet(pak.Source, pak)
	for _, error := range parseErrors {
		line, _, _ := LineColumnStr(source, error.code.value)
		expectedError, ok := expectedErrors[line]
		if !ok {
			t.Fatalf("unexpected error at line %d:\n  %s", line, error.msg)
		}
		if expectedError != error.msg {
			t.Fatalf("errormessage does not match, got '%s' but expected '%s'\n", error.msg, expectedError)
		}
		delete(expectedErrors, line)
	}

	tc := NewTypeChecker(source, filename)
	tc.silentErrors = true
	_ = TypeCheckPackage(tc, pak, true)

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

func normalTest(t *testing.T, filename string) {
	binaryAbsFilename, err := compile(filename)
	if err != nil {
		t.Fatal(err)
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

func compileFileToPackage(filename string, requiresMain bool) (packageDef *TcPackageDef, err error) {
	bytes, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	source := string(bytes)
	tokenizer := NewTokenizer(source, filename)
	pak, parseErrors := parsePackage(tokenizer)
	if len(parseErrors) > 0 {
		return nil, fmt.Errorf("file '%s' has parsing errors", filename)
	}
	tc := NewTypeChecker(source, filename)
	packageDef = TypeCheckPackage(tc, pak, requiresMain)
	if len(tc.errors) > 0 {
		err = fmt.Errorf("package %s has errors", packageDef.Name)
	}

	return packageDef, err
}

func compile(filename string) (string, error) {

	tempDir := path.Join(os.TempDir(), "golem")
	base := filepath.Base(filename)
	base, hasFileEnding := strings.CutSuffix(base, ".golem")
	if !hasFileEnding {
		return "", fmt.Errorf("Input file name '%s' must end on .golem", filename)
	}
	if baseNameError := IsValidIdentifier(base); baseNameError != "" {
		return "", fmt.Errorf("%s", baseNameError)
	}

	typedPak, err := compileFileToPackage(filename, true)
	if err != nil {
		return "", err
	}
	typedPak = cgenprepass(typedPak).(*TcPackageDef)
	sourceCodeC := compilePackageToC(typedPak)

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
	args := []string{}
	args = append(args, typedPak.CFlags...)
	args = append(args, absFilename, "-o", binaryAbsFilename)
	cmd := exec.Command("gcc", args...)
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
		log.Fatal(syscall.Exec(binaryAbsFilename, argv, os.Environ()))
		return // dead code
	} else {
		err = exec.Command(binaryAbsFilename, argv...).Run()
		if err != nil {
			log.Fatal(err)
		}
		return
	}
}
