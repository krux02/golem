package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
)

func errorTest(filename string) error {
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return err
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
			return fmt.Errorf("no '# NumErrors:' comment found")
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
			return err
		}
	}
	numErrors := len(expectedErrors)
	if numErrors != expectedNumErrors {
		return fmt.Errorf("%d number of errors doesn't match the expected %d number of errors", numErrors, expectedNumErrors)
	}
	pak := parsePackage(source, filename)
	validateSourceSet(pak.source, pak)

	tc := NewTypeChecker(source, filename)
	tc.silentErrors = true
	_ = tc.TypeCheckPackage(pak)

	for _, error := range tc.errors {
		line, _, _ := LineColumnStr(source, error.node.Source())
		expectedError, ok := expectedErrors[line]
		if !ok {
			return fmt.Errorf("unexpected error at line %d, proper logging not implemented", line)
		}
		if expectedError != error.msg {
			return fmt.Errorf("errormessage does not match, got '%s' but expected '%s'\n", error.msg, expectedError)
		}
		delete(expectedErrors, line)
	}
	for line, expectedError := range expectedErrors {
		return fmt.Errorf("expected error '%s' at line %d not triggered by the compiler", expectedError, line)
	}
	return nil
}

const debugPrintParesedCode = false
const debugPrintTypecheckedCode = false
const debugPrintGeneratedCode = false

func normalTest(filename string) error {
	binaryAbsFilename, err := compile(filename)
	if err != nil {
		return err
	}
	return exec.Command(binaryAbsFilename).Run()
}

func runTestFile(filename string) error {
	if strings.HasPrefix(filepath.Base(filename), "test_error_") {
		return errorTest(filename)
	} else {
		return normalTest(filename)
	}
}

func runTests(testFiles []string) {
	testFails := 0
	testPasses := 0

	for _, testFile := range testFiles {
		err := runTestFile(testFile)
		if err != nil {
			testFails += 1
			fmt.Printf("test Failed: %s\n", testFile)
			fmt.Println(err)
		} else {
			testPasses += 1
			fmt.Printf("test Passed: %s\n", testFile)
		}
	}

	if testFails > 0 {
		fmt.Printf("testFails: %v\n", testFails)
		fmt.Printf("testPasses: %v\n", testPasses)
		os.Exit(1)
	}

	fmt.Printf("testPasses: %v\n", testPasses)
}

func compile(filename string) (string, error) {
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Printf("error: %s\n", err.Error())
		return "", err
	}

	source := string(bytes)
	pak := parsePackage(source, filename)
	validateSourceSet(pak.source, pak)
	if debugPrintParesedCode {
		fmt.Println("----------------------- parsed  code -----------------------")
		fmt.Println(AstFormat(pak))
	}
	tc := NewTypeChecker(source, filename)
	typedPak := tc.TypeCheckPackage(pak)
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
