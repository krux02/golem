package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
	"syscall"
)

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
	pak := parsePackage(source, filename)

	validateSourceSet(pak.source, pak)

	tc := NewTypeChecker(source, filename)
	_ = tc.TypeCheckPackage(pak)

	for _, error := range tc.errors {
		line, _, _ := LineColumnStr(source, error.node.Source())
		expectedError, ok := expectedErrors[line]
		if !ok {
			panic(fmt.Errorf("unexpected error at line %d, proper logging not implemented", line))
		}
		if expectedError != error.msg {
			panic(fmt.Errorf("errormessage does not match, got '%s' but expected '%s'\n", error.msg, expectedError))
		}
		delete(expectedErrors, line)
	}
	for line, expectedError := range expectedErrors {
		panic(fmt.Errorf("expected error '%s' at line %d not triggered by the compiler", expectedError, line))
	}
	fmt.Printf("errortest successful")
}

const debugPrintParesedCode = false
const debugPrintTypecheckedCode = false
const debugPrintGeneratedCode = false

func runAllTests() {
	testpath, _ := filepath.Abs("tests")
	recursiveTestScanAndRun(testpath)
}

func recursiveTestScanAndRun(dir string) {
	files, err := ioutil.ReadDir(dir)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	testFails := 0
	testPasses := 0

	for _, file := range files {
		name := file.Name()
		if file.IsDir() {
			subDir := path.Join(dir, name)
			recursiveTestScanAndRun(subDir)
		} else if strings.HasPrefix(name, "test_") && strings.HasSuffix(name, ".golem") {
			absName := path.Join(dir, name)
			binaryAbsFilename, err := compile(absName)
			if err != nil {
				fmt.Printf("test compilation fail: %s", name)
				println(err)
				testFails += 1
				continue
			}
			err = exec.Command(binaryAbsFilename).Run()
			if err != nil {
				fmt.Printf("test execution fail: %s", name)
				println(err)
				testFails += 1
				continue
			}
			testPasses += 1
			fmt.Printf("test passed: %s\n", name)
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
	if !filepath.IsAbs(filename) {
		panic("filename must be absolute")
	}

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
