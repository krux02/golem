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

func main() {
	var filename string
	if len(os.Args) == 2 {
		filename = os.Args[1]
	} else {
		panic("program needs one argument only, the input file")
	}
	filename, err := filepath.Abs(filename)
	if err != nil {
		panic(err)
	}
	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	fmt.Println("----------------------- parsed  code -----------------------")
	pak := parsePackage(string(bytes), filename)
	validateSourceSet(pak.source, pak)
	fmt.Println(AstFormat(pak))
	fmt.Println("--------------------- typechecked code ---------------------")
	tc := NewTypeChecker(string(bytes), filename)
	typedPak := tc.TypeCheckPackage(pak)
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
	err = cmd.Run()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("=========================== exec ===========================")
	// exec should not return
	log.Fatal(syscall.Exec(binaryAbsFilename, nil, nil))
}
