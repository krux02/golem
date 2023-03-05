package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
)

func Unwrap[T any](arg T, err error) T {
	if err != nil {
		panic(err)
	}
	return arg
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("not enough arguments")
		flag.PrintDefaults()
		os.Exit(1)
	}

	command := os.Args[1]
	args := os.Args[2:]
	switch command {
	case "build":
		buildCmd := flag.NewFlagSet("build", flag.ExitOnError)
		buildCmd.Parse(args)
		compileAndRunFile(args[0], true)
	case "test":
		var tests []string
		if len(args) > 0 {
			tests = args
		} else {
			execDir := filepath.Dir(Unwrap(os.Executable()))
			pattern := filepath.Join(execDir, "tests/test_*.golem")
			currentDir := Unwrap(filepath.Abs("."))
			pattern = Unwrap(filepath.Rel(currentDir, pattern))
			tests = Unwrap(filepath.Glob(pattern))
		}
		fmt.Printf("%+v\n", tests)
		runTests(tests)
	default:
		fmt.Println("expected 'build', 'test' subcommands")
		os.Exit(1)
	}

}
