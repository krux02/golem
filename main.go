package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
)

func Must[T any](arg T, err error) T {
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
		for _, arg := range args {
			tests = append(tests, Must(filepath.Abs(arg)))
		}
		execDir := filepath.Dir(Must(os.Executable()))
		os.Chdir(execDir)

		if len(tests) == 0 {
			tests = globForTests()
		}
		fmt.Printf("%+v\n", tests)

		// TODO, this doesn't work
		runTests(nil, tests)
	default:
		fmt.Println("expected 'build', 'test' subcommands")
		os.Exit(1)
	}

}
