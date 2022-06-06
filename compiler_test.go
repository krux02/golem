package main

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"strings"
	"testing"
)

const testDir = "tests"

func TestMain(t *testing.T) {
	files, err := ioutil.ReadDir(testDir)
	if err != nil {
		t.Fatal(err)
	}
	for _, file := range files {
		name := file.Name()
		if strings.HasSuffix(name, ".golem") {
			fmt.Printf("matching file %s\n", name)
			compileAndRunFile(filepath.Join(testDir, name))
		}
	}
}
