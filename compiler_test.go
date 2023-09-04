package main

import (
	"path/filepath"
	"testing"
)

func TestMain(t *testing.T) {
	testFiles := Must(filepath.Glob("tests/test_*.golem"))
	if len(testFiles) == 0 {
		t.Errorf("No tests found\n")
		return
	}
	for _, testFile := range testFiles {
		t.Run(testFile, func(t *testing.T) {
			runTestFile(t, testFile)
		})
	}
}
