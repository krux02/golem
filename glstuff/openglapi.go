package main

import (
	"encoding/xml"
	"fmt"
	"log"
	"os"
)

type Enum struct {
	Name  string `xml:"name,attr"`
	Value string `xml:"value,attr"`
}

type Enums struct {
	XmlName   xml.Name `xml:"enums"`
	Comment   string   `xml:"comment,attr"`
	Namespace string   `xml:"name,attr"`
	Start     string   `xml:"start,attr"`
	End       string   `xml:"end,attr"`
	Vendor    string   `xml:"vendor,attr"`
	Enum      []Enum   `xml:"enum"`
}

type Group struct {
	Name string `xml:"name,attr"`
	Enum []Enum `xml:"enum"`
}

type Groups struct {
	XMLName xml.Name `xml:"groups"`
	Group   []Group  `xml:"group"`
}

type Type struct {
	XMLName xml.Name `xml:"type"`
	Name    string   `xml:"name"`
	Data    string   `xml:",chardata"`
}

type Types struct {
	XMLName xml.Name `xml:"types"`
	Comment string   `xml:",type"`
	Type    []Type   `xml:"type"`
}

type Param struct {
	PType string `xml:"ptype"`
	Name  string `xml:"name"`
	Len   string `xml:"len,attr"`
}

type Command struct {
	//Proto string  `xml:"proto,rawdata"`
	Name  string  `xml:"proto>name"`
	Param []Param `xml:"param"`
}

type Commands struct {
	XMLName   xml.Name  `xml:"commands"`
	Namespace string    `xml:"namespace,attr"`
	Command   []Command `xml:"command"`
}

type OpenGLRegistry struct {
	XMLName  xml.Name `xml:"registry"`
	Comment  string   `xml:"comment"`
	Types    Types    `xml:"types"`
	Groups   Groups   `xml:"groups"`
	Enums    []Enums  `xml:"enums"`
	Commands Commands `xml:"commands"`
}

func main() {
	file, err := os.ReadFile("gl.xml")
	if err != nil {
		log.Fatal(err)
	}

	var reg OpenGLRegistry

	err = xml.Unmarshal(file, &reg)
	if err != nil {
		log.Fatal(err)
	}

	//fmt.Printf("comment: %s\n", reg.Comment)
	//fmt.Printf("types: %#v\n", reg.Types.Type)
	for _, typ := range reg.Types.Type {
		fmt.Printf("%s\n", typ.Name)
	}
	// for _, group := range reg.Groups.Group {
	// 	fmt.Printf("%s: %d\n", group.Name, len(group.Enum))
	// 	for _, enum := range group.Enum {
	// 		fmt.Printf("  %s = %s\n", enum.Name, enum.Value)
	// 	}
	// }
	//
	for _, enums := range reg.Enums {
		if enums.Vendor == "" || enums.Vendor == "ARB" {
			fmt.Printf("vendor: %s\n", enums.Vendor)
			for _, it := range enums.Enum {
				fmt.Printf("  %s = %s\n", it.Name, it.Value)
			}
		}
	}

	for _, command := range reg.Commands.Command {
		fmt.Printf("proc %s(", command.Name)
		for _, param := range command.Param {
			fmt.Printf("%s: %s, ", param.Name, param.PType)
		}

		fmt.Printf(")\n")
	}
}
