package main

import (
	"encoding/xml"
	"fmt"
	"log"
	"os"
	"slices"
	"strings"
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
	Name  string `xml:"name"`
	PType string `xml:"ptype"`
	Group string `xml:"group,attr"`
	Len   string `xml:"len,attr"`
}

type Command struct {
	//Proto string  `xml:"proto,rawdata"`
	Name string `xml:"proto>name"`
	Type string `xml:"proto>ptype"`
	//Proto string      `xml:"proto"`
	Param []Param     `xml:"param"`
	Alias []RefByName `xml:"alias"` // currently unused
}

type RefByName struct {
	Name string `xml:"name,attr"`
}

type Commands struct {
	XMLName   xml.Name  `xml:"commands"`
	Namespace string    `xml:"namespace,attr"`
	Command   []Command `xml:"command"`
}

type FeatureList struct {
	//XMLName xml.Name    `xml:"require"`
	Profile string      `xml:"profile,attr"`
	Comment string      `xml:"comment,attr"`
	Command []RefByName `xml:"command"`
	Enum    []RefByName `xml:"enum"`
}

type Feature struct {
	//XMLName xml.Name  `xml:"feature"`
	Api     string        `xml:"api,attr"`
	Name    string        `xml:"name,attr"`
	Number  string        `xml:"number,attr"`
	Require []FeatureList `xml:"require"`
	Remove  []FeatureList `xml:"remove"`
}

type Extension struct {
	Name      string        `xml:"name,attr"`
	Supported string        `xml:"supported"`
	Comment   string        `xml:"comment"`
	Require   []FeatureList `xml:"require"`
}

//type Extension struct{}

type OpenGLRegistry struct {
	XMLName   xml.Name    `xml:"registry"`
	Comment   string      `xml:"comment"`
	Types     Types       `xml:"types"`
	Groups    Groups      `xml:"groups"`
	Enums     []Enums     `xml:"enums"`
	Commands  Commands    `xml:"commands"`
	Feature   []Feature   `xml:"feature"`
	Extension []Extension `xml:"extension"`
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

	fmt.Printf("%s\n", strings.Join(strings.Split(reg.Comment, "\n"), "\n# "))

	// fmt.Printf("types: %#v\n", reg.Types.Type)
	// for _, typ := range reg.Types.Type {
	// 	fmt.Printf("type %s = %s\n", typ.Name, typ.Data)
	// }
	// for _, group := range reg.Groups.Group {
	// 	fmt.Printf("%s: %d\n", group.Name, len(group.Enum))
	// 	for _, enum := range group.Enum {
	// 		fmt.Printf("  %s = %s\n", enum.Name, enum.Value)
	// 	}
	// }
	// for _, enums := range reg.Enums {
	// 	fmt.Printf("# vendor: %s\n", enums.Vendor)
	// 	for _, it := range enums.Enum {
	// 		fmt.Printf("  %s = %s\n", it.Name, it.Value)
	// 	}
	// }

	currentApi := "gl"

	var commands []string
	var enums []string

	for _, feature := range reg.Feature {
		if feature.Api == currentApi {
			// fmt.Printf("name: %s, api: %s, number: %s\n", feature.Name, feature.Api, feature.Number)
			for _, requireSection := range feature.Remove {
				var removeCommands []string

				for _, command := range requireSection.Command {
					removeCommands = append(removeCommands, command.Name)
				}
				slices.Sort(removeCommands)
				var removeEnums []string
				for _, enum := range requireSection.Enum {
					removeEnums = append(removeEnums, enum.Name)
				}
				slices.Sort(removeEnums)

				j := 0
				for i := range commands {
					command := commands[i]
					if _, remove := slices.BinarySearch(removeCommands, command); !remove {
						commands[j] = command
						j += 1
					}
				}
				commands = commands[0:j]

				j = 0
				for i := range enums {
					enum := enums[i]
					if _, remove := slices.BinarySearch(removeCommands, enum); !remove {
						enums[j] = enum
						j += 1
					}
				}
				enums = enums[0:j]
			}

			for _, requireSection := range feature.Require {
				if requireSection.Profile != "compatibility" {
					for _, command := range requireSection.Command {
						commands = append(commands, command.Name)
					}
					for _, enum := range requireSection.Enum {
						enums = append(enums, enum.Name)
					}
				}
			}
		}
	}

	slices.Sort(commands)
	{
		// remove duplicades
		j := 0
		for i := range commands {
			if i == 0 || commands[i] != commands[i-1] {
				commands[j] = commands[i]
				j += 1
			}
		}
		commands = commands[0:j]
	}
	slices.Sort(enums)
	{
		// remove duplicades
		j := 0
		for i := range enums {
			if i == 0 || enums[i] != enums[i-1] {
				enums[j] = enums[i]
				j += 1
			}
		}
		enums = enums[0:j]
	}

	for _, regEnums := range reg.Enums {
		for _, enum := range regEnums.Enum {
			if _, found := slices.BinarySearch(enums, enum.Name); found {
				fmt.Printf("const %s:u32 = %s\n", enum.Name, enum.Value)
			}
		}
	}

	for _, command := range reg.Commands.Command {
		if _, found := slices.BinarySearch(commands, command.Name); found {
			fmt.Printf("proc \"importc\" %s(", command.Name)
			for i, param := range command.Param {
				if i != 0 {
					fmt.Printf(", ")
				}
				if param.Name == "type" {
					fmt.Printf("typ")
				} else {
					fmt.Printf("%s", param.Name)
				}
				fmt.Printf(": ")
				if param.Len != "" {
					fmt.Printf("pointer")
				} else {
					fmt.Printf("%s", param.PType)
				}
			}
			if command.Type != "" {
				fmt.Printf("): %s\n", command.Type)
			} else if command.Name[0:5] == "glMap" {
				fmt.Printf("): pointer\n")
			} else {
				fmt.Printf("): void\n")
			}
		}
	}

	for i, command := range commands {
		//fmt.Printf("commad: %s\n", command)
		ok := slices.ContainsFunc(reg.Commands.Command, func(c Command) bool {
			return c.Name == command
		})

		if !ok {
			fmt.Printf("not found: %s\n", command)
		}
		if i != 0 && command == commands[i-1] {
			fmt.Printf("found double: %s\n", command)
		}
	}
	// fmt.Printf("total enums: %d (%d)\n", len(enums))
	// fmt.Printf("total commands: %d\n", len(commands))

}
