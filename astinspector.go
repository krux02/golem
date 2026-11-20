package main

// this function intends to run an interactive visualizer to inspect the ast
// simply for debugging purpose. Maybe it will be used for something else in the
// future. The idea is, when this is called, something is wrong in the compiler.

import (
	"reflect"
	"github.com/Zyko0/go-sdl3/sdl"
	"unicode/utf8"
)

const charSize = sdl.DEBUG_TEXT_FONT_CHARACTER_SIZE
const indentSize = 16 // indentation in pixels not spaces



func RenderTextColored(renderer *sdl.Renderer, x,y float32, text string, bg, fg sdl.Color) {
	
	var bgRect = sdl.FRect{
    X : x,
    Y : y,
    W : float32(utf8.RuneCountInString(text) * charSize),
    H : charSize,
	}

	/* background */
	
	renderer.SetDrawColor(bg.R, bg.G, bg.B, bg.A);
	renderer.RenderFillRect(&bgRect)
	renderer.SetDrawColor(fg.R, fg.G, fg.B, fg.A);
	renderer.DebugText(x, y, text);

}

var Color_FieldName sdl.Color = sdl.Color{255,255,0,255}
var Color_TypeName sdl.Color = sdl.Color{255,0,255,255}
var Color_BasicValue sdl.Color = sdl.Color{0,255,255,255}

type RecursiveNodeInspector struct {
	renderer *sdl.Renderer
	x,y float32
}

func (self *RecursiveNodeInspector) SetDrawColor(renderer *sdl.Renderer, c sdl.Color) {
	
	renderer.SetDrawColor(c.R, c.G, c.B, c.A)
}



// return the number of lines printed
func (self *RecursiveNodeInspector) RecursiveNodeInspection(node reflect.Value) {

	switch node.Kind() {
	case reflect.Pointer:
		self.RecursiveNodeInspection(node.Elem())
	case reflect.Slice:
		for i := 0; i < node.Len(); i++ {			
			self.RecursiveNodeInspection(node.Index(i))
		}
	case reflect.Struct:
		for field, value := range node.Fields() {
			//renderer.SetDrawColor(r uint8, g uint8, b uint8, a uint8)
			self.renderer.SetDrawColor(renderer, Color_FieldName)
			renderer.DebugText(x, y, field.Name)
			y += charSize
			result += RecursiveNodeInspection(renderer, value, x + indentSize, y + float32(result) * charSize)
			// SetDrawColor(renderer, Color_BasicValue)
			// renderer.DebugText(x + 16, y, value.String())
			// y += charSize
			// renderer.DebugTextFormat(x float32, y float32, format string, values ...any)
			// field.Name.
		}
	default:
		renderer.DebugText(x,y, "don't know")
		return 1
	}

	return result;
}

func AstInspector(expr any) {
	// defer binsdl.Load().Unload()
	path := sdl.Path()
	sdl.LoadLibrary(path)
	defer sdl.Quit()

	if err := sdl.Init(sdl.INIT_VIDEO); err != nil {
		panic(err)
	}

	window, renderer, err := sdl.CreateWindowAndRenderer("AstInspector", 900, 900, 0)
	if err != nil {
		panic(err)
	}

	defer renderer.Destroy()
	defer window.Destroy()
	
	var running = true
	for running {
		var event sdl.Event
		_, mouseX, mouseY := sdl.GetMouseState()

		for sdl.PollEvent(&event) {
			switch event.Type {
			case sdl.EVENT_QUIT:
				running = false;
			case sdl.EVENT_KEY_DOWN:
				keyEvent := event.KeyboardEvent()
				if keyEvent.Scancode == sdl.SCANCODE_ESCAPE {
					running = false;
				}
			}
		}

		renderer.SetDrawColor(0, 0, 0, 255)
		renderer.Clear()
		renderer.SetDrawColor(255, 255, 255, 255)
		// renderer.DebugText(50, 50, sdl.Path())

		RecursiveNodeInspection(renderer, reflect.ValueOf(expr), 50, 50)
		
		renderer.DebugTextFormat(mouseX, mouseY, "x: %f y: %f", mouseX, mouseY)

		//RenderTextColored(renderer, mouseX, mouseY, "test 123 äöü ß ][", sdl.Color{255,123,0,255}, sdl.Color{15,22,147,255})
		renderer.Present()
	}
}

