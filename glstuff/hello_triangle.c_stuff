#include <GL/gl.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_events.h>
#include <SDL2/SDL_video.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

int main(int argc, const char* argv[]) {

  SDL_Window* window = SDL_CreateWindow("hello triangle", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 640, 480, SDL_WINDOW_OPENGL);
  assert(window);
  /* SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4); */
  /* SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 6); */
  /* SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_DEBUG_FLAG); */
  /* SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE); */
  SDL_GLContext context = SDL_GL_CreateContext(window);
  assert(context);

  bool running;

  while(running) {

    SDL_Event event;
    while(SDL_PollEvent(&event)) {
      switch (event.type) {
        case SDL_QUIT:
          running = false;
          break;
        case SDL_KEYDOWN:
          if (event.key.keysym.scancode == SDL_SCANCODE_ESCAPE) {
            running = false;
          }
          break;
      }
    }

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glBegin(GL_TRIANGLES);
    glColor3f(1,0,0);
    glVertex2f(-1,-1);
    glColor3f(0,1,0);
    glVertex2f(1,-1);
    glColor3f(0,0,1);
    glVertex2f(0,1);
    glEnd();

    SDL_GL_SwapWindow(window);
  }

  return 0;
}
