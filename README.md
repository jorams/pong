# Multiplayer Pong

This is a simple implementation of Pong played on two different computers over the network. Each instance displays only half of the playing field. The "right side" connects to the left side.

## Dependencies

- [glop](https://github.com/patzy/glop) (MIT)
- [cl-opengl](http://www.common-lisp.net/project/cl-opengl/) (BSD)
- [let+](https://github.com/tpapp/let-plus) (Boost)
- [usocket](http://common-lisp.net/project/usocket/) (MIT)
- [nibbles](https://github.com/froydnj/nibbles/) (BSD)

## Quickstart

To start the game put this repository somewhere [Quicklisp](http://www.quicklisp.org/) can find it and execute the following in the REPL on the server (left) side:

```lisp
(ql:quickload :pong)
(pong:start :left)
```

Then execute the following on the client (right) side:

```lisp
(ql:quickload :pong)
(pong:start :right :host "ip.of.left.side")
```

The game should start playing. Note that an aspect ratio of 16:9 is expected. By default the game will start full screen, meaning a screen with a different aspect ratio will deform the display. If it looks too bad you can pass `:fullscreen nil` to the `START` function seen above, which will result in a window of 800x450.

## License

    Copyright (c) 2014 Joram Schrijver

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
