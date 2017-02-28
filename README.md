# Porting Prezi to Elm in 99 lines of code

This is the source code to a talk, I gave at [TopConf 2017](https://www.topconf.com/conference//linz-2017/talk/porting-prezi-to-elm-in-100-lines-of-code/) in Linz, Austria. It implements a simplified version of [Prezi](https://prezi.com) in [Elm](http://elmlang.org) based on SVG. 

Eventually I will release a more elaborated version of this code with the ability to upload custom SVG files with presentation contents AND the frames.

You might also be interested in [Dive](https://github.com/myrho/dive), another approach based on HTML5 Canvas.

## Installation

You need to [install Elm](https://guide.elm-lang.org/install.html) before.

Then run:

    elm make Main.elm --output index.html

Since there are no additional frames defined in the `init` function of the code, you might want to add some on your own!

## License

BSD-3
