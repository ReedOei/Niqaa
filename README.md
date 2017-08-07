# Niqaa

Niqaa is a game about space combat. And, for the moment, that's pretty much all it is.

# Installation

You need Haskell, Stack, and SDL2 installed to build Niqaa.
Eventually, I hope to create releases but first there must be something to release, eh?

If you are lacking SDL2, then before running `stack build`, you must install it.
If you are on Ubuntu, then run:

`sudo apt install libsdl2-dev`

If you are running Mac OS X with Homebrew installed, you can run:

`brew install sdl2`

You may also need to run

`brew install pango`

~~~
git clone https://github.com/Kytuzian/Niqaa
cd Niqaa
stack install gtk2hs-buildtools
stack build
stack exec Niqaa
~~~

# Data Files

The game runs off of data files which are stored in the app/data/ directory.
You can write your own and the game will read them if you do it properly.
Just follow the pattern of the files that are already there.
If you would like to submit any of your creations, just let me know.

# Contributors
- Reed Oei
- Meg O'Brien

