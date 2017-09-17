# Has5
### Description:
Has5 is an implementation of the classic 5 in a Row game. It's written using gtk2hs for the GUI, the game loop and the updates to the GUI are concurrent. The existing AI is designed with parallelism but a new AI can be added easily by adding it to the possible functions in Logic. 

### Build on Windows:
If you don't have gtk+ installed or the build still fails the best way for me to get it working was by following the first step of the Leksah "Building from source" guide.
I then only needed to add an environment variable for "pathToMysysFolder\mysys64\mingw64\bin" otherwise i got an error when trying to build with stack.
On further errors the rest of the Leksah guide might be a good place to start.
