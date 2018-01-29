# Building 
```bash
$ cabal configure
$ cabal build
```
# Running
```bash
./dist/build/cpp-obs/cpp-obs test/fibiter.c fibiter_obfs.c
```
# Configuration
cpp-obs expects a config.yaml to be placed in the currnt working directory. 
## gcc 
gcc defines the preprocessor. For instance, for linux, 
```
- gcc: gcc
```
for Mac OSX, install gcc via homebrew and
```
- gcc : /usr/local/bin/gcc-7
```
## whitelist 
whitlelist defines the list of functions to be obfuscated. If whitelist is left as an empty list, all functions will be obfuscated, except for main function and the inlined functions. 
## blacklist
blacklist currently serves as a place holder for debugging, it has no effect.

For example, refer to config.yaml or test/python_ast_config.yaml. 
# Trouble Shooting
## In windows, we probably encounter 
```
.\dist\build\cpp-obs\cpp-obs.exe test\fibiter.c fibiter_obs.c
cpp-obs.exe: ("C:/Program Files/Haskell Platform/8.2.1/mingw/x86_64-w64-mingw32/include/_mingw.h": line 542):
asmbly statement not supported.
CallStack (from HasCallStack):
  error, called at Main.hs:41:51 in main:Main
```
## Mac OS X, we might encounter
```
Parse Error: /usr/include/stdio.h:133: (column 19) [ERROR]  >>> Syntax Error !
  Syntax error !
  The symbol `_close' does not fit here.
```
Install gcc from brew.
```
$ brew install gcc 
```
Then set the gcc in config.yaml as /usr/local/bin/gcc-7
