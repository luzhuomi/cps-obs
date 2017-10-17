# cpp-obs
# Building 
```bash
$ cabal configure
$ cabal build
```
# Running
```bash
./dist/build/cpp-obs/cpp-obs test/fibiter.c fibiter_obfs.c
```
# Trouble Shooting
In windows, we probably encounter 
```
.\dist\build\cpp-obs\cpp-obs.exe test\fibiter.c fibiter_obs.c
cpp-obs.exe: ("C:/Program Files/Haskell Platform/8.2.1/mingw/x86_64-w64-mingw32/include/_mingw.h": line 542):
asmbly statement not supported.
CallStack (from HasCallStack):
  error, called at Main.hs:41:51 in main:Main
```

Kindly comment away the #include ... from the source file and add them back to the obfuscated files manually, until we figure out how to resolve it.
