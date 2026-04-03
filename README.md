# cps-obs

A C code obfuscator that uses **Static Single Assignment (SSA)** and **Continuation-Passing Style (CPS)** transformations to obfuscate control flow. It also supports **Control Flow Flattening (CFF)** as an alternative obfuscation technique.

The tool parses C source code, constructs a control flow graph, converts it to SSA form, and then applies CPS or CFF transformations to produce obfuscated C code that is functionally equivalent but significantly harder to reverse-engineer.

## Reference

Based on the research paper: [https://dl.acm.org/citation.cfm?id=3294083](https://dl.acm.org/citation.cfm?id=3294083)

## Building

### Using Cabal

```bash
cabal update
cabal build
```

### Using Stack

```bash
stack build
```

## Running

```bash
cabal run cps-obs -- <input.c> <output.c>
```

Or if built with stack:

```bash
stack exec cps-obs -- <input.c> <output.c>
```

## Configuration

cps-obs expects a `config.yaml` file in the current working directory. Platform-specific templates are provided: `config.yaml.linux`, `config.yaml.mac`, `config.yaml.windows`.

```yaml
# Location of the C preprocessor
gcc: gcc

# Functions to obfuscate with CPS transformation
# Use "." to obfuscate all functions
cps:
  - .

# Functions to obfuscate with CFF transformation
cff: []

# Blacklist (placeholder, currently has no effect)
blacklist: []
```

### gcc

Specifies the C preprocessor path. Examples by platform:

| Platform | Value |
|----------|-------|
| Linux | `gcc` |
| macOS (Homebrew) | `/usr/local/bin/gcc-8` |
| Windows (Visual Studio) | `cc` |

### cps and cff

Whitelists that control which functions are obfuscated and with which technique:

- **CPS (Continuation-Passing Style)** — transforms function control flow into continuation-passing form. Produces stronger obfuscation but with higher code size and runtime overhead.
- **CFF (Control Flow Flattening)** — flattens control flow into a switch-based dispatch loop. Produces more compact obfuscation with lower overhead.

Use `"."` to apply to all functions. Otherwise, list specific function names:

```yaml
# Apply CPS to all functions
cps:
  - .
cff: []
```

```yaml
# Apply CFF to specific functions only
cps: []
cff:
  - sort
  - search
```

```yaml
# Mix both techniques
cps:
  - fib
cff:
  - sort
```

The `main` function and `inline` functions are always excluded from obfuscation.

## Examples

### Example 1: Obfuscating iterative Fibonacci

**Input** (`test/fibiter.c`):
```c
#include <stdio.h>
#include <stdlib.h>

int fib(int x)
{
  int f1 = 1;
  int f2 = 1;
  for (int i=3; i<=x; i++)
    {
      int t = f1 + f2;
      f1 = f2;
      f2 = t;
    }
  return f2;
}

int main() {
  printf("%d\n", fib(10));
  return 0;
}
```

**Run:**
```bash
cabal run cps-obs -- test/fibiter.c fibiter_obfs.c
```

The `fib` function's control flow is transformed into CPS form while `main` is left unchanged. The obfuscated output is functionally equivalent — `fibiter_obfs.c` can be compiled and will produce the same result.

### Example 2: Obfuscating a sorting algorithm

**Input** (`test/sort.c`):
```c
#include <stdio.h>
#include <stdlib.h>

void sort(int a[], int size) {
  int i = 0;
  while (i < size) {
    int j = i+1;
    while (j < size) {
      if (a[j] < a[i]) {
        int t = a[i];
        a[i] = a[j];
        a[j] = t;
      }
      j++;
    }
    i++;
  }
  return;
}

int main()
{
  int a[] = {2,7,4,5,1,0};
  int size = 6;
  sort(a, size);
  for (int i = 0; i < size; i++)
    {
      printf("%d ", a[i]);
    }
  return 0;
}
```

**Run:**
```bash
cabal run cps-obs -- test/sort.c sort_obfs.c
```

### Example 3: Selective obfuscation with CFF

To obfuscate only the `sort` function using CFF instead of CPS, modify `config.yaml`:

```yaml
gcc: gcc
cps: []
cff:
  - sort
blacklist: []
```

Then run:
```bash
cabal run cps-obs -- test/sort.c sort_obfs.c
```

### Example 4: Obfuscating data structures

The tool handles complex C programs involving structs, pointers, and linked data structures:

```bash
cabal run cps-obs -- test/bst.c bst_obfs.c
cabal run cps-obs -- test/linkedlist.c linkedlist_obfs.c
cabal run cps-obs -- test/struct.c struct_obfs.c
```

## Benchmark: CPS vs CFF

Performance comparison of the two obfuscation techniques (ratio vs original — lower is better):

### Source and Object Code Size

| Test Case | CFF (src) | CPS (src) | CFF (obj) | CPS (obj) |
|-----------|-----------|-----------|-----------|-----------|
| fib | 1.04 | 1.31 | 1.00 | 1.20 |
| sort | 1.09 | 1.45 | 1.00 | 1.90 |
| linkedlist | 1.62 | 6.27 | 1.31 | 7.92 |
| bst | 1.52 | 4.65 | 1.31 | 5.46 |
| md5 | 1.41 | 1.84 | 1.88 | 6.66 |

### Runtime Overhead

| Benchmark | CFF | CPS |
|-----------|-----|-----|
| md5 | 1.04 | 2.11 |
| jpeg2ps | 1.00 | 1.10 |
| python2 | 1.01 | 1.06 |
| pigz | 1.08 | 1.22 |
| ffmpeg | 1.01 | 1.03 |

CFF generally produces lower overhead in both code size and runtime. CPS provides stronger obfuscation but incurs additional overhead for call stack space.

## Test Suite

The `test/` directory contains 33 C test programs covering various C constructs:

- **Basic**: `fib.c`, `fibiter.c`, `test.c`
- **Control flow**: `break.c`, `goto.c`, `switch.c`, `emptythen.c`, `whileififelse.c`
- **Data structures**: `struct.c`, `tree_struct.c`, `linkedlist.c`, `bst.c`
- **Arrays and pointers**: `local_array.c`, `arrayinitlist.c`, `deref_pointer.c`, `pointer_result_type.c`
- **Functions**: `func_call.c`, `static_inline.c`, `static_nonstatic.c`, `noreturn.c`
- **Algorithms**: `sort.c`, `search.c`, `ast.c`

## Troubleshooting

### Windows: assembly statement error

```
cps-obs.exe: ("C:/Program Files/Haskell Platform/.../include/_mingw.h": line 542):
assembly statement not supported.
```

Install Visual Studio and set `gcc: cc` in `config.yaml`.

### macOS: parse error in system headers

```
Parse Error: /usr/include/stdio.h:133: (column 19) [ERROR]  >>> Syntax Error !
```

Install GCC via Homebrew (`brew install gcc`) and update `config.yaml` to point to the Homebrew GCC path (e.g., `/usr/local/bin/gcc-13`).

### Ubuntu 17.10 with glibc 2.26

Not yet supported due to parser limitations.
