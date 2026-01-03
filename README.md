# zenon: C build tool of the 21st century

An experimental build system and script runner for languages and compilers supporting [separate compilation](https://www.cs.sjsu.edu/~pearce/modules/lectures/cpp/advanced/SeparateCompilation.htm).

Out of the box, `zenon` supports [clang](https://clang.llvm.org/) (C), [clang++](https://clang.llvm.org/) (C++), [flang](https://flang.llvm.org/docs/) (Fortran),
[ispc](https://ispc.github.io/) (ISPC), [ghc](https://www.haskell.org/) (Haskell), [mlton](http://www.mlton.org) (SML) and [patscc](http://www.ats-lang.org) (ATS2).

Additional compilers can be configured in the `zenon.yaml` file. 

## Installation

Using [opam](https://opam.ocaml.org/):

```sh
$ opam pin add -y git+https://codeberg.org/zshipko/zenon.git
```

Or [dune](https://dune.build/):

```sh
$ git clone https://codeberg.org/zshipko/zenon.git
$ cd zenon
$ dune pkg lock
$ dune build
$ dune install
```

## Running

```sh
$ zenon build
```

will build all targets in your `zenon.yaml` file

and

```sh
$ zenon build example
```

will build just the `example` target.

For full command line commands and flags, see the output of `zenon --help`

`zenon` can also detect and build source files without a configuration file:

```sh
$ zenon build -o example
```

When run without a `zenon.yaml` file, zenon will:
- automatically discover source files in the current directory
- detect the appropriate compiler based on file extensions
- build an executable with a default name based on the directory or detected sources

You can also specify source files directly via the command line using the `-f` flag:

```sh
$ zenon build -f main.c -f utils.c
```

Glob patterns are also supported:

```sh
$ zenon build -f 'src/*.c'
```


## Configuration

`zenon` uses yaml for configuration, to get started create a `zenon.yaml` file in the root of your project:

```yaml
build:
  - target: example
    files:
      - 'src/*.c'
flags:
  - lang: c
    compile:
      - "-std=c23"
```

The above configuration will compile an executable named `example` from all the C source files in `src`

### Building a static library

```yaml
build:
  - target: libutils.a
    files:
      - 'src/*.c'
```

### Using dependencies

```yaml
build:
  - target: libutils.a
    files:
      - 'utils/*.c'

  - target: myapp
    files:
      - 'src/*.c'
    depends-on: [libutils.a]
```

### Sharing files and flags

```yaml
files:
  - 'common/*.c'

flags:
  - lang: c
    compile: ["-Wall", "-O2"]

build:
  - target: app1
    files:
      - 'app1/*.c'

  - target: app2
    files:
      - 'app2/*.c'
```

Top-level `files` and `flags` apply to all targets.

### Using pkg-config

```yaml
build:
  - target: myapp
    files:
      - 'src/*.c'
    pkg: [sdl2, opengl]
```

### Running scripts

```yaml
build:
  - name: codegen
    script: python generate.py

  - target: myapp
    depends-on: [codegen]
    files:
      - 'src/*.c'
```

## Field Reference

### Top-level fields

- `build` - List of build targets (required)
- `files` - Source files to include in all targets
- `flags` - Compiler/linker flags for all targets
- `compilers` - Custom compiler configurations
- `linkers` - Custom linker configurations
- `ignore` - File patterns to exclude from all targets
- `pkg` - pkg-config packages for all targets

### Build target fields

- `target` - Output file name (e.g. `myapp`, `libfoo.a`)
- `name` - Target name for `zenon build <name>` (defaults to target)
- `files` - Source files for this target (supports globs like `*.c`, `**/*.c`)
- `root` - Root directory for source files (defaults to `.`)
- `depends-on` - List of targets that must build first
- `linker` - Linker to use (auto-detected from target name and source files if not specified)
- `script` - Shell script to run instead of compiling
- `after` - Shell script to run after building
- `if` - Only build if this shell command succeeds
- `pkg` - pkg-config packages for this target
- `ignore` - File patterns to exclude from this target
- `hidden` - Don't build unless explicitly named
- `disable_cache` - Rebuild every time
- `flags` - Compiler/linker flags for this target
- `compilers` - List of compiler names to use (e.g., `[clang, clang++]`)

### Flag configuration

```yaml
flags:
  - lang: c
    compile: ["-std=c23", "-Wall"]
    link: ["-lm"]
```

- `lang` - Language (file extension without dot: `c`, `cpp`, `hs`, etc.)
- `compile` - Flags passed to compiler
- `link` - Flags passed to linker

### Custom compilers/linkers

It is possible to define custom compilers at the top level, then reference by name in build targets:

```yaml
compilers:
  - name: mycc
    ext: [myc]
    command: ["mycc", "#flags", "-o", "#output"]

linkers:
  - name: mycc
    link-type: exe
    command: ["mycc", "--link", "#flags", "-o", "#output", "#objs"]

build:
  - target: example
    compilers: [mycc]
```

In the example above, `#objs`, `#flags` and `#output` are template arguments and will be replaced with the object paths, flags and output path.

- `name` - Compiler name
- `ext` - File extensions this compiler handles
- `command` - Command template (`#flags`, `#output`, `#objs` are substituted)
- `link-type` - Specifies the type of linker (should only be set when defining linkers)

