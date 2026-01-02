# zenon: C build tool of the 21st century

An experimental build system and script runner for the C family of languages (and Haskell)

## Configuration

`zenon` uses yaml for configuration, to get started create a "zenon.yaml" file in the root of your project:

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

## Running

```sh
$ zenon build
```

will build all targets

and

```sh
$ zenon build example
```

will build just the `example` target.

For full command line commands and flags, see the output of `zenon --help`

