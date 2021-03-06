# Floorplan compiler

A language for specifying the layout of a one-dimensional address space, particularly
for garbage collectors and manual memory managers written in Rust and C/C++.

## Building and running

Floorplan is written in Haskell and must be [built with either
stack](https://docs.haskellstack.org/en/stable/README/) or
[cabal](haskell.org/cabal). If you just want to build and install Floorplan
globally on your system you can do the following:

```bash
$ cabal install flp
```

This will pull the latest stable release of Floorplan from
[Hackage](hackage.haskell.org/package/flp) and install the `flp` executable
globally. If instead you wish to make modifications to the compiler or to
reference it directly from another Haskell project, you can pull the
master branch of this repo and build the project like follows:

```bash
$ git clone --branch master https://github.com/RedlineResearch/floorplan.git
$ cd floorplan && stack build
...
Completed 2 action(s).
```

At which point you can compile, for example, the file `examples/immix/layout.flp` with
the `build-immix` script:

```bash
$ ./build-immix
...
   Compiling immix_rust v0.0.1 (/home/karl/w/flp/examples/immix)
   Finished dev [unoptimized + debuginfo] target(s) in 4.56s
```

This script ensures the Floorplan compiler is built, installs it for your current
user, and then builds the Immix project which itself invokes the Floorplan compiler
to build the file `examples/immix/src/heap/layout.flp`.

In order to run the compiler against some other `.flp` file, the compiler can
be run directly as follows:

```bash
stack exec flp [path/to/layout.flp] [path/to/generated.rs]
```

### Rust

Note that to build a Rust file generated in this manner,
you must include the `flp-framework` to your Cargo dependencies, and `flp-compiler`
to your cargo build-dependencies. The latter is simply a wrapper for calling out
to the (already stack-installed) flp compiler, and the framework crate contains
necessary macros and address types that generated Rust code uses.

The skeleton of a Rust cargo project is given in the `genrs/` directory of this
repo, which can be copied over and modified to support the needs of a memory
manager other than immix-rust.

### C/C++

C/C++ output is under active development. Regardless of which output type is chosen
(`.c`, `.h`, or `.hpp`), the compiler will output the same C-like code. This is
expected to work with a recent version of the Clang compiler, but will likely work
just as fine under a modern verison of GCC.

When reporting an issue with the C output mode of the Floorplan compiler, please
check first or have a reasonable belief that the same issue happens with Clang.
Modifications to the compiler which tailor the output to different C compilers are
more than welcome, but we intend to primarily support Clang as a backend toolchain.

## Dependencies

A customized version of the [language-rust](https://github.com/harpocrates/language-rust)
package is included in the `deps/` directory of this repo, which adds support for
directly splicing of host language expressions into quasiquoted Rust code. This is the
mechanism by which Floorplan generates Rust code.

## Testing and contributing

If you want to help maintain or contribute new code to this project, feel free to
make a pull request or better yet start an issue in the the Issues tracker so that
you can get our feedback along the way. A number of avenues for work on the compiler
exist, including but not limited to:

- More example Rust allocators implemented with a Floorplan layout.
- Rust templates for allocating on alignment boundaries.
- Extensively document the interfaces generated.
- Better error messages.
- Integrating the core semantics (`app/semantics.hs`) directly into the project
  `src/` hierarchy.
- Calling out to a SMT library to verify alignment and size constraints.
- Targeting both C and Rust.
- Support for non-64-bit architectures.
- Generating debugging assertions.
- Dynamic tracking of type information for each piece of the heap.
- Cleaning up the dependencies by integrating the Rust splicing support directly
  into [the upstream repository](https://github.com/harpocrates/language-rust),
  e.g. as a separate quasiquoter.
- Generate cargo-based Rust documentation alongside generated functions, indicating
  why a certain function was generated and how it might be used.
- Repairing the Coq proofs in the `proofs/*.v` files.
- Better Rust integration and downstream crates.

