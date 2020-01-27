//! # The Official Floorplan Compiler Crate
//!
//! This module interfaces with the Haskell implementation of the
//! Floorplan compiler, providing official support for compiling
//! Floorplan specifications into Rust code directly from the
//! cargo toolchain.
//!
//! In order to use this crate, you must have the `flp` executable
//! on your `$PATH` when developing a Floorplan-based memory manager.
//!
//! In order to compile directly from the cargo toolchain, include a
//! `build.rs` in your project root similar to the following:
//! 
//! ```rust,ignore
//! extern crate flp_compiler as flpc;
//!
//! fn main() {
//!    flpc::Build::new()
//!        .src("src/heap/layout.flp")
//!        .dest("src/heap/layout.rs")
//!        .compile();
//! }
//! ```
//!
//! For information on acquiring the Floorplan compiler itself,
//! go see the [GitHub project here][github-project].
//!
//! [github-project]: https://github.com/RedlineResearch/floorplan
use std::process::*;
use std::io::Write;

/// A build configuration.
#[derive(Clone, Debug)]
pub struct Build {
    src: Option<String>,
    dest: Option<String>,
}

/// A very basic implementation of a binding to an external compiler, supporting
/// specification of input and output files.
impl Build {

    /// Construct a new build configuration with default values, which will fail to compile
    /// by default.
    pub fn new() -> Build {
        Build {
            src: None,
            dest: None,
        }
    }

    /// Set the source `.flp` to be compiled.
    pub fn src(&mut self, s: &str) -> &mut Build {
        self.src = Some(s.to_string());
        self
    }

    /// Set the destination `.rs` file to generate the output library into.
    pub fn dest(&mut self, d: &str) -> &mut Build {
        self.dest = Some(d.to_string());
        self
    }

    /// Run the compiler on the current build configuration, failing miserably on error.
    pub fn compile(&mut self) {
        if let Err(e) = self.try_compile() {
            fail(&e);
        }
    }

    /// Attempt to run the compiler on the current build configuration, politely
    /// returning an error message if the compiler fails.
    pub fn try_compile(&mut self) -> Result<(), String> {
        let src = self.src.clone().unwrap();
        let dest = self.dest.clone().unwrap();
        Command::new("flp")
                .args(&[src, dest])
                .output()
                .expect("Floorplan failed to run.");
        Ok(())
    }

}

fn fail(s: &str) -> ! {
    let _ = writeln!(std::io::stderr(), "\nError: {}\n\n", s);
    std::process::exit(1);
}

