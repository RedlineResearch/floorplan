extern crate flp_compiler as flpc;

fn main() {

    flpc::Build::new()
        .src("src/layout.flp")
        .dest("src/layout.rs")
        .compile();

}

