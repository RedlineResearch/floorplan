extern crate cc;
extern crate flp_compiler as flpc;

fn main() {
    //cc::compile_library("libgc_clib_x64.a", &["src/heap/gc/clib_x64.c"]);
    cc::Build::new()
        .file("src/heap/gc/clib_x64.c")
        .compile("libgc_clib_x64.a");

    flpc::Build::new()
        .src("src/heap/layout.flp")
        .dest("src/heap/layout.rs")
        .compile();
//
//    if cfg!(target_os = "linux") {
//        cc::Config::new()
//                     .flag("-lpfm")
//                     .flag("-O3")
//                     .file("src/common/perf.c")
//                     .compile("libgc_perf.a");
//    }
}
