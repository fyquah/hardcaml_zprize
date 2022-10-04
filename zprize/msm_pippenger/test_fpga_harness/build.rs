extern crate cmake;

use std::env;

use cmake::Config;

// This build script is based on this guide to call C++ from rust
// https://flames-of-code.netlify.app/blog/rust-and-cmake-cplusplus/

fn main() {
    let dst = Config::new("cxx").define("CMAKE_BUILD_TYPE", "Release").build();

    // Now - emitting some cargo commands to build and link the lib.
    // This turns to be common to both our libs, so we do it once.
    println!("cargo:rustc-link-search=native={}", dst.display());
    // Phase `foo` here stands for the library name (without lib prefix and without .a suffix)
    //
    println!("cargo:rustc-link-lib=dylib=driver");
    println!("cargo:rustc-link-lib=dylib=gmp");

    // C++ is bit more complicated, since platform specifics come to play
    let target = env::var("TARGET").unwrap();
    if target.contains("apple") {
        println!("cargo:rustc-link-lib=dylib=c++");
    } else if target.contains("linux") {
        println!("cargo:rustc-link-lib=dylib=stdc++");
    } else {
        unimplemented!();
    }
}
