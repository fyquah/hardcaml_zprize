# Compiling the Reference

Run `cargo build` in `rust/ark_bls12_377_g1` to compile the dynamic library
exposing a reference implementation of the bls12-377 g1 curve. This is
necessary for the expect tests to work expectedly.

z3 should be installed to run tests.

Installing ocaml deps: `opam install . --deps-only`
