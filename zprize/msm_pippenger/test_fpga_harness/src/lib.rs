// Copyright Supranational LLC
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

use ark_bls12_377::{Fr, G1Affine};
use ark_ec::AffineCurve;
use ark_ff::PrimeField;
use ark_std::Zero;

use std::os::raw::c_void;
use std::time::SystemTime;

#[allow(unused_imports)]
use blst::*;

pub mod util;

#[cfg_attr(feature = "quiet", allow(improper_ctypes))]
extern "C" {
    fn msm_init(
        xclbin: *const u8,
        xclbin_len: usize,
        points: *const G1Affine,
        npoints: usize,
    ) -> *mut c_void;

    fn msm_mult(
        context: *mut c_void,
        out: *mut u64,
        batch_size: usize,
        scalars: *const Fr,
    ) -> ();
}

#[repr(C)]
pub struct MultiScalarMultContext {
    context: *mut c_void,
}

pub fn multi_scalar_mult_init<G: AffineCurve>(
    points: &[G]
) -> MultiScalarMultContext {
    let now = SystemTime::now();
    let xclbin = std::env::var("XCLBIN").expect("Need an XCLBIN file");
    let ret = unsafe {
        let context = msm_init(
            xclbin.as_ptr() as *const u8,
            xclbin.len(),
            points as *const _ as *const G1Affine,
            points.len(),
        );
        MultiScalarMultContext { context }
    };
    println!("multi_scalar_mult_init took {:?}", now.elapsed());

    ret
}

pub fn multi_scalar_mult<G: AffineCurve>(
    context: &mut MultiScalarMultContext,
    points: &[G],
    scalars: &[<G::ScalarField as PrimeField>::BigInt],
) -> Vec<G::Projective> {
    let npoints = points.len();
    if scalars.len() % npoints != 0 {
        panic!("length mismatch")
    }

    let batch_size = scalars.len() / npoints;
    let mut ret = vec![G::Projective::zero(); batch_size];

    unsafe {
        let result_ptr =
            &mut *(&mut ret as *mut Vec<G::Projective> as *mut Vec<u64>);

        msm_mult(
            context.context,
            result_ptr.as_mut_ptr(),
            batch_size,
            scalars as *const _ as *const Fr,
        );
    };

    ret
}
