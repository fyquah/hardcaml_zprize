// Copyright Supranational LLC
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

use std::os::raw::c_void;
// use ark_bls12_377::{Fr, G1Affine};
use ark_ec::AffineCurve;
use ark_ff::PrimeField;
use ark_std::Zero;

#[allow(unused_imports)]
use blst::*;

pub mod util;

#[repr(C)]
pub struct MultiScalarMultContext {
    context: *mut c_void,
}

pub fn multi_scalar_mult_init<G: AffineCurve>(
    _points: &[G],
) -> MultiScalarMultContext {
    let mut ret = MultiScalarMultContext {
        context: std::ptr::null_mut(),
    };
    ret = ret;

    // TODO: Complete me. Use the below as reference!
    //
    // let err = unsafe {
    //     mult_pippenger_init(
    //         &mut ret,
    //         points as *const _ as *const G1Affine,
    //         points.len(),
    //         std::mem::size_of::<G1Affine>(),
    //     )
    // };
    // if err.code != 0 {
    //     panic!("{}", String::from(err));
    // }

    ret
}
    
pub fn multi_scalar_mult<G: AffineCurve>(
    _context: &mut MultiScalarMultContext,
    points: &[G],
    scalars: &[<G::ScalarField as PrimeField>::BigInt],
) -> Vec<G::Projective> {
    let npoints = points.len();
    if scalars.len() % npoints != 0 {
        panic!("length mismatch")
    }

    let batch_size = scalars.len() / npoints;
    let mut ret = vec![G::Projective::zero(); batch_size];
    ret = ret;

    // TODO: Complete me. Use the below as reference!
    // let err = unsafe {
    //     let result_ptr = 
    //         &mut *(&mut ret as *mut Vec<G::Projective>
    //                as *mut Vec<u64>);

    //     mult_pippenger_inf(
    //         context,
    //         result_ptr.as_mut_ptr(),
    //         points as *const _ as *const G1Affine,
    //         npoints, batch_size,
    //         scalars as *const _ as *const Fr,
    //         std::mem::size_of::<G1Affine>(),
    //     )
    // };
    // if err.code != 0 {
    //     panic!("{}", String::from(err));
    // }

    ret
}
