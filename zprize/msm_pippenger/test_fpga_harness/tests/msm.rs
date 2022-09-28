// Copyright Supranational LLC
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

use ark_ff::BigInteger256;
use ark_ec::ProjectiveCurve;

#[allow(unused_imports)]
use blst_msm::*;

#[test]
fn msm_correctness() {
    let (batches, points, scalars, arkworks_results) = util::generate_or_load_test_data();
    let mut context = multi_scalar_mult_init(points.as_slice());
    let msm_results = multi_scalar_mult(&mut context, points.as_slice(), unsafe {
        std::mem::transmute::<&[_], &[BigInteger256]>(scalars.as_slice())
    });
                                                 
    for b in 0..batches {
        assert_eq!(msm_results[b].into_affine(), arkworks_results[b]);
    }
}
