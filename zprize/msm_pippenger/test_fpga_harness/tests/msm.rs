// Copyright Supranational LLC
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

use ark_ec::ProjectiveCurve;
use ark_ff::BigInteger256;

use serial_test::serial;
use std::str::FromStr;
use std::time::SystemTime;

#[allow(unused_imports)]
use blst_msm::*;

#[test]
#[serial]
fn msm_correctness() {
    let (batches, points, scalars, arkworks_results) = util::generate_or_load_test_data();
    let mut context = multi_scalar_mult_init(points.as_slice());

    let num_rounds =
        match std::env::var("TEST_NUM_ROUNDS") {
            Err(_) => 1,
            Ok(x) => u64::from_str(&x).unwrap()
        };
    println!("Running msm test for {} rounds", num_rounds);

    for r in 0..num_rounds {
        let t_begin = SystemTime::now();
        let msm_results = multi_scalar_mult(&mut context, points.as_slice(), unsafe {
            std::mem::transmute::<&[_], &[BigInteger256]>(scalars.as_slice())
        });
        println!("Running multi_scalar_mult took {:?} (round = {})", t_begin.elapsed(), r);

        for b in 0..batches {
            assert_eq!(
                msm_results[b].into_affine(),
                arkworks_results[b],
                "assertion failed in round = {} batch = {}", r, b);
        }
    }
}
