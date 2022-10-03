// Copyright Supranational LLC
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

use ark_bls12_377::FrParameters;
use ark_bls12_377::G1Affine;
use ark_ec::AffineCurve;
use ark_ec::msm::VariableBaseMSM;
use ark_ec::ProjectiveCurve;
use ark_ff::BigInteger256;
use ark_ff::Fp256;

use std::ops::Neg;
use std::str::FromStr;
use std::time::SystemTime;

#[allow(unused_imports)]
use blst_msm::*;

#[test]
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

#[test]
fn msm_with_identity_result() {
    let batches = 4;
    let num_points = 1 << 8;
    let g1_generator = G1Affine::prime_subgroup_generator();
    let mut points = Vec::new();
    let mut scalars = Vec::new();
    let mut arkworks_results = Vec::new();
    for i in 0..(num_points) {
        if i % 2 == 0 {
            points.push(g1_generator);
        } else {
            points.push(g1_generator.neg());
        }
    }

    for _i in 0..(batches * num_points) {
        scalars.push(Fp256::<FrParameters>::new(BigInteger256::from(1)));
    }

    for b in 0..batches {
        let start = b * points.len();
        let end = (b + 1) * points.len();
        arkworks_results.push(
                VariableBaseMSM::multi_scalar_mul(points.as_slice(), unsafe {
                    std::mem::transmute::<&[_], &[BigInteger256]>(&scalars[start..end])
                    }).into_affine());
    }

    let mut context = multi_scalar_mult_init(points.as_slice());
    let msm_results = multi_scalar_mult(&mut context, points.as_slice(), unsafe {
        std::mem::transmute::<&[_], &[BigInteger256]>(scalars.as_slice())
    });

    for b in 0..batches {
        let arkworks_result = arkworks_results[b];
        let our_result = msm_results[b].into_affine();
        println!("Our result = {:?}", our_result);
        println!("Arkworks result = {:?}", arkworks_result);

        assert_eq!(
            our_result,
            arkworks_result,
            "msm_with_identity_result test failed!");
    }
}
