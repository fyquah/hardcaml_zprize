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

use serial_test::serial;
use std::ops::Neg;

#[allow(unused_imports)]
use blst_msm::*;

#[test]
#[serial]
fn msm_with_interemediate_infinity_when_adding_non_representable_points1() {
    // println!(" *** RAHUL DEBUG *** ");
    let batches = 1;
    let num_points = 4;
    let mut points = Vec::new();
    let mut scalars = Vec::new();
    let mut arkworks_results = Vec::new();

    // All these points cannot be handled by the FPGA. All of them will be computed on the host.
    points.push(
        util::create_g1_affine_from_string(
            b"32D756062D349E59416ECE15CCBF8E86EF0D33183465A42FE2CB65FC1664272E6BB28F0E1C7A7C9C05824AD09ADC00",
            b"6E4B66BB23EF4BEF715F597162D6662D8161CD062D6212D39392E17232444A0760B5DC479DB98123AB3887AA3CB34E"
        )
    );
    points.push(
        util::create_g1_affine_from_string(
            b"32d756062d349e59416ece15ccbf8e86ef0d33183465a42fe2cb65fc1664272e6bb28f0e1c7a7c9c05824ad09adc00",
            b"13feedf5ca1219ed6c9a666fb3e72d4eca17825fac7b17c4b5fcf4e47d703b60faaa767e862467f615d877855c34cb3"
        )
    );
    points.push(
        util::create_g1_affine_from_string(
            b"1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000",
            b"0"
        )
    );
    points.push(points[2]);
    assert_eq!(points[0].neg(), points[1]);

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
        assert_eq!(
            our_result,
            arkworks_result,
            "msm_with_interemediate_infinity_when_adding_non_representable_points1 failed!");
    }
}

// This test validates that the affine-point booth-algorith multiplication can
// tolerate results with intermediate infinities.
#[test]
#[serial]
fn msm_with_interemediate_infinity_when_adding_non_representable_points2() {
    let batches = 1;
    let num_points = 4;
    let mut points = Vec::new();
    let mut scalars = Vec::new();
    let mut arkworks_results = Vec::new();

    // These three points can't be represented in twisted edwards form
    points.push(
        util::create_g1_affine_from_string(
            b"32D756062D349E59416ECE15CCBF8E86EF0D33183465A42FE2CB65FC1664272E6BB28F0E1C7A7C9C05824AD09ADC00",
            b"6E4B66BB23EF4BEF715F597162D6662D8161CD062D6212D39392E17232444A0760B5DC479DB98123AB3887AA3CB34E"
        )
    );
    points.push(
        util::create_g1_affine_from_string(
            b"1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000",
            b"0"
        )
    );
    points.push((points[0] + points[1]).neg());
    points.push(points[1]);
    // This one point will be computed on the fpga, since it's representable in twisted edwards

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
        assert_eq!(
            our_result,
            arkworks_result,
            "msm_with_interemediate_infinity_when_adding_non_representable_points2 failed!");
    }
}

// MSM where the result is equal to infinity, without requiring to do special handling for non
// representable points.
#[test]
#[serial]
fn msm_with_infinity_result() {
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
        assert_eq!(
            our_result,
            arkworks_result,
            "msm_with_infinity_result test failed!");
    }
}


// MSM where the result is equal to infinity, without requiring to do special handling for non
// representable points.
#[test]
#[serial]
fn msm_with_unrepresentable_points_at_chunk_boundaries() {
    let batches = 4;
    let num_points = 1 << 12;
    let (mut points, scalars) = util::generate_points_scalars::<G1Affine>(num_points, batches);
    let mut arkworks_results = Vec::new();

    // Sprinkle the unrepresentable points at the boundary of all but the third chunk.
    points[1024] = 
        util::create_g1_affine_from_string(
            b"32D756062D349E59416ECE15CCBF8E86EF0D33183465A42FE2CB65FC1664272E6BB28F0E1C7A7C9C05824AD09ADC00",
            b"6E4B66BB23EF4BEF715F597162D6662D8161CD062D6212D39392E17232444A0760B5DC479DB98123AB3887AA3CB34E"
        );

    points[2048-1] =
        util::create_g1_affine_from_string(
            b"1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000",
            b"0"
        );
    points[3*1024-1] =
        util::create_g1_affine_from_string(
            b"32d756062d349e59416ece15ccbf8e86ef0d33183465a42fe2cb65fc1664272e6bb28f0e1c7a7c9c05824ad09adc00",
            b"13feedf5ca1219ed6c9a666fb3e72d4eca17825fac7b17c4b5fcf4e47d703b60faaa767e862467f615d877855c34cb3"
        );

    points[4096 - 1] =
        util::create_g1_affine_from_string(
            b"1ae3a4617c510eac63b05c06ca1493b1a22d9f300f5138f1ef3622fba094800170b5d44300000008508c00000000000",
            b"0"
        );

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
        assert_eq!(
            our_result,
            arkworks_result,
            "msm_with_unrepresentable_points_at_chunk_boundaries test failed when checking batch {}!",
            b);
    }
}

// MSM where the result is equal to infinity, without requiring to do special handling for non
// representable points.
#[test]
#[serial]
fn msm_one_unrepresentable_one_regular() {
    let batches = 1;
    let mut scalars = Vec::new();
    let mut points = Vec::new();
    let mut arkworks_results = Vec::new();

    for _i in 0..batches {
        scalars.push(BigInteger256::from(1));
        scalars.push(BigInteger256::from(2));
    }

    // One point that can be represented
    points.push(G1Affine::prime_subgroup_generator());

    // One point that can't be represented
    points.push( 
        util::create_g1_affine_from_string(
            b"32D756062D349E59416ECE15CCBF8E86EF0D33183465A42FE2CB65FC1664272E6BB28F0E1C7A7C9C05824AD09ADC00",
            b"6E4B66BB23EF4BEF715F597162D6662D8161CD062D6212D39392E17232444A0760B5DC479DB98123AB3887AA3CB34E"
        ));

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
        assert_eq!(
            our_result,
            arkworks_result,
            "msm_one_unrepresentable_one_regular test failed when checking batch {}!",
            b);
    }
}
