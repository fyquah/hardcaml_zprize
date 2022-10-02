// Copyright Supranational LLC
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

use ark_bls12_377::G1Affine;
use ark_bls12_377::FrParameters;
use ark_ec::msm::VariableBaseMSM;
use ark_ff::BigInteger256;
use ark_ff::Fp256;
use ark_ec::ProjectiveCurve;
use ark_serialize::CanonicalSerialize;
use ark_serialize::CanonicalDeserialize;

use std::fs::File;
use std::path::Path;
use std::str::FromStr;

use blst_msm::*;

fn generate_or_load_test_data() -> (usize, Vec<G1Affine>, Vec<Fp256<FrParameters>>, Vec<G1Affine>) {
    match std::env::var("TEST_LOAD_DATA_FROM") {
        Err(_) =>  {
            let test_npow = std::env::var("TEST_NPOW").unwrap_or("15".to_string());
            let npoints_npow = i32::from_str(&test_npow).unwrap();
            let batches = 4;
            let (points, scalars) =
                util::generate_points_scalars::<G1Affine>(1usize << npoints_npow, batches);
            let mut arkworks_results = Vec::new();

            for b in 0..batches {
                let start = b * points.len();
                let end = (b + 1) * points.len();
                let arkworks_result =
                    VariableBaseMSM::multi_scalar_mul(points.as_slice(), unsafe {
                        std::mem::transmute::<&[_], &[BigInteger256]>(&scalars[start..end])
                    }).into_affine();
                arkworks_results.push(arkworks_result);
            }

            match std::env::var("TEST_WRITE_DATA_TO") {
                Err(_) => (),
                Ok(dirname) => {
                    println!("Saving testdata to {}", dirname);
                    // it would be real sad if we run all the data generation above, then it chokes
                    // because dir doesn't exist .... run mkdir -p here

                    let dirname = Path::new(&dirname);
                    std::fs::create_dir_all(&dirname).unwrap();
                    points.serialize(
                        File::create(dirname.join("points.bin")).unwrap()
                    ).unwrap();
                    scalars.serialize(
                        File::create(dirname.join("scalars.bin")).unwrap()
                    ).unwrap();
                    arkworks_results.serialize(
                        File::create(dirname.join("arkworks_results.bin")).unwrap()
                    ).unwrap();
                }
            }

            (batches, points, scalars, arkworks_results)
        },
        Ok(test_data_dir) => {
            println!("Loading testdata from {}", test_data_dir);
            let points =
                Vec::<G1Affine>::deserialize(
                    File::open(Path::new(&test_data_dir).join("points.bin")).unwrap()
                ).unwrap();
            let scalars =
                Vec::<Fp256<FrParameters>>::deserialize(
                    File::open(Path::new(&test_data_dir).join("scalars.bin")).unwrap()
                ).unwrap();
            let arkworks_results =
                Vec::<G1Affine>::deserialize(
                    File::open(Path::new(&test_data_dir).join("arkworks_results.bin")).unwrap()
                ).unwrap();
            assert!(scalars.len() % points.len() == 0);
            assert!(scalars.len() >= points.len());

            let batches = scalars.len() / points.len();
            (batches, points, scalars, arkworks_results)
        }
    }
}

#[test]
fn msm_correctness() {
    let (batches, points, scalars, arkworks_results) = generate_or_load_test_data();

    let mut context = multi_scalar_mult_init(points.as_slice());
    let msm_results = multi_scalar_mult(&mut context, points.as_slice(), unsafe {
        std::mem::transmute::<&[_], &[BigInteger256]>(scalars.as_slice())
    });
                                                 
    for b in 0..batches {
        assert_eq!(msm_results[b].into_affine(), arkworks_results[b]);
    }
}
