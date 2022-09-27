// Copyright Supranational LLC
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

use rand::SeedableRng;
use rand_chacha::ChaCha20Rng;

use ark_ec::{AffineCurve, ProjectiveCurve};
use ark_std::UniformRand;

use ark_bls12_377::G1Affine;
use ark_bls12_377::FrParameters;
use ark_ec::msm::VariableBaseMSM;
use ark_ff::BigInteger256;
use ark_ff::Fp256;
use ark_serialize::CanonicalSerialize;
use ark_serialize::CanonicalDeserialize;

use std::fs::File;
use std::path::Path;
use std::str::FromStr;

use std::time::SystemTime;

pub fn generate_points_scalars<G: AffineCurve>(
    len: usize,
    batch_size: usize
) -> (Vec<G>, Vec<G::ScalarField>) {
    let mut rng = ChaCha20Rng::from_entropy();

    let mut points =
        <G::Projective as ProjectiveCurve>::batch_normalization_into_affine(
            &(0..len)
                .map(|_| G::Projective::rand(&mut rng))
                .collect::<Vec<_>>(),
        );
    
// Sprinkle in some infinity points
//    points[3] = G::zero();
    while points.len() < len {
        points.append(&mut points.clone());
    }

    let scalars = (0..len * batch_size)
        .map(|_| G::ScalarField::rand(&mut rng))
        .collect::<Vec<_>>();

    (points, scalars)
}

pub fn generate_or_load_test_data() -> (usize, Vec<G1Affine>, Vec<Fp256<FrParameters>>, Vec<G1Affine>) {
    match std::env::var("TEST_LOAD_DATA_FROM") {
        Err(_) =>  {
            let now = SystemTime::now();
            let test_npow = std::env::var("TEST_NPOW").expect("Must specified either TEST_NPOW or TEST_LOAD_DATA_FROM");
            let npoints_npow = i32::from_str(&test_npow).unwrap();
            let batches = 4;
            let (points, scalars) =
                generate_points_scalars::<G1Affine>(1usize << npoints_npow, batches);
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
            println!("Generating testdata took {:?}", now.elapsed());

            match std::env::var("TEST_WRITE_DATA_TO") {
                Err(_) => (),
                Ok(dirname) => {
                    println!("Saving testdata to {}", dirname);
                    let now = SystemTime::now();
                    // it would be real sad if we run all the data generation above, then it chokes
                    // because dir doesn't exist .... run mkdir -p here

                    let dirname = Path::new(&dirname);
                    std::fs::create_dir_all(&dirname).unwrap();
                    points.serialize_unchecked(
                        File::create(dirname.join("points.bin")).unwrap()
                    ).unwrap();
                    scalars.serialize_unchecked(
                        File::create(dirname.join("scalars.bin")).unwrap()
                    ).unwrap();
                    arkworks_results.serialize_unchecked(
                        File::create(dirname.join("arkworks_results.bin")).unwrap()
                    ).unwrap();
                    println!("Saving testdata took {:?}", now.elapsed());
                }
            }

            (batches, points, scalars, arkworks_results)
        },
        Ok(test_data_dir) => {
            println!("Loading testdata from {}", test_data_dir);
            let now = SystemTime::now();
            let points =
                Vec::<G1Affine>::deserialize_unchecked(
                    File::open(Path::new(&test_data_dir).join("points.bin")).unwrap()
                ).unwrap();
            let scalars =
                Vec::<Fp256<FrParameters>>::deserialize_unchecked(
                    File::open(Path::new(&test_data_dir).join("scalars.bin")).unwrap()
                ).unwrap();
            let arkworks_results =
                Vec::<G1Affine>::deserialize_unchecked(
                    File::open(Path::new(&test_data_dir).join("arkworks_results.bin")).unwrap()
                ).unwrap();
            println!("Loading testdata took {:?}", now.elapsed());
            assert!(scalars.len() % points.len() == 0);
            assert!(scalars.len() >= points.len());

            let batches = scalars.len() / points.len();
            (batches, points, scalars, arkworks_results)
        }
    }
}
