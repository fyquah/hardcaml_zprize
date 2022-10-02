// Copyright Supranational LLC
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

use criterion::{criterion_group, criterion_main, Criterion};

use ark_ff::BigInteger256;

use blst_msm::*;

fn criterion_benchmark(c: &mut Criterion) {
    let (batches, points, scalars, _arkworks_results) =
        util::generate_or_load_test_data();
    let mut context = multi_scalar_mult_init(points.as_slice());
    let npoints = points.len();
    let mut group = c.benchmark_group("FPGA-MSM");
    group.sample_size(10);

    let name =
        if npoints.is_power_of_two() {
            format!("2**{}x{}", npoints.trailing_zeros(), batches)
        } else {
            format!("{}x{}", npoints, batches)
        };

    group.bench_function(name, |b| {
        b.iter(|| {
            let _msm_results = multi_scalar_mult(&mut context, points.as_slice(), unsafe {
                std::mem::transmute::<&[_], &[BigInteger256]>(scalars.as_slice())
            });
        })
    });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
