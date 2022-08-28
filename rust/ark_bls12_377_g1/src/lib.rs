use ark_ec::AffineCurve;
use ark_ec::models::{ModelParameters, SWModelParameters};
use ark_ec::models::short_weierstrass_jacobian::GroupAffine;
use ark_ff::PrimeField;
use ark_ff::FpParameters;
use ark_ff::biginteger::BigInteger384;
use std::ops::Neg;

type G1AffinePoint = GroupAffine<ark_bls12_377::g1::Parameters>;
type BaseField = <ark_bls12_377::g1::Parameters as ModelParameters>::BaseField;

fn index_pointer(px: *const u64, i: u64) -> u64 {
    unsafe { *px.add(i as usize) }
}

fn ptr_c_uchar_to_array(px: *const u64) -> [u64; 6] {
    [
        index_pointer(px, 0),
        index_pointer(px, 1),
        index_pointer(px, 2),
        index_pointer(px, 3),
        index_pointer(px, 4),
        index_pointer(px, 5),
    ]
}

/* TODO(fyquah): Return null and propogate it to ocaml. */
fn copy_pointer_to_fp384(px: *const u64) -> BaseField {
    BaseField::from_repr(BigInteger384::new(ptr_c_uchar_to_array(px))).unwrap()
}

fn copy_biginteger_to_pointer(x: &BigInteger384, dst: *mut u64) {
    let x = match x { BigInteger384(x) => x };
    unsafe { 
        for i in 0..6 {
            let p = dst.add(i as usize);
            *p = x[i];
        }
    }
}

fn copy_fp384_to_pointer(x: &BaseField, dst: *mut u64) {
    copy_biginteger_to_pointer(&x.into_repr(), dst)
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_new(px: *const u64, py: *const u64, infinity: bool) -> *mut G1AffinePoint {
    let point = G1AffinePoint::new(
        copy_pointer_to_fp384(px),
        copy_pointer_to_fp384(py),
        infinity);
    Box::into_raw(Box::new(point))
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_get_x(ptr_point: *mut G1AffinePoint, dst: *mut u64) -> () {
    let point : &G1AffinePoint = unsafe { &*ptr_point };
    copy_fp384_to_pointer(&point.x, dst);
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_get_y(ptr_point: *mut G1AffinePoint, dst: *mut u64) -> () {
    let point : &G1AffinePoint = unsafe { &*ptr_point };
    copy_fp384_to_pointer(&point.y, dst);
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_subgroup_generator() -> *mut G1AffinePoint {
    let point = G1AffinePoint::prime_subgroup_generator();
    Box::into_raw(Box::new(point))
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_mul(ptr_point: *mut G1AffinePoint, by: u64) -> *mut G1AffinePoint {
    let point = unsafe { &*ptr_point };
    Box::into_raw(Box::new(G1AffinePoint::from(point.mul(by))))
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_add(ptr_a: *mut G1AffinePoint, ptr_b: *mut G1AffinePoint) -> *mut G1AffinePoint {
    let a = unsafe { &*ptr_a };
    let b = unsafe { &*ptr_b };
    Box::into_raw(Box::new(*a + *b))
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_neg(ptr: *mut G1AffinePoint) -> *mut G1AffinePoint {
    let a = unsafe { &*ptr };
    Box::into_raw(Box::new((*a).neg()))
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_free(ptr: *mut G1AffinePoint) -> () {
    /* Box::from_raw will result in the rust free-ing memory used by [ptr]. */
    unsafe {
        let _unused = Box::from_raw(ptr);
        ()
    }
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_get_infinity(ptr_a: *mut G1AffinePoint) -> bool {
    let a = unsafe { &*ptr_a };
    a.infinity
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_is_on_curve(ptr_a: *mut G1AffinePoint) -> bool {
    let a = unsafe { &*ptr_a };
    a.is_on_curve()
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_equal(ptr_a: *mut G1AffinePoint, ptr_b: *mut G1AffinePoint) -> bool {
    let a = unsafe { &*ptr_a };
    let b = unsafe { &*ptr_b };
    a == b
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_coeff_a(dst: *mut u64) {
    copy_fp384_to_pointer(&ark_bls12_377::g1::Parameters::COEFF_A, dst);
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_coeff_b(dst: *mut u64) {
    copy_fp384_to_pointer(&ark_bls12_377::g1::Parameters::COEFF_B, dst);
}

#[no_mangle]
pub extern "C" fn ark_bls12_377_g1_modulus(dst: *mut u64) {
    copy_biginteger_to_pointer(&ark_bls12_377::fq::FqParameters::MODULUS, dst);
}
