use ark_ec::AffineCurve;
use ark_ec::models::ModelParameters;
use ark_ec::models::short_weierstrass_jacobian::GroupAffine;

type G1Group = GroupAffine<ark_bls12_377::g1::Parameters>;
type BaseField = <ark_bls12_377::g1::Parameters as ModelParameters>::BaseField;

pub fn main() {
    let a = G1Group::prime_subgroup_generator();
    println!("  a.mul(2) = {}", a + a);

    let b = G1Group::new(BaseField::from(2), BaseField::from(3), false);
    println!("  b = {}", b);
    println!("  b = {}", b + b);
}
