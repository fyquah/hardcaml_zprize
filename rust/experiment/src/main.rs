use ark_ec::AffineCurve;
use ark_ec::models::{ModelParameters, SWModelParameters};
use ark_ec::models::short_weierstrass_jacobian::GroupAffine;
use ark_ff::PrimeField;
use ark_ff::biginteger::BigInteger384;
use ark_bls12_377::g1::Parameters;

type G1AffinePoint = GroupAffine<Parameters>;
type BaseField = <Parameters as ModelParameters>::BaseField;

pub fn main() {
    let generator = G1AffinePoint::prime_subgroup_generator();
    println!("generator = {}\n\n", generator);
    println!("a = {}\n\n", Parameters::COEFF_A);
    println!("b = {}\n\n", Parameters::COEFF_B);

    let x = 2;
    let y = 3;

    let point = G1AffinePoint::new(
        BaseField::from_repr(BigInteger384::from(x)).unwrap(),
        BaseField::from_repr(BigInteger384::from(y)).unwrap(),
        false
        );
    
    println!("point = {} {}\n\n", point, point.is_on_curve());

    println!("cofactor = {:?}\n\n", Parameters::COFACTOR);
}
