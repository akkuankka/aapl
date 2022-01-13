use derive_more::From;
use num::complex::Complex;
use num::traits::Pow;
use rust_decimal::{Decimal, prelude::ToPrimitive};
use rust_decimal_macros::dec;
use rust_decimal::prelude::*;

/// ATTN: Call `normalise` any time you manipulate me!
#[derive(Clone, Copy, From, Debug)]
pub enum Number {
    Int(i64),
    Decf(Decimal),
    Compl(Complex<f64>),
}

use Number::*;

impl Number {
    pub fn normalise(self) -> Self {
        match self {
            // check complexes are actually complex
            Compl(c) => {
                if c.im == 0. {
                    Decf(c.re.try_into().expect("isn't NaN")).normalise()
                } else {
                    Compl(c)
                }
            }
            Decf(d) => {
                if d.fract() == dec!(0) && d < i64::MAX.into() {
                    Int(d.mantissa() as i64)
                } else {
                    Decf(d)
                }
            }
            otherwise => otherwise,
        }
    }

    pub fn from_strs(is_negative: bool, bigpart: &str, smallpart: Option<&str>) -> Number {
        let integral: i64 = bigpart
            .parse()
            .expect("Parser did not recognise invalid integer");

        if smallpart == None || smallpart == Some("0") {
            let sign = if is_negative { -1 } else { 1 };
            Int(integral * sign)
        } else {
            let smallpart = smallpart.unwrap();
            let concatted = format!("{}{}", bigpart, smallpart);
            let concatted: i64 = concatted
                .parse()
                .expect("Parser did not recognise invalid integer or decimal part");
            Decf(Decimal::new(concatted, smallpart.len() as u32))
        }
    }

    pub fn from_str_fractional(is_negative: bool, smallpart: &str) -> Number {
        let i: i64 = smallpart.parse::<i64>()
            .expect("Parser did not recognise invalid integer or decimal part") *
            if is_negative {-1} else {1};
        Decf(Decimal::new(i, smallpart.len() as u32))
    }

    pub fn with_imaginary(self, other: Number) -> Number {
        let curried = |other| match self {
            Int(a) =>  Complex::new(a as f64, other).into(),
            Decf(a) => Complex::new(dec_into_float(&a), other).into(),
            Compl(a) => Complex::new(a.re, other).into()
        };
        curried(match other {
            Int(a) => a as f64,
            Decf(a) => dec_into_float(&a),
            Compl(a) => a.im
        })
    }

    pub fn inverse(self) -> Number {
        match self {
            Int(a) => Decf(dec!(1) / Decimal::from(a)),
            Decf(a) => Decf(dec!(1) / a).normalise(),
            Compl(a) => Compl(a.inv()).normalise()
        }
    }
}


impl std::cmp::PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self.normalise(), other.normalise()) {
            (Int(a), Int(b)) => a == b,
            (Decf(a), Decf(b)) => a == b,
            (Compl(a), Compl(b)) => a == b,
            (_a, _b) => false, // they've been normalised
        }
    }
}
// impl std::cmp::Eq for Number {}
impl std::cmp::PartialEq<i64> for Number {
    fn eq(&self, other: &i64) -> bool {
        match self.normalise() {
            Int(ref a) => a == other,
            _ => false
        }
    }
}


impl std::cmp::PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Int(a), Int(b)) => a.partial_cmp(b),
            (Int(a), Decf(b)) => Decimal::from(*a).partial_cmp(b),
            (Decf(a), Decf(b)) => a.partial_cmp(b),
            _ => None
        }
    }
}
impl std::cmp::PartialOrd<i64> for Number  {
    fn partial_cmp(&self, other: &i64) -> Option<std::cmp::Ordering> {
        match self {
            Int(a) => a.partial_cmp(other),
            Decf(b) => b.partial_cmp(&Decimal::from(*other)),
            _ => None
        }
    }
}

impl std::ops::Add for Number {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Int(a), Int(b)) => Int(a + b),
            (Decf(a), Decf(b)) => Decf(a + b),
            (Compl(a), Compl(b)) => Compl(a + b),
            (Compl(a), Decf(b)) => Compl(a + dec_into_float(&b)),
            (Compl(a), Int(b)) => Compl(a + b as f64),
            (Decf(a), Int(b)) => Decf(a + Decimal::from(b)),
            (b, a) => a + b,
        }
    }
}

impl std::ops::Neg for Number {
    type Output = Self;
    fn neg(self) -> Self {
        match self {
            Int(i) => Int(-i),
            Decf(f) => Decf(-f),
            Compl(c) => Compl(-c)
        }
    }
}

impl std::ops::Sub for Number {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        self + -other
    }
}

impl std::ops::Mul for Number {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Int(a), Int(b)) => Int(a + b), (Decf(a), Decf(b)) => Decf(a * b),
            (Compl(a), Compl(b)) => Compl(a * b),
            (Compl(a), Decf(b)) => Compl(a * dec_into_float(&b)),
            (Compl(a), Int(b)) => Compl(a * b as f64),
            (Decf(a), Int(b)) => Decf(a * Decimal::from(b)),
            (b, a) => a * b,
        }
    }
}

impl std::ops::Div for Number {
    type Output = Self;
    fn div(self, other: Self) -> Self {
        self * other.inverse()
    }
}

pub trait Pown<T> {
    fn pown(self, other: T) -> Self;
}

impl Pown<&Number> for Number {
    fn pown(self, exp: &Number) -> Self {
        match (&self, exp) {
            (Int(0), Int(0)) => panic!("Raised 0 to 0th power!"),
            (Int(base), Int(exp)) if *exp >= 0 => {
                Int(base.pow(*exp as u32))
            }
            (Int(base), Int(exp)) => {
                Int(base.pow(exp.abs() as u32)).inverse()
            }// otherwise
            (Int(base), Decf(exp)) => {
                Compl(Complex::from(*base as f64).powf(dec_into_float(exp))).normalise()
            }
            (Int(base), Compl(exp)) => {
                Compl(exp.expf(*base as f64)).normalise()
            }
            (Decf(base), Int(exp)) => Decf(base.pow(*exp)).normalise(),
            (Decf(base), Decf(exp)) => Decf(base.pow(*exp)).normalise(),

            (Decf(base), Compl(exp)) => {
                Compl(exp.expf(dec_into_float(base))).normalise()
            }
            (Compl(base), Int(exp)) => {
                Compl(base.powi(*exp as i32)).normalise()
            }
            (Compl(base), Decf(exp)) => {
                Compl(base.powf(dec_into_float(exp)))
            }
            (Compl(base), Compl(exp)) => {
                Compl(base.powc(*exp)).normalise()
            }
        }
    }
}

fn dec_into_float(dec: &Decimal) -> f64 {
    dec.to_f64().unwrap_or(0.)
}

#[cfg(test)]
mod test {
    // TODO: More tests!
    use super::*;

    #[test]
    fn plus_works_correctly() {
        assert_eq!(
            Number::from(1) + Number::from(Complex::new(6., 2.)),
            Number::from(Complex::new(1., 0.) + Complex::new(6., 2.))
        )
    }
}
