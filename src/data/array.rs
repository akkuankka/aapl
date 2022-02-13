use tinyvec::TinyVec;
use std::rc::Rc;

pub mod tensor;
use tensor::Tensor;

use super::Number;
#[derive(Clone, PartialEq, Debug)]
pub struct Array {
    shape: TinyVec<[usize; 4]>,
    arr: ArrayInnards
}

#[derive(Clone, PartialEq, Debug)]
pub enum ArrayInnards {
    Vector(TinyVec<[Scalar; 8]>),
    Matrix(TinyVec<[TinyVec<[Scalar; 8]>; 8]>),
    Tensor(Tensor<Scalar>)
}

#[derive(Clone, PartialEq, Debug)]
pub enum Scalar {
    Char(char),
    Number(Number),
    Nested(Rc<Array>),
}

impl Default for Scalar {
    #[inline]
    fn default() -> Scalar {
        Scalar::Number(Number::from(0))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ArrayError {
    WrongShape,
    Domain,
    OutOfBounds,
    Empty,
}

