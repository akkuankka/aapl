use std::fmt::Debug;
use num::Integer;
/**
  An n-dimensional array of type `T`, with a shape as a vector of natural numbers,
  indexable by a
*/
use tinyvec::{tiny_vec, TinyVec};

use super::ArrayError;

#[derive(Clone, Debug)]
pub struct Tensor<T: Clone + Debug> {
    shape: TinyVec<[usize; 4]>,
    shape_indexes: TinyVec<[usize; 4]>,
    backing: TinyVec<[Vec<T>; 4]>,
}

impl<T: Clone + Debug> Tensor<T> {
    /// Creates an empty tensor with shape `[0]`.
    pub fn new() -> Self {
        Self {
            shape: tiny_vec!([usize; 4]),
            shape_indexes: tiny_vec!([usize; 4]),
            backing: tiny_vec!([Vec<T>; 4]),
        }
    }

    /// Creates a tensor by reshaping and repeating an array to the shape provided.
    pub fn from_array(shape: &[usize], array: &[T]) -> Self {
        let length = shape.iter().reduce(|&x, &y| &(x * y));
        if let Some(&length_needed) = length {
            if length_needed == 0 {
                Self::new()
            } else {
                Self {
                    shape: TinyVec::from(shape),
                    backing: tiny_vec!([Vec<T>; 4] => repeat_to_len(length_needed, array)),
                    shape_indexes: TinyVec::from(calculate_indexes(shape).as_slice()),
                }
            }
        } else {
            // shape was empty or contained a zero
            Self::new()
        }
    }

    pub fn shape_indexes(&self) -> &[usize] {
        &self.shape_indexes
    }

    pub fn try_get_at(&self, idx: &[usize]) -> Result<&T, ArrayError>{
        if idx.len() != self.shape.len() {return Err(ArrayError::WrongShape)}
        let has_range_error = idx.iter() // if one of the indexes is t
            .zip(self.shape.iter())
            .map(|(&l, &r)| l > r - 1 )
            .reduce(|l, r| l || r)
            .unwrap_or(false)
            ;
        if has_range_error {return Err(ArrayError::OutOfBounds)}
        let real_index = idx.iter()
            .zip(self.shape_indexes.iter())
            .map(|(&x, &y)| x * y)
            .reduce(|x, y| x + y)
            .ok_or(ArrayError::Empty)?
            ;
        let (which_vec, qual_index) = real_index.div_mod_floor(&(u32::MAX as usize));
         
    }
}

fn calculate_indexes(shape: &[usize]) -> Vec<usize> {
    let mut result: Vec<usize> = shape.iter().scan(1, |st, y| {
        *st = *st * *y;
        Some(*st)
    }).collect();
    result[0] = 0;
    result
}

pub fn repeat_to_len<T: Clone>(length: usize, content: &[T]) -> Vec<T> {
    content.iter().cycle().take(length).cloned().collect()
}
