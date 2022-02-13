use num::Integer;
use std::{
    fmt::Debug,
};
/**
  An n-dimensional array of type `T`, with a shape as a vector of natural numbers,
  indexable by a
*/
use tinyvec::{tiny_vec, TinyVec};

use super::ArrayError;

#[derive(Clone, Debug)]
pub struct Tensor<T: Clone + Debug> {
    shape: TinyVec<[usize; 4]>,
    shape_indices: TinyVec<[usize; 4]>,
    backing: TinyVec<[Vec<T>; 4]>,
}

impl<T: Clone + Debug> Tensor<T> {
    /// Creates an empty tensor with shape `[0]`.
    pub fn new() -> Self {
        Self {
            shape: tiny_vec!([usize; 4]),
            shape_indices: tiny_vec!([usize; 4]),
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
                    shape_indices: TinyVec::from(calculate_indices(shape).as_slice()),
                }
                .normalise_atinit()
            }
        } else {
            // shape was empty or contained a zero
            Self::new()
        }
    }

    pub fn shape_indices(&self) -> &[usize] {
        &self.shape_indices
    }

    pub fn try_get_at(&self, idx: &[usize]) -> Result<&T, ArrayError> {
        if idx.len() != self.shape.len() {
            return Err(ArrayError::WrongShape);
        } // cant index 3x3x3 tensor by a 2x2 matrix
        let has_range_error = idx
            .iter() // if one of the indices is t
            .zip(self.shape.iter())
            .map(|(&l, &r)| l > r - 1)
            .reduce(|l, r| l || r)
            .unwrap_or(false);
        if has_range_error {
            return Err(ArrayError::OutOfBounds);
        }
        let real_index = idx
            .iter()
            .zip(self.shape_indices.iter())
            .map(|(&x, &y)| x * y)
            .reduce(|x, y| x + y)
            .ok_or(ArrayError::Empty)?;
        let (which_vec, qual_index) = real_index.div_mod_floor(&(u32::MAX as usize));
        let result = &self.backing[which_vec][qual_index];
        Ok(result)
    }

    fn normalise_atinit(self) -> Self {
        let mut carry = Vec::new();
        for array in self.backing.iter_mut() {
            if array.len() >= u32::MAX as usize {
                carry.extend_from_slice(&array);
                let mut with_carry = carry;
                let (allowed, overflow) = with_carry.split_at_mut((u32::MAX - 1) as usize);
                carry = Vec::from(overflow);
                array = &mut Vec::from(allowed)
            }
        }
        self
    }

    /// Iterates over every item in a tensor
    pub fn iter(&self) -> Iter<T> {
        Iter {
            location: 0,
            position: 0,
            vecs: &self.backing
        }
    }
}

fn calculate_indices(shape: &[usize]) -> Vec<usize> {
    let mut result: Vec<usize> = shape
        .iter()
        .scan(1, |st, y| {
            *st = *st * *y;
            Some(*st)
        })
        .collect();
    result[0] = 0;
    result
}

pub fn repeat_to_len<T: Clone>(length: usize, content: &[T]) -> Vec<T> {
    content.iter().cycle().take(length).cloned().collect()
}

#[derive(Debug, Clone)]
pub struct Iter<'a, T> {
    /// location inside the list of backing vectors
    location: usize,
    /// position within the backing vectors
    position: usize,
    /// slice of the backing vectors
    vecs: &'a [Vec<T>],
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    #[inline]
    fn next(&mut self) -> Option<&'a T> {
       let result = self.vecs.get(self.location).and_then(|v| v.get(self.position));
        self.inc();
        result
    }
}

impl<'a, T> Iter<'a, T> {
    #[inline]
    fn inc(&mut self) {
        assert!(self.position <= u32::MAX as usize);
        if u32::MAX as usize - self.position <= 1 {
           self.location += 1;
            self.position = 0;
        }
        else {
            self.position += 1
        }
    }
}

impl<T: Debug + Clone + PartialEq> PartialEq for Tensor<T> {
    fn eq(&self, other: &Self) -> bool {
        self.shape == other.shape // if the shapes aren't equal they can't be equal
        && self.backing == other.backing

    }
}


