use num::Integer;
use std::{fmt::Debug, iter::once};
/**
  An n-dimensional array of type `T`, with a shape as a vector of natural numbers,
  indexable by a
*/
use tinyvec::{tiny_vec, TinyVec};

use super::ArrayError;

#[derive(Clone, Debug)]
pub struct Tensor<T: Clone + Debug + Default> {
    shape: TinyVec<[usize; 4]>,
    shape_indices: TinyVec<[usize; 4]>,
    backing: TinyVec<[T; 8]>,
}

impl<T: Clone + Debug + Default> Tensor<T> {
    /// Creates an empty tensor with shape `[0]`.
    pub fn new() -> Self {
        Self {
            shape: tiny_vec!([usize; 4]),
            shape_indices: tiny_vec!([usize; 4]),
            backing: tiny_vec!([T; 8]),
        }
    }

    /// Creates a tensor by reshaping and repeating an array to the shape provided.
    pub fn from_array(shape: &[usize], array: &[T]) -> Self {
        let length = shape.iter().copied().reduce(|x, y| x * y);
        if let Some(length_needed) = length {
            if length_needed == 0 {
                Self::new()
            } else {
                let result = Self {
                    shape: TinyVec::from(shape),
                    backing: TinyVec::from(&*repeat_to_len(length_needed, array)),
                    shape_indices: TinyVec::from(calculate_indices(shape).as_slice()),
                };
                println!("created array: {:#?}", result);
                result
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
            .map(|(&i, &s)| i > s + 1)
            .reduce(|l, r| l || r)
            .unwrap_or(false);
        if has_range_error {
            eprintln!("tried to index array of shape [{:?}] by array of shape [{:?}]", &self.shape, &idx);
            return Err(ArrayError::OutOfBounds);
        }
        let real_index = idx // for some index [2;1] into an array of shape [3;3] correct index is 3
            .iter() 
            .rev() // [1;2]
            .map(|x| x - 1) // [0;1]
            .zip(self.shape_indices.iter()) // [(0, 1), (1, 3)]
            .map(|(x, y)| x * y) // [0, 3]
            .reduce(|x, y| x + y) // 3
            .ok_or(ArrayError::Empty)?;
        Ok(&self.backing[real_index])
    }

           // I think this is not required //
    // fn normalise_atinit(self) -> Self {
    //     let mut carry = Vec::new();
    //     for array in self.backing.iter_mut() {
    //         if array.len() >= u32::MAX as usize {
    //             carry.extend_from_slice(&array);
    //             let mut with_carry = carry;
    //             let (allowed, overflow) = with_carry.split_at_mut((u32::MAX - 1) as usize);
    //             carry = Vec::from(overflow);
    //             array = &mut Vec::from(allowed)
    //         }
    //     }
    //     self
    // }

    /// Iterates over every item in a tensor
    pub fn iter(&self) -> Iter<T> {
        Iter {
            position: 0,
            vecs: &self.backing
        }
    }
}

fn calculate_indices(shape: &[usize]) -> Vec<usize> {
    let mut result: Vec<_> = once(&1).chain(shape.iter()).scan(1, |st, y| {
        *st = *st * *y;
        Some(*st)
    }).collect();
    result.pop();
    result
}

pub fn repeat_to_len<T: Clone>(length: usize, content: &[T]) -> Vec<T> {
    content.iter().cycle().take(length).cloned().collect()
}

#[derive(Debug, Clone)]
pub struct Iter<'a, T> {
    /// position within the backing vector
    position: usize,
    /// slice of the backing vectors
    vecs: &'a [T],
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    #[inline]
    fn next(&mut self) -> Option<&'a T> {
       let result = self.vecs.get(self.position);
        self.inc();
        result
    }
}

impl<'a, T> Iter<'a, T> {
    #[inline]
    fn inc(&mut self) {
        self.position += 1
    }
}

impl<T: Debug + Clone + PartialEq + Default> PartialEq for Tensor<T> {
    fn eq(&self, other: &Self) -> bool {
        self.shape == other.shape // if the shapes aren't equal they can't be equal
        && self.backing == other.backing

    }
}

#[cfg(test)]
mod test {
    pub use super::*;

    macro_rules! tensor {
        ([$($n:literal),+] => $e:expr) => {{
            Tensor::from_array(
                &[$($n),+],
                $e
            )
        }}
    }

    #[test]
    fn get_at() {
        let tensor: Tensor<u32> = tensor![[3,2] => &[1, 2, 3, 4, 5, 6]];
        assert_eq!(tensor.try_get_at(&[1,2]).unwrap(), &2)
    }

    #[test]
    fn get_at_extension() {
        let tensor: Tensor<u32> = tensor![[3,3] => &[1, 2, 3, 4, 5, 6]];
        assert_eq!(tensor.try_get_at(&[3,2]).unwrap(), &2)
    }

    #[test]
    fn tensor_iter_works() {
        let tensor: Tensor<u32> = tensor![[3,3] => &[1, 2, 3, 4, 5, 6, 7, 8, 9]];
        let result = tensor.iter().cloned().reduce(|x, y| (x+y)).unwrap();
        assert_eq!(result, 45);
    }

    #[test]
    fn calculate_indices_correctly() {
        assert_eq!(calculate_indices(&[3, 3]), vec![1, 3])
    }
}

