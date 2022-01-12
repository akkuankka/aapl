use derive_more::{Display, From};
#[derive(Clone, Copy, Debug, PartialEq, Eq, From, Display)]
#[display(fmt = "Error: {} unrecognised as glyph.", "_0")]
pub struct UnrecognisedCharError(char);

mod number;
use number::*;

impl std::error::Error for UnrecognisedCharError {}
pub use fprim::*;
mod fprim {
    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub enum FuncPrimitive {
        Plus,
        Minus,
        Times,
        Div,
        Star,
        Log,
        Floor,
        Ceiling,
        Pipe,
        Fact,
        Circle,
        Lt,
        LEq,
        Eq,
        Gt,
        GEq,
        NEq,
        And,
        Or,
        Nand,
        Nor,
        Not,
        Random,
        Rho,
        Iota,
        Epsilon,
        Take,
        Drop,
        Catenate,
        Cons,
        GradeUp,
        GradeDown,
        Flip,
        Transpose,
        SideFlip,
        DownShoe,
        RightShoe,
        LeftShoe,
        UpShoe,
        Squad,
        LeftTack,
        RightTack,
        Trequals,
        Tally,
        Replicate,
        RepFirst,
        Expand,
        ExpFirst,
    }

    use super::UnrecognisedCharError;
    use FuncPrimitive::*;

    impl TryFrom<char> for FuncPrimitive {
        type Error = UnrecognisedCharError;
        fn try_from(value: char) -> Result<Self, Self::Error> {
            Ok(match value {
                '+' => Plus,
                '-' => Minus,
                '×' => Times,
                '÷' => Div,
                '*' => Star,
                '⍟' => Log,
                '⌊' => Floor,
                '⌈' => Ceiling,
                '|' => Pipe,
                '!' => Fact,
                '○' => Circle,
                '<' => Lt,
                '≤' => LEq,
                '=' => Eq,
                '≥' => GEq,
                '≠' => NEq,
                '∧' => And,
                '∨' => Or,
                '⍲' => Nand,
                '⍱' => Nor,
                '~' => Not,
                '?' => Random,
                '⍴' => Rho,
                '⍳' => Iota,
                '∊' => Epsilon,
                '↑' => Take,
                '↓' => Drop,
                '⍪' => Cons,
                ',' => Catenate,
                '⍋' => GradeUp,
                '⍒' => GradeDown,
                '⌽' => Flip,
                '⍉' => Transpose,
                '⊖' => SideFlip,
                '∪' => DownShoe,
                '⊃' => RightShoe,
                '⊂' => LeftShoe,
                '∩' => UpShoe,
                '⌷' => Squad,
                '⊣' => LeftTack,
                '⊢' => RightTack,
                '≡' => Trequals,
                '≢' => Tally,
                '/' => Replicate,
                '\\' => Expand,
                '⌿' => RepFirst,
                '⍀' => ExpFirst,
                otherwise => return Err(otherwise.into()),
            })
        }
    }
} // literally in a module so i can fold it

pub use opprim::*;
mod opprim {
    use super::UnrecognisedCharError;
    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub enum MonOpPrimitive {
        Swap,
        Each,
        Reduce,
        Scan,
        SideReduce,
        SideScan,
    }

    impl TryFrom<char> for MonOpPrimitive {
        type Error = UnrecognisedCharError;
        fn try_from(value: char) -> Result<Self, Self::Error> {
            Ok(match value {
                '⍨' => MonOpPrimitive::Swap,
                '¨' => MonOpPrimitive::Each,
                '/' => MonOpPrimitive::Reduce,
                '\\' => MonOpPrimitive::Scan,
                '⌿' => MonOpPrimitive::SideReduce,
                '⍀' => MonOpPrimitive::SideScan,
                otherwise => return Err(otherwise.into()),
            })
        }
    }

    #[derive(Clone, Copy, PartialEq, Eq, Debug)]
    pub enum DyOpPrimitive {
        Rank,
        Domino,
        Jot,
        Power,
        Dot,
        OuterProduct,
        Over,
        Stencil,
        WinReduce,
        WinSideReduce,
    }

    impl TryFrom<char> for DyOpPrimitive {
        type Error = UnrecognisedCharError;
        fn try_from(val: char) -> Result<Self, Self::Error> {
            Ok(match val {
                '⍤' => DyOpPrimitive::Rank,
                '⍠' => DyOpPrimitive::Domino,
                '∘' => DyOpPrimitive::Jot,
                '⍣' => DyOpPrimitive::Power,
                '.' => DyOpPrimitive::Dot,
                '⍥' => DyOpPrimitive::Over,
                '⌺' => DyOpPrimitive::Stencil,
                '/' => DyOpPrimitive::WinReduce,
                '⌿' => DyOpPrimitive::WinSideReduce,
                '%' => DyOpPrimitive::OuterProduct,
                otherwise => return Err(otherwise.into()),
            })
        }
    }
}
