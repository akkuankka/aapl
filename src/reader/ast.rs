use crate::data::Number;
use crate::data::{DyOpPrimitive, FuncPrimitive, MonOpPrimitive};
use derive_more::From;
use std::rc::Rc;

pub type ExprList = Vec<Expr>;
#[derive(Clone, Debug, From, PartialEq)]
pub enum Expr {
    Array(ArrayExpr),
    Func(FuncExpr),
    OperExpr(OperExpr),
}

/// The definition of a value, such as bar<-foo
#[derive(Clone, Debug, From, PartialEq)]
pub struct Definition<T> {
    name: Rc<String>,
    val: Box<T>,
}

impl<T> Definition<T> {
    pub fn new(name: Rc<String>, val: Box<T>) -> Self  {
        Self {
            name, val
        }
    }
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum ArrayExpr {
    Atom(ArrayAtom),
    MonFunction {
        f: Function,
        arg: Box<ArrayExpr>,
    },
    DyFunction {
        f: Function,
        larg: Box<ArrayExpr>,
        rarg: Box<ArrayExpr>,
    },
    String(Rc<String>),
    Definition(Definition<ArrayExpr>),
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum ArrayAtom {
    Name(Rc<String>),
    Number(Number),
    Char(char),
    Idx(Box<Index>),
    Multi(Vec<ArrayExpr>),
    Nested(Box<ArrayExpr>),
    Zilde,
    Alpha,
    Omega,
    Dalpha,
    Domega,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Index {
    val: ArrayAtom,
    subscripts: Vec<Option<ArrayExpr>>,
}

impl Index {
    pub fn new(val: ArrayAtom, subscripts: Vec<Option<ArrayExpr>>) -> Self {
        Self { val, subscripts }
    }
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum NameStructure {
    Just(Rc<String>),
    More(Vec<NameStructure>),
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum FuncExpr {
    Function(Function),
    Atop(Box<Atop>),
    Fork(Box<Fork>),
    Definition(Definition<FuncExpr>),
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum Function {
    Atom(FuncAtom),
    Derived(DerivedFunc),
}

#[derive(Clone, Debug, From, PartialEq)]
pub struct DerivedFunc {
    operand: Box<LeftOperand>,
    operator: MonOpExpr,
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum FuncAtom {
    Name(Rc<String>),
    Prim(FuncPrimitive),
    Defn(ExprList),
    Dalpha,
    Domega,
    Del,
    Bracket(Box<FuncExpr>),
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum LeftOperand {
    Array(ArrayAtom),
    Function(FuncAtom),
    Derived(DerivedFunc),
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum RightOperand {
    Array(ArrayAtom),
    Function(FuncAtom),
}

#[derive(PartialEq, Debug, From, Clone)]
pub enum Atop {
    LeftCurried { left: ArrayAtom, f: Function },
    Normal { top: Function, bottom: Function },
}

#[derive(Debug, Clone, From, PartialEq)]
pub enum Fork {
    LeftCurried {
        with: ArrayAtom,
        left: Function,
        right: Function,
    }, // first argument curried left
    Normal {
        left: Function,
        centre: Function,
        right: Function,
    },
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum OperExpr {
    Monadic(MonOpExpr),
    Dyadic(DyOpAtom),
    OperDefinition(Definition<OperExpr>),
    Bracket(Box<OperExpr>),
    Dedel,
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum MonOpExpr {
    Atom(MonOpAtom),
    AppDyad { op: DyOpAtom, rand: RightOperand },
}

pub type MonOpAtom = OpAtom<MonOpPrimitive>;
pub type DyOpAtom = OpAtom<DyOpPrimitive>;

#[derive(Clone, Debug, From, PartialEq)]
pub enum OpAtom<P> {
    Name(Rc<String>),
    #[from(ignore)]
    Prim(P),
    Defined(Box<ExprList>),
}

impl<P> OpAtom<P> {
    pub fn from_primitive(prim: P) -> OpAtom<P> {
        OpAtom::Prim(prim)
    }
}
