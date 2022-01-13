use crate::data::{DyOpPrimitive, FuncPrimitive, MonOpPrimitive, Number}; //TODO
use nom::{
    branch::alt,
    bytes::streaming::{tag, take_till},
    character::streaming::{char, digit1, line_ending, one_of, space1},
    combinator::{map, map_res, opt, value},
    multi::{separated_list0, separated_list1},
    sequence::{delimited, preceded, tuple},
    IResult, Parser,
};
use nom_unicode::streaming::{alpha1, alphanumeric0};
use std::rc::Rc;

pub mod ast;
mod string;
use string::parse_string;

use self::ast::{
    ArrayAtom, ArrayExpr, Atop, Definition, DerivedFunc, DyOpAtom, Expr, ExprList, Fork, FuncAtom,
    FuncExpr, Function, Index, LeftOperand, MonOpAtom, MonOpExpr, OpAtom, OperExpr, RightOperand,
};

const NUMBER_CHARACTERS: &str = "0123456789";
type ParseResult<'a, T> = IResult<&'a str, T>;

pub fn parse_exprlist(i: &str) -> ParseResult<ExprList> {
    separated_list1(parse_break, parse_expr)(i)
}

fn parse_expr(i: &str) -> ParseResult<Expr> {
    alt((
        map(parse_array_expr, |x| x.into()),
        map(parse_func_expr, |x| x.into()),
        map(parse_oper_expr, |x| x.into()),
    ))(i)
}

// Array expressions
fn parse_array_expr(i: &str) -> ParseResult<ArrayExpr> {
    alt((
        map(parse_array_atom, |x| x.into()),
        map(parse_mon_func, |x| x.into()),
        map(parse_dy_func, |x| x.into()),
        map(parse_string, |x| x.into()),
        map(parse_definition(parse_array_expr), |x| x.into()),
    ))(i)
}

fn parse_array_atom(i: &str) -> ParseResult<ArrayAtom> {
    alt((
        map(parse_nested, |x| x.into()),
        map(parse_multi, |x| x.into()),
        map(parse_number_literal, |x| x.into()),
        map(parse_char_literal, |x| x.into()),
        map(parse_name, |x| x.into()),
        map(parse_indexing, |x| x.into()),
        map(parse_array_glyphs, |x| x.into()),
    ))(i)
}

fn parse_array_glyphs(i: &str) -> ParseResult<ArrayAtom> {
    alt((
        value(ArrayAtom::Zilde, char('⍬')),
        value(ArrayAtom::Alpha, char('⍺')),
        value(ArrayAtom::Omega, char('⍵')),
        value(ArrayAtom::Dalpha, tag("⍺⍺")),
        value(ArrayAtom::Domega, tag("⍵⍵")),
    ))(i)
}

fn parse_indexing(i: &str) -> ParseResult<Box<Index>> {
    let (i, bigbit) = parse_array_atom(i)?;
    let (i, subscript) = parse_subscript(i)?;
    Ok((i, Box::new(Index::new(bigbit, subscript))))
}

fn parse_subscript(i: &str) -> ParseResult<Vec<Option<ArrayExpr>>> {
    delimited(
        char('['),
        separated_list0(char(';'), opt(parse_array_expr)),
        char(']'),
    )(i)
}

fn parse_multi(i: &str) -> ParseResult<Vec<ArrayExpr>> {
    separated_list1(space1, parse_array_expr)(i)
}

fn parse_nested(i: &str) -> ParseResult<Box<ArrayExpr>> {
    map(delimited(char('('), parse_array_expr, char(')')), Box::new)(i)
}

fn parse_mon_func(i: &str) -> ParseResult<(Function, Box<ArrayExpr>)> {
    tuple((parse_function, map(parse_array_expr, Box::new)))(i)
}
fn parse_dy_func(i: &str) -> ParseResult<(Function, Box<ArrayExpr>, Box<ArrayExpr>)> {
    map(
        tuple((parse_array_expr, parse_function, parse_array_expr)),
        |(l, f, r)| (f, Box::new(l), Box::new(r)),
    )(i)
}

// Function Expressions
fn parse_func_expr(i: &str) -> ParseResult<FuncExpr> {
    alt((
        map(parse_definition(parse_func_expr), |x| x.into()),
        map(parse_fork, |x| Box::new(x).into()),
        map(parse_atop, |x| Box::new(x).into()),
        map(parse_function, |x| x.into()),
    ))(i)
}

fn parse_function(i: &str) -> ParseResult<Function> {
    alt((
        map(parse_derived_func, |x| x.into()),
        map(parse_func_atom, |x| x.into()),
    ))(i)
}

fn parse_derived_func(i: &str) -> ParseResult<DerivedFunc> {
    map(
        tuple((map(parse_left_operand, Box::new), parse_monop_expr)),
        DerivedFunc::from,
    )(i)
}

fn parse_func_atom(i: &str) -> ParseResult<FuncAtom> {
    alt((
        map(parse_bracketed_expr(parse_func_expr), |x| x.into()),
        map(parse_defn, |x| x.into()),
        map(parse_name, |x| x.into()),
        map(parse_funcprimitive, |x| x.into()),
        value(FuncAtom::Dalpha, tag("⍺⍺")),
        value(FuncAtom::Domega, tag("⍵⍵")),
        value(FuncAtom::Del, char('∇')),
    ))(i)
}

fn parse_defn(i: &str) -> ParseResult<ExprList> {
    delimited(char('{'), parse_exprlist, char('}'))(i)
}

fn parse_left_operand(i: &str) -> ParseResult<LeftOperand> {
    alt((
        map(parse_array_atom, |x| x.into()),
        map(parse_func_atom, |x| x.into()),
        map(parse_derived_func, |x| x.into()),
    ))(i)
}

fn parse_right_operand(i: &str) -> ParseResult<RightOperand> {
    alt((
        map(parse_array_atom, |x| x.into()),
        map(parse_func_atom, |x| x.into()),
    ))(i)
}

fn parse_atop(i: &str) -> ParseResult<Atop> {
    alt((
        map(
            tuple((
                // normal atop case
                parse_function,
                parse_function,
            )),
            |x| x.into(),
        ),
        map(
            tuple((
                // left argument curried case
                parse_array_atom,
                parse_function,
            )),
            |x| x.into(),
        ),
    ))(i)
}

fn parse_fork(i: &str) -> ParseResult<Fork> {
    alt((
        map(
            tuple((
                // normal fork case
                parse_function,
                parse_function,
                parse_function,
            )),
            |x| x.into(),
        ),
        map(
            tuple((
                // left argument curried case
                parse_array_atom,
                parse_function,
                parse_function,
            )),
            |x| x.into(),
        ),
    ))(i)
}

//Operators
fn parse_oper_expr(i: &str) -> ParseResult<OperExpr> {
    alt((
        map(parse_monop_expr, |x| x.into()),
        map(parse_dyop_atom, |x| x.into()),
        map(parse_definition(parse_oper_expr), |x| x.into()),
        map(parse_bracketed_expr(parse_oper_expr), |x| x.into()),
        value(OperExpr::Dedel, tag("∇∇")),
    ))(i)
}

fn parse_monop_expr(i: &str) -> ParseResult<MonOpExpr> {
    alt((
        map(tuple((parse_dyop_atom, parse_right_operand)), |x| x.into()),
        map(parse_monop_atom, |x| x.into()),
    ))(i)
}

fn parse_op_atom<'a, O>(
    mut primparser: impl Parser<&'a str, O, nom::error::Error<&'a str>>,
) -> impl FnMut(&'a str) -> ParseResult<OpAtom<O>> {
    move |i| {
        if let Ok((i, prim)) = primparser.parse(i) {
            Ok((i, OpAtom::from_primitive(prim)))
        } else {
            alt((
                map(parse_name, |x| x.into()),
                map(parse_defn, |x| Box::new(x).into()),
            ))(i)
        }
    }
    // alt((
    //         map(parse_name, |x| x.into()),
    //         map(parse_defn, |x| Box::new(x).into()),
    //         map(primparser, OpAtom::from_primitive)
    // ))(i)
}

fn parse_monop_atom(i: &str) -> ParseResult<MonOpAtom> {
    parse_op_atom(parse_monop_prim)(i)
}

fn parse_dyop_atom(i: &str) -> ParseResult<DyOpAtom> {
    parse_op_atom(parse_dyop_prim)(i)
}

// Primitives
fn parse_definition<'a, O>(
    mut eparser: impl Parser<&'a str, O, nom::error::Error<&'a str>>,
) -> impl FnMut(&'a str) -> ParseResult<Definition<O>> {
    move |i| {
        let (i, name) = parse_name(i)?;
        let (i, _) = char('←')(i)?;
        let (i, expr) = eparser.parse(i)?;
        Ok((i, Definition::new(name, Box::new(expr))))
    }
}

fn parse_bracketed_expr<'a, O>(
    mut eparser: impl Parser<&'a str, O, nom::error::Error<&'a str>>,
) -> impl FnMut(&'a str) -> ParseResult<Box<O>> {
    move |i: &str| {
        let (i, _) = char('(')(i)?;
        let (i, res) = eparser.parse(i)?;
        let (i, _) = char('(')(i)?;
        Ok((i, Box::new(res)))
    }
    // map(delimited(
    //     char('('),
    //     eparser,
    //     char(')'),
    // ),
    //     Box::new
    // )(i)
}

fn parse_name(i: &str) -> ParseResult<Rc<String>> {
    let (i, first) = alpha1(i)?;
    let (i, rest) = alphanumeric0(i)?;
    let together = format!("{}{}", first, rest).into();
    Ok((i, together))
}

fn parse_number_literal(i: &str) -> ParseResult<Number> {
    let (i, realpart) = parse_number(i)?;
    if let Ok((i, _)) = one_of::<_, _, nom::error::Error<&str>>("jJ")(i) {
        let (i, imgpart) = parse_number(i)?;
        Ok((i, realpart.with_imaginary(imgpart)))
    } else {
        Ok((i, realpart))
    }
}

fn parse_number(i: &str) -> ParseResult<Number> {
    let (i, is_minus) = value(true, char('¯'))(i)?;
    let (i, bigpart) = opt(digit1)(i)?;
    if let Some(bigpart) = bigpart {
        let (i, smallpart) = opt(parse_decimal_segment)(i)?;
        Ok((i, Number::from_strs(is_minus, bigpart, smallpart)))
    } else {
        let (i, smallpart) = parse_decimal_segment(i)?;
        Ok((i, Number::from_str_fractional(is_minus, smallpart)))
    }
}

fn parse_decimal_segment(i: &str) -> ParseResult<&str> {
    preceded(char('.'), digit1)(i)
}

fn parse_endline(i: &str) -> ParseResult<()> {
    value(
        (),
        tuple((
            opt(char('⍝')),
            take_till(nom_unicode::is_control),
            line_ending,
        )),
    )(i)
}

fn parse_break(i: &str) -> ParseResult<()> {
    alt((value((), char('⋄')), parse_endline))(i)
}

fn parse_funcprimitive(i: &str) -> ParseResult<FuncPrimitive> {
    map_res(
        one_of("+-×÷*⍟⌊⌈|!○<≤=≥≠∧∨⍲⍱~?⍴⍳∊↑↓⍪,⍋⍒⌽⍉⊖∪⊃⊂∩⌷⊣⊢≡≢"),
        |c| c.try_into(),
    )(i)
}

fn parse_monop_prim(i: &str) -> ParseResult<MonOpPrimitive> {
    map_res(one_of("⍨¨/\\⌿⍀"), |c| c.try_into())(i)
}

fn parse_dyop_prim(i: &str) -> ParseResult<DyOpPrimitive> {
    map_res(one_of("⍤⍠∘⍣.⍥⌺/⌿%"), |c| c.try_into())(i)
}

fn parse_char_literal(i: &str) -> ParseResult<char> {
    delimited(
        char('\''),
        alt((
            value('\\', tag(r"\\")),
            value('\n', tag(r"\n")),
            value('\'', tag(r"\'")),
        )),
        char('\''),
    )(i)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn string_parses_correctly() {
        assert_eq!(Ok(("", Rc::new("banano".to_string()))), parse_string::<nom::error::Error<&str>>(r#""banano""#));
        assert_eq!(Ok(("", Rc::new("ba
ano".to_string()))), parse_string::<nom::error::Error<&str>>(r#""ba\nano""#))
    }
}
