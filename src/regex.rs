use std::collections::BTreeSet;

#[derive(Eq,Ord,PartialOrd,PartialEq)]
pub enum Operator {
    Not(Box<SubElement>),
    Plus(Box<SubElement>),
    Star(Box<SubElement>),
    Option(Box<SubElement>),
    Concat(Vec<SubElement>),
    And(BTreeSet<SubElement>),
    Or(BTreeSet<SubElement>)
}

#[derive(Eq,Ord,PartialOrd,PartialEq)]
pub struct SymbolRange {
    from: char,
    to: char
}

#[derive(Eq,Ord,PartialOrd,PartialEq)]
pub enum SubElement {
    Operator(Operator),
    SymbolRange(SymbolRange),
    String(Vec<char>)
}

pub fn make_not(se: SubElement) -> Operator {
    Operator::Not(Box::new(se))
}

pub fn make_plus(se: SubElement) -> Operator {
    Operator::Plus(Box::new(se))
}

pub fn make_star(se: SubElement) -> Operator {
    Operator::Star(Box::new(se))
}

pub fn make_option(se: SubElement) -> Operator {
    Operator::Option(Box::new(se))
}

pub fn make_concat() -> Operator {
    Operator::Concat(Vec::new())
}

pub fn make_and() -> Operator {
    Operator::And(BTreeSet::new())
}

pub fn make_or() -> Operator {
    Operator::Or(BTreeSet::new())
}
