// reads lexer and parser grammars
use std::collections::HashMap;

// TODO: private enum types???

// the grammar nodes and leaves
pub enum GramItem<'a> {
    Expression,
    Disjunct, Term, Specifier, Conjunct, Element,
    Range, Subexpr, String(&'a str), Number(i32)
}

// combined result of a grammar parsing functions
enum GramRes<'a> {
    GramOk{item: GramItem<'a>, tail: &'a str},
    GramErr{msg: &'static str, tail: &'a str}
}

pub fn read_grammar<'a>(text: String)
            -> Result<HashMap<&'a str, GramItem<'a>>, &'static str> {
    let mut result = HashMap::new();
    Ok(result)
}

// expr = <disjunct>, (<op-or>, <disjunct>)*
fn read_expression(text: &str) -> GramRes {
    GramRes::GramErr{msg: "unimplemented", tail: text}
}

// disjunct = <term>, (<op-cat>, <term>)*
fn read_disjunct(text: &str) -> GramRes {
    GramRes::GramErr{msg: "unimplemented", tail: text}
}

// term = <conjunct>, ((<op-and>| <op-diff>), <conjunct>)*
fn read_term(text: &str) -> GramRes {
GramRes::GramErr{msg: "unimplemented", tail: text}
}

// def specifier = <op-star> | <op-opt> | <op-rep>
fn read_specifier(text: &str) -> GramRes {
    GramRes::GramErr{msg: "unimplemented", tail: text}
}

// conjunct = <op-neg>?, <element>, <specifier>?
fn read_conjunct(text: &str) -> GramRes {
    GramRes::GramErr{msg: "unimplemented", tail: text}
}

// def element = <range> | <string> | <ident> | <subexpr>
fn read_element(text: &str) -> GramRes {
    GramRes::GramErr{msg: "unimplemented", tail: text}
}

// range = <number>, <range-op>, <number>? | <range-op>, <number>?
fn read_range(text: &str) -> GramRes {
    GramRes::GramErr{msg: "unimplemented", tail: text}
}

// subexpr = '(', <expression>, ')'
fn read_subexpr(text: &str) -> GramRes {
GramRes::GramErr{msg: "unimplemented", tail: text}
}

// sequences of characters in quotes
// If 2 strings follow each other, they are joined in single string.
// Two quote characters are not replaced in order to avoid making a copy.
// Inside strings everything is allowed.
fn read_string(text: &str) -> GramRes {
GramRes::GramErr{msg: "unimplemented", tail: text}
}

// numbers can be hexadecimal, decimal or single characters in quotes
fn read_number(text: &str) -> GramRes {
    GramRes::GramOk{item: GramItem::Number(-1), tail: text}
}

fn read_num_dec(text: &str) -> GramRes {
    GramRes::GramOk{item: GramItem::Number(-1), tail: text}
}

fn read_num_hex(text: &str) -> GramRes {
    GramRes::GramOk{item: GramItem::Number(-1), tail: text}
}

fn read_num_chr(text: &str) -> GramRes {
GramRes::GramOk{item: GramItem::Number(-1), tail: text}
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn read_number_test() {
        let res = read_number("0");
        assert!(matches!(res, GramRes::GramOk{..}));
    }
}
