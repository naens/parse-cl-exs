use std::collections::HashMap;

// reads lexer and parser grammars
pub mod gram_reader {

    // TODO: private enum types???

    // the grammar nodes and leaves
    pub enum GramItem {
        expression,
        disjunct, term, specifier, conjunct, element,
        relm, range, string, number
    }

    // successful result of a grammar parser function
    struct GramOk {
        item: GramItem,
        tail: &str,
    }

    // error result of a grammar parsing function
    struct GramErr {
        msg: &str,
        location: &str
    }

    // combined result of a grammar parsing functions
    enum GramRes {
        GramOk(GramOk),
        GramErr(GramErr)
    }

    pub fn read_grammar(text: String) -> Map<&str, GramItem> {
        let mut result = HashMap::new();
        result
    }

    // expr = <disjunct>, (<op-or>, <disjunct>)*
    fn read_expression(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::expression), text)
    }

    // disjunct = <term>, (<op-cat>, <term>)*
    fn read_disjunct(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::disjunct), text)
    }

    // term = <conjunct>, ((<op-and>| <op-diff>), <conjunct>)*
    fn read_term(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::term), text)
    }

    // def specifier = <op-star> | <op-opt> | <op-rep>
    fn read_specifier(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::specifier), text)
    }

    // conjunct = <op-neg>?, <element>, <specifier>?
    fn read_conjunct(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::conjunct), text)
    }

    // def element = <range> | <string> | <ident> | <subexpr>
    fn read_element(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::element), text)
    }

    // def relm = <string> | <number>
    fn read_relm(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::relm), text)
    }

    // range = <relm>, <range-op>, <relm>? | <range-op>, <relm>?
    fn read_range(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::range), text)
    }

    // subexpr = '(', <expression>, ')'
    fn read_subexpr(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::subexpr), text)
    }

    fn read_string(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::char), text)
    }

    fn read_number(text: &str) -> GramRes {
        GramRes(GramOK(GramItem::number), text)
    }

}
