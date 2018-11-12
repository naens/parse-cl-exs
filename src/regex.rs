struct Operator {
    id: OperatorId,
    list: Vec<SubElement>
}

enum OperatorId {
    Not,
    Plus,
    Star,
    Question,
    Concat,
    And,
    Or
}

struct SymbolRange {
    from: char,
    to: char
}

enum SubElement {
    Operator(Operator),
    SymbolRange(SymbolRange),
    String(Vec<char>)
}
