mod gram_reader;
#[macro_use] extern crate matches;

#[test]
pub fn abcd() {
    assert!(matches!( (12, 13), (12, _) ));
}
