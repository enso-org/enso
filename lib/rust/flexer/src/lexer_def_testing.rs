//! This file contains tests for the API used to _define_ a lexer.



// =============
// === Lexer ===
// =============

use crate::group::Group;
use crate::automata::nfa::NFA;
use crate::automata::dfa::DFA;

#[allow(missing_docs)]
#[derive(Clone,Debug)]
pub struct Lexer {
    groups: Vec<Group>
}

#[allow(missing_docs)]
impl Lexer {
    pub fn new() -> Self {
        let groups = Vec::new();
        Lexer{groups}
    }

    // TODO [AA] Parent
    pub fn define_group(&mut self,name:&str) -> &mut Group {
        let id = self.groups.len();
        let group = Group::new(id,String::from(name),None);
        self.groups.push(group);
        self.groups.get_mut(id).expect("Has just been pushed so should always exist.")
    }

    pub fn specialize(&self) -> String {
        let group_nfa:Vec<NFA> = self.groups.iter().map(|group|group.into()).collect();
        let _group_dfa:Vec<DFA> = group_nfa.iter().map(|nfa|nfa.into()).collect();
        let str = String::new();
        str
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use crate::lexer_def_testing::Lexer;
    use crate::automata::pattern::Pattern;



    #[test]
    fn try_generate_code() {
        let mut lexer = Lexer::new();

        let a_word        = Pattern::char('a').many1();
        let b_word        = Pattern::char('b').many1();
        let space         = Pattern::char(' ');
        let spaced_a_word = space.clone() >> a_word.clone();
        let spaced_b_word = space.clone() >> b_word.clone();
        let any           = Pattern::any();
        let end           = Pattern::eof();

        // The ordering here is necessary.
        let root_group = lexer.define_group("ROOT");
        root_group.create_rule(&a_word,"1 + 1");
        root_group.create_rule(&b_word,"2 + 2");
        root_group.create_rule(&end,"3 + 3");
        root_group.create_rule(&any,"4 + 4");

        let seen_first_word_group = lexer.define_group("SEEN_FIRST_WORD");
        seen_first_word_group.create_rule(&spaced_a_word,"5 + 5");
        seen_first_word_group.create_rule(&spaced_b_word,"6 + 6");
        seen_first_word_group.create_rule(&end,"7 + 7");
        seen_first_word_group.create_rule(&any,"8 + 8");

        let result = lexer.specialize();
        let expected = String::from("");

        assert_eq!(result,expected)
    }
}
