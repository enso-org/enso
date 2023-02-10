//! Code that supports tokenization of Ast nodes, that powers several features like text
//! generation, obtaining size of source code, generating idmap.

use crate::*;



// ======================
// === Token Literals ===
// =====================

/// Token representing blank.
pub const BLANK_TOKEN: char = '_';

/// Symbol appearing after base of the number literal.
pub const NUMBER_BASE_SEPARATOR: char = '_';

/// Suffix to made a modifier from an operator
pub const MOD_SUFFIX: char = '=';

/// Symbol enclosing raw Text line.
pub const FMT_QUOTE: char = '\'';

/// Symbol enclosing formatted Text line.
pub const RAW_QUOTE: char = '"';

/// Symbol used to break lines in Text block.
pub const NEWLINE: char = '\n';

/// Symbol introducing escape segment in the Text.
pub const BACKSLASH: char = '\\';

/// Symbol enclosing expression segment in the formatted Text.
pub const EXPR_QUOTE: char = '`';

/// Symbol that introduces UTF-16 code in the formatted Text segment.
pub const UNICODE16_INTRODUCER: char = 'u';

/// String that opens "UTF-21" code in the formatted Text segment.
pub const UNICODE21_OPENER: &str = "u{";

/// String that closese "UTF-21" code in the formatted Text segment.
pub const UNICODE21_CLOSER: &str = "}";

/// Symbol that introduces UTF-16 code in the formatted Text segment.
pub const UNICODE32_INTRODUCER: char = 'U';

/// Quotes opening block of the raw text.
pub const RAW_BLOCK_QUOTES: &str = "\"\"\"";

/// Quotes opening block of the formatted text.
pub const FMT_BLOCK_QUOTES: &str = "'''";



// =============
// === Block ===
// =============

has_tokens!(BlockLine<T>, self.elem, self.off);



// ===============
// === Shifted ===
// ===============

has_tokens!(Shifted<T>, self.off, self.wrapped);
has_tokens!(ShiftedVec1<T>, self.head, self.tail);



// =============================================================================
// === Shape ===================================================================
// =============================================================================


// ===================
// === Identifiers ===
// ===================

has_tokens!(Blank, BLANK_TOKEN);
has_tokens!(Var, self.name);
has_tokens!(Cons, self.name);
has_tokens!(Opr, self.name);
has_tokens!(Annotation, self.name);
has_tokens!(Mod, self.name, MOD_SUFFIX);


// ==============
// === Number ===
// ==============

/// Helper to represent that optional number base has additional character.
struct NumberBase<T>(T);

has_tokens!(NumberBase<T>, self.0, NUMBER_BASE_SEPARATOR);
has_tokens!(Number, self.base.as_ref().map(NumberBase), self.int);



// ====================
// === Applications ===
// ====================

has_tokens!(Infix<T>, self.larg, self.loff, self.opr, self.roff, self.rarg);

has_tokens!(Prefix<T>, self.func, self.off, self.arg);
has_tokens!(SectionLeft<T>, self.arg, self.off, self.opr);
has_tokens!(SectionRight<T>, self.opr, self.off, self.arg);
has_tokens!(SectionSides<T>, self.opr);

// ==============
// === Module ===
// ==============

// === Module ==

impl<T: HasTokens> HasTokens for Module<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        let mut iter = self.lines.iter();
        if let Some(first_line) = iter.next() {
            first_line.feed_to(consumer);
        }
        for line in iter {
            (NEWLINE, line).feed_to(consumer);
        }
    }
}


// === Block ==

impl<T: HasTokens> HasTokens for Block<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        NEWLINE.feed_to(consumer);
        for empty_line_space in &self.empty_lines {
            (empty_line_space, NEWLINE).feed_to(consumer);
        }
        (self.indent, &self.first_line).feed_to(consumer);
        for line in &self.lines {
            (NEWLINE, line.elem.as_ref().map(|_| self.indent), line).feed_to(consumer);
        }
    }
}



// ============
// === Tree ===
// ============

impl<T: HasTokens> HasTokens for Tree<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        if let Some(str) = &self.leaf_info {
            Token::Str(str).feed_to(consumer)
        } else {
            for element in &self.span_info {
                match element {
                    SpanSeed::Space(SpanSeedSpace { space }) =>
                        Token::Off(*space).feed_to(consumer),
                    SpanSeed::Token(SpanSeedToken { token }) => Token::Str(token).feed_to(consumer),
                    SpanSeed::Child(SpanSeedChild { node }) => node.feed_to(consumer),
                }
            }
        }
        if let Some(str) = &self.trailing_token {
            Token::Str(str).feed_to(consumer)
        }
    }
}
