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



// ===============
// === Builder ===
// ===============

has_tokens!(Empty);
has_tokens!(Letter, self.char);
has_tokens!(Space, self);
has_tokens!(Text, self.str);
has_tokens!(Seq, self.first, self.second);



// =============
// === Block ===
// =============

has_tokens!(BlockLine<T>, self.elem, self.off);


// =============
// === Macro ===
// =============

// === Macro Segments ==

has_tokens!(MacroMatchSegment<T>, self.head, self.body);


// === MacroPatternMatch subtypes ===

has_tokens!(MacroPatternMatchRawBegin);
has_tokens!(MacroPatternMatchRawEnd);
has_tokens!(MacroPatternMatchRawNothing);
has_tokens!(MacroPatternMatchRawSeq<T>, self.elem);
has_tokens!(MacroPatternMatchRawOr<T>, self.elem);
has_tokens!(MacroPatternMatchRawMany<T>, self.elem);
has_tokens!(MacroPatternMatchRawExcept<T>, self.elem);
has_tokens!(MacroPatternMatchRawBuild<T>, self.elem);
has_tokens!(MacroPatternMatchRawErr<T>, self.elem);
has_tokens!(MacroPatternMatchRawTag<T>, self.elem);
has_tokens!(MacroPatternMatchRawCls<T>, self.elem);
has_tokens!(MacroPatternMatchRawTok<T>, self.elem);
has_tokens!(MacroPatternMatchRawBlank<T>, self.elem);
has_tokens!(MacroPatternMatchRawVar<T>, self.elem);
has_tokens!(MacroPatternMatchRawCons<T>, self.elem);
has_tokens!(MacroPatternMatchRawOpr<T>, self.elem);
has_tokens!(MacroPatternMatchRawAnnotation<T>, self.elem);
has_tokens!(MacroPatternMatchRawMod<T>, self.elem);
has_tokens!(MacroPatternMatchRawNum<T>, self.elem);
has_tokens!(MacroPatternMatchRawText<T>, self.elem);
has_tokens!(MacroPatternMatchRawBlock<T>, self.elem);
has_tokens!(MacroPatternMatchRawMacro<T>, self.elem);
has_tokens!(MacroPatternMatchRawInvalid<T>, self.elem);
has_tokens!(MacroPatternMatchRawFailedMatch);


// === Switch ===

has_tokens!(Switch<T>, self.deref());


// === Shifted ===

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
        (!self.is_orphan).as_some(NEWLINE).feed_to(consumer);
        for empty_line_space in &self.empty_lines {
            (empty_line_space, NEWLINE).feed_to(consumer);
        }
        (self.indent, &self.first_line).feed_to(consumer);
        for line in &self.lines {
            (NEWLINE, line.elem.as_ref().map(|_| self.indent), line).feed_to(consumer);
        }
    }
}



// ==============
// === Macros ===
// ==============

// === Match ==

impl<T: HasTokens> HasTokens for Match<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        for pat_match in &self.pfx {
            for sast in pat_match.iter() {
                // reverse the order for prefix: ast before spacing
                (&sast.wrapped, &sast.off).feed_to(consumer);
            }
        }
        self.segs.feed_to(consumer);
    }
}



// ============
// === Tree ===
// ============

impl HasTokens for Tree {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        for dust in &self.span_info {
            Token::from(dust).feed_to(consumer)
        }
    }
}
