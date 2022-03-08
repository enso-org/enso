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


// =====================
// === TextBlockLine ===
// =====================

/// Not an instance of `Tokenizer`, as it needs to know parent block's offset.
impl<T: HasTokens> TextBlockLine<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer, offset: usize) {
        for empty_line_spaces in &self.empty_lines {
            (NEWLINE, empty_line_spaces).feed_to(consumer);
        }
        (NEWLINE, offset, &self.text).feed_to(consumer);
    }
}



// =====================
// === Text Segments ===
// =====================

has_tokens!(SegmentPlain, self.value);
has_tokens!(SegmentRawEscape, BACKSLASH, self.code);
has_tokens!(SegmentExpr<T>, EXPR_QUOTE, self.value, EXPR_QUOTE);
has_tokens!(SegmentEscape, BACKSLASH, self.code);


// =================
// === RawEscape ===
// =================

has_tokens!(Unfinished);
has_tokens!(Invalid, self.str);
has_tokens!(Slash, BACKSLASH);
has_tokens!(Quote, FMT_QUOTE);
has_tokens!(RawQuote, RAW_QUOTE);


// ==============
// === Escape ===
// ==============

has_tokens!(EscapeCharacter, self.c);
has_tokens!(EscapeControl, self.name);
has_tokens!(EscapeNumber, self.digits);
has_tokens!(EscapeUnicode16, UNICODE16_INTRODUCER, self.digits);
has_tokens!(EscapeUnicode21, UNICODE21_OPENER.deref(), self.digits, UNICODE21_CLOSER.deref());
has_tokens!(EscapeUnicode32, UNICODE32_INTRODUCER, self.digits);


// =============
// === Block ===
// =============

has_tokens!(BlockLine<T>, self.elem, self.off);


// =============
// === Macro ===
// =============

// === Macro Segments ==

has_tokens!(MacroMatchSegment<T>, self.head, self.body);
has_tokens!(MacroAmbiguousSegment<T>, self.head, self.body);


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

// ===============
// === Invalid ===
// ===============

has_tokens!(Unrecognized, self.str);
has_tokens!(Unexpected<T>, self.stream);
has_tokens!(InvalidQuote, self.quote);
has_tokens!(InlineBlock, self.quote);


// ===================
// === Identifiers ===
// ===================

has_tokens!(Blank, BLANK_TOKEN);
has_tokens!(Var, self.name);
has_tokens!(Cons, self.name);
has_tokens!(Opr, self.name);
has_tokens!(Annotation, self.name);
has_tokens!(Mod, self.name, MOD_SUFFIX);
has_tokens!(InvalidSuffix<T>, self.elem, self.suffix);


// ==============
// === Number ===
// ==============

/// Helper to represent that optional number base has additional character.
struct NumberBase<T>(T);

has_tokens!(NumberBase<T>, self.0, NUMBER_BASE_SEPARATOR);
has_tokens!(Number, self.base.as_ref().map(NumberBase), self.int);
has_tokens!(DanglingBase, self.base, NUMBER_BASE_SEPARATOR);



// ============
// === Text ===
// ============


// === Lines ===

has_tokens!(TextLineRaw, RAW_QUOTE, self.text, RAW_QUOTE);
has_tokens!(TextLineFmt<T>, FMT_QUOTE, self.text, FMT_QUOTE);


// === TextBlockRaw ==

impl HasTokens for TextBlockRaw {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        (RAW_BLOCK_QUOTES, self.spaces).feed_to(consumer);
        for line in self.text.iter() {
            line.feed_to(consumer, self.offset);
        }
    }
}


// === TextBlockFmt ==

impl<T: HasTokens> HasTokens for TextBlockFmt<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        (FMT_BLOCK_QUOTES, self.spaces).feed_to(consumer);
        for line in self.text.iter() {
            line.feed_to(consumer, self.offset);
        }
    }
}


// === TextUnclosed ==

impl<T: HasTokens> HasTokens for TextUnclosed<T> {
    fn feed_to(&self, consumer: &mut impl TokenConsumer) {
        match &self.line {
            TextLine::TextLineRaw(line) => (RAW_QUOTE, &line.text).feed_to(consumer),
            TextLine::TextLineFmt(line) => (FMT_QUOTE, &line.text).feed_to(consumer),
        }
    }
}



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


// === Ambiguous ===

has_tokens!(Ambiguous<T>, self.segs);



// =====================
// === Spaceless AST ===
// =====================

spaceless_ast!(Comment);
spaceless_ast!(Documented<T>);
spaceless_ast!(Import<T>);
spaceless_ast!(Export<T>);
spaceless_ast!(JavaImport<T>);
spaceless_ast!(Mixfix<T>);
spaceless_ast!(Group<T>);
spaceless_ast!(SequenceLiteral<T>);
spaceless_ast!(TypesetLiteral<T>);
spaceless_ast!(Def<T>);
spaceless_ast!(Foreign);
spaceless_ast!(Modified<T>);



// =============
// === Tests ===
// =============

/// Tests for spacelesss AST. Other AST is covered by parsing tests that verify
/// that correct lengths and text representation are generated. Only spaceless AST
/// is not returned by the parser and can't be covered in this way.
#[cfg(test)]
mod tests {
    use super::*;

    // === Comment ===

    fn make_comment() -> Shape<Ast> {
        Comment { lines: vec![] }.into()
    }

    #[test]
    #[should_panic]
    fn comment_panics_on_repr() {
        make_comment().repr();
    }

    #[test]
    #[should_panic]
    fn comment_panics_on_length() {
        make_comment().len();
    }


    // === Import ===

    fn make_import() -> Shape<Ast> {
        let path = vec![Ast::var("Target")];
        Import { path, rename: None, isAll: false, onlyNames: None, hidingNames: None }.into()
    }

    #[test]
    #[should_panic]
    fn import_panics_on_repr() {
        make_import().repr();
    }

    #[test]
    #[should_panic]
    fn import_panics_on_length() {
        make_import().len();
    }


    // === Mixfix ===

    fn make_mixfix() -> Shape<Ast> {
        Mixfix { name: vec![], args: vec![] }.into()
    }

    #[test]
    #[should_panic]
    fn mixfix_panics_on_repr() {
        make_mixfix().repr();
    }

    #[test]
    #[should_panic]
    fn mixfix_panics_on_length() {
        make_mixfix().len();
    }


    // === Group ===

    fn make_group() -> Shape<Ast> {
        Group { body: None }.into()
    }

    #[test]
    #[should_panic]
    fn group_panics_on_repr() {
        make_group().repr();
    }

    #[test]
    #[should_panic]
    fn group_panics_on_length() {
        make_group().len();
    }


    // === Def ===

    fn make_def() -> Shape<Ast> {
        Def { name: Ast::cons("Foo"), args: vec![], body: None }.into()
    }

    #[test]
    #[should_panic]
    fn def_panics_on_repr() {
        make_def().repr();
    }

    #[test]
    #[should_panic]
    fn def_panics_on_length() {
        make_def().len();
    }

    // === Foreign ===

    fn make_foreign() -> Shape<Ast> {
        Foreign { indent: 0, lang: "Python".into(), code: vec![] }.into()
    }

    #[test]
    #[should_panic]
    fn foreign_panics_on_repr() {
        make_foreign().repr();
    }

    #[test]
    #[should_panic]
    fn foreign_panics_on_length() {
        make_foreign().len();
    }
}
