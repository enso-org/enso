// === Module ===

/// The ast node for a module that represents the file's root block.
///
/// The module consists of a sequence of possibly empty lines with no leading indentation.
#[allow(missing_docs)]
#[derive(Debug,Clone)]
pub struct Module { pub lines: Vec<Option<AnyAst>> }


// === Block ===

/// The ast node for a block that represents a sequence of equally indented lines.
///
/// Lines may contain some child ast or be empty. Block is used for all code blocks except for
/// the root one, which uses `Module`.
#[derive(Debug,Clone)]
pub struct Block {
    /// Absolute's block indent, counting from the module's root.
    pub indent: usize,
    /// Leading empty lines. Each line is represented by absolute count of spaces
    /// it contains, counting from the root.
    pub empty_lines: Vec<usize>,
    /// First line with non-empty item.
    pub first_line: Box<AnyAst>,
    /// Rest of lines, each of them optionally having contents.
    pub lines: Vec<Option<AnyAst>>,
}
