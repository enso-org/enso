//! A small utility allowing writing nicer looking code.



// ============
// === Void ===
// ============

/// A small utility allowing writing nicer looking code. Consider the following example:
///
/// ```text
/// let node = self.new_node_with(|node| {
//      init(node);
//  });
/// ```
/// 
/// It is impossible to write it in a more compact form when using Rust formatter, as the
/// `new_node_with` requires a closure which does not return a value. With this utility, the code
/// can be rewritten to:
/// ```text
/// let node = self.new_node_with(|node| init(node).void());
/// ```
#[allow(missing_docs)]
pub trait Void {
    #[inline(always)]
    fn void(&self) {}
}
impl<T> Void for T {}
