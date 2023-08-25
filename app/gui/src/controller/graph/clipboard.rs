//! Copy-pasting nodes using the clipboard.
//!
//! # Clipboard Content Format
//!
//! We use a JSON-encoded [`ClipboardContent`] structure, marked with our custom [`MIME_TYPE`].
//! This way, we have a separate clipboard format for our application and can extend it in the
//! future.
//! We also support plain text pasting to make it easier to paste the content from other
//! applications, but only if the [`PLAIN_TEXT_PASTING_ENABLED`] is `true`. Allowing pasting plain
//! text can bring unnecessary security risks, like the execution of malicious code immediately
//! after pasting.
//!
//! To copy the node as plain text, the user can enter the editing node, select the node expression,
//! and copy it to the clipboard using the [`ensogl::Text`] functionality.

use crate::prelude::*;

use crate::controller::graph::Handle;
use crate::controller::graph::NewNodeInfo;
use crate::model::module::NodeMetadata;

use ensogl::system::web::clipboard;
use serde::Deserialize;
use serde::Serialize;



// =================
// === Constants ===
// =================

/// We use the `web` prefix to be able to use a custom MIME type. Typically browsers support a
/// restricted set of MIME types in the clipboard.
/// See [Clipboard pickling](https://github.com/w3c/editing/blob/gh-pages/docs/clipboard-pickling/explainer.md).
///
/// `application/enso` is not an officially registered MIME-type (yet), but it is not important for
/// our purposes.
const MIME_TYPE: &str = "web application/enso";
/// Whether to allow pasting nodes from plain text.
const PLAIN_TEXT_PASTING_ENABLED: bool = true;



// ==============
// === Errors ===
// ==============

#[derive(Debug, Clone, PartialEq, failure::Fail)]
#[fail(
    display = "`application/enso` MIME-type is used, but clipboard content has incorrect format."
)]
pub struct InvalidFormatError;

/// Clipboard payload.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
enum ClipboardContent {
    /// A single node that was copied from the application.
    Node(CopiedNode),
}

/// A single node that was copied from the application.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct CopiedNode {
    /// A whole node's expression (without a pattern).
    expression: String,
    /// Node's metadata.
    metadata:   Option<NodeMetadata>,
}

/// Copy the node to the clipboard.
pub fn copy_node(expression: String, metadata: Option<NodeMetadata>) -> FallibleResult {
    let text_data = Some(expression.clone());
    let content = ClipboardContent::Node(CopiedNode { expression, metadata });
    let text_repr = serde_json::to_string(&content)?;
    clipboard::write(text_repr.as_bytes(), MIME_TYPE.to_string(), text_data);
    Ok(())
}


/// Paste the node from the clipboard at a specific position.
///
/// As pasting is an asynchronous operation, we need to provide a callback for handling possible
/// errors.
pub fn paste_node(graph: &Handle, position: Vector2, on_error: fn(String)) {
    clipboard::read(
        MIME_TYPE.to_string(),
        paste_node_from_custom_format(graph, position, on_error),
        plain_text_fallback(graph, position, on_error),
    );
}

/// A standard callback for pasting node using our custom format.
fn paste_node_from_custom_format(
    graph: &Handle,
    position: Vector2,
    on_error: impl Fn(String) + 'static,
) -> impl Fn(Vec<u8>) + 'static {
    let graph = graph.clone_ref();
    let closure = move |content| -> FallibleResult {
        let _transaction = graph.module.get_or_open_transaction("Paste node");
        let string = String::from_utf8(content)?;
        if let Ok(content) = serde_json::from_str(&string) {
            match content {
                ClipboardContent::Node(node) => {
                    let expression = node.expression;
                    let metadata = node.metadata;
                    graph.new_node_at_position(position, expression, metadata)?;
                    Ok(())
                }
            }
        } else {
            Err(InvalidFormatError.into())
        }
    };
    move |content| {
        if let Err(err) = closure(content) {
            on_error(format!("Failed to paste node. {err}"));
        }
    }
}

/// An alternative callback for pasting node from plain text. It is used when [`MIME_TYPE`] is not
/// available in the clipboard, and only if [`PLAIN_TEXT_PASTING_ENABLED`]. Otherwise, it is a
/// noop.
fn plain_text_fallback(
    graph: &Handle,
    position: Vector2,
    on_error: impl Fn(String) + 'static,
) -> impl Fn(String) + 'static {
    let graph = graph.clone_ref();
    let closure = move |text| -> FallibleResult {
        if PLAIN_TEXT_PASTING_ENABLED {
            let _transaction = graph.module.get_or_open_transaction("Paste node");
            let expression = text;
            graph.new_node_at_position(position, expression, None)?;
        }
        Ok(())
    };
    move |text| {
        if let Err(err) = closure(text) {
            on_error(format!("Failed to paste node. {err}"));
        }
    }
}



// ===============
// === Helpers ===
// ===============

impl Handle {
    /// Create a new node at the provided position.
    fn new_node_at_position(
        &self,
        position: Vector2,
        expression: String,
        metadata: Option<NodeMetadata>,
    ) -> FallibleResult {
        let info = NewNodeInfo {
            expression,
            doc_comment: None,
            metadata,
            id: None,
            location_hint: double_representation::graph::LocationHint::End,
            introduce_pattern: true,
        };
        let ast_id = self.add_node(info)?;
        self.set_node_position(ast_id, position)?;
        Ok(())
    }
}
