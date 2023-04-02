//! The module containing the types used for deserializing language-server responses containing
//! widget configuration.

use crate::prelude::*;

use ide_view::graph_editor::component::node::input::widget;



/// =====================
/// === WidgetUpdates ===
/// =====================

/// A complete object received from the widget visualization. Contain widget definitions for all
/// requested arguments.
///
/// Pairs of argument name and associated widget configuration.
pub(super) type WidgetUpdates<'a> = Vec<(&'a str, FallableWidgetData<'a>)>;

/// A wrapper type that allows deserialization of a widget to partially fail. Failure message of
/// individual widget data deserialization will be preserved, and it will be allowed to continue
/// deserializing other widgets.
#[derive(Debug)]
pub(super) struct FallableWidgetData<'a>(pub(super) FallibleResult<Option<Widget<'a>>>);

impl<'de: 'a, 'a> serde::Deserialize<'de> for FallableWidgetData<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: serde::Deserializer<'de> {
        let widget = <Option<Widget>>::deserialize(deserializer)
            .map_err(|e| failure::err_msg(e.to_string()));
        Ok(Self(widget))
    }
}


/// ==============
/// === Widget ===
/// ==============

/// Describes the widget configuration, as provided by the node metadata from the engine.
///
/// Must be kept in sync with `Widget` type definition in `Standard.Base.Metadata` module.
/// In order to not ruin forward compatibility, only fields that are currently used by the IDE are
/// specified and deserialized.
#[derive(Debug, serde::Deserialize)]
pub(super) struct Widget<'a> {
    /// The display mode for the parameter.
    #[serde(default)]
    pub display: widget::Display,
    #[serde(borrow, flatten)]
    pub inner:   WidgetSpecific<'a>,
}

/// Widget type dependant fields.
#[derive(Debug, serde::Deserialize)]
#[serde(tag = "constructor")]
pub(super) enum WidgetSpecific<'a> {
    /// Describes a single value widget (dropdown).
    #[serde(rename = "Single_Choice")]
    SingleChoice {
        /// The placeholder text value. By default, the parameter name is used.
        #[serde(borrow, default)]
        label:  Option<&'a str>,
        /// A list of choices to display.
        #[serde(borrow, default)]
        values: Vec<Choice<'a>>,
    },

    /// Describes a list editor widget producing a Vector.
    /// Items can be dragged around to change the order, or dragged out to be deleted from the
    /// Vector.
    #[serde(rename = "Vector_Editor")]
    VectorEditor {
        /// The widget to use for editing the items.
        #[serde(borrow)]
        item_editor:  Box<Widget<'a>>,
        /// The default value for new items inserted when the user clicks the `+` button.
        #[serde(borrow)]
        item_default: &'a str,
    },

    /// Describes a multi value widget.
    #[serde(rename = "Multi_Choice")]
    MultipleChoice,

    /// Describe a code parameter.
    #[serde(rename = "Code_Input")]
    CodeInput,

    /// Describe a boolean parameter.
    #[serde(rename = "Boolean_Input")]
    BooleanInput,

    /// Describe a numeric parameter.
    #[serde(rename = "Numeric_Input")]
    NumericInput,

    /// Describes a text widget.
    #[serde(rename = "Text_Input")]
    TextInput,

    /// Describes a folder chooser.
    #[serde(rename = "Folder_Browse")]
    FolderBrowse,

    /// Describes a file chooser.
    #[serde(rename = "File_Browse")]
    FileBrowse,
}

/// Widget display mode. Determines when the widget should be expanded.
#[derive(serde::Deserialize, Debug, Clone, Copy, Default, PartialEq, Eq)]
#[serde(tag = "constructor")]
pub enum Display {
    /// The widget should always be in its expanded mode.
    #[default]
    Always,
    /// The widget should only be in its expanded mode when it has non-default value.
    #[serde(rename = "When_Modified")]
    WhenModified,
    /// The widget should only be in its expanded mode whe the whole node is expanded.
    #[serde(rename = "Expanded_Only")]
    ExpandedOnly,
}

/// A choice in a single or multiselect widget.
#[derive(Debug, serde::Deserialize)]
pub(super) struct Choice<'a> {
    /// The value of the choice. Must be a valid code expression.
    pub value: &'a str,
    /// Custom label to display in the dropdown. If not provided, IDE will create a label based on
    /// value.
    #[serde(borrow)]
    pub label: Option<&'a str>,
}
