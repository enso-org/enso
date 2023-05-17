//! The module containing the types used for deserializing language-server responses containing
//! widget configuration.

use crate::prelude::*;

use ide_view::graph_editor::component::node::input::widget;



// =========================
// === WidgetDefinitions ===
// =========================

/// A top level object received from the widget visualization, which contains widget definitions for
/// all arguments of a single Enso method. Configurations are paired with the name of function
/// argument they are associated with.
pub(super) type WidgetDefinitions<'a> = Vec<(Cow<'a, str>, FallableWidgetDefinition<'a>)>;

/// A wrapper type that allows deserialization of a widget definitions to partially fail: failure
/// message of individual widget definition deserialization will be preserved and deserialization
/// will continue.
#[derive(Debug)]
pub(super) struct FallableWidgetDefinition<'a> {
    pub(super) widget: FallibleResult<Option<WidgetDefinition<'a>>>,
}

impl<'de: 'a, 'a> serde::Deserialize<'de> for FallableWidgetDefinition<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: serde::Deserializer<'de> {
        let widget = <Option<WidgetDefinition>>::deserialize(deserializer)
            .map_err(|e| failure::err_msg(e.to_string()));
        Ok(Self { widget })
    }
}



// ========================
// === WidgetDefinition ===
// ========================

/// Widget definition provided from the engine. It is used to define how to display a widget of
/// particular argument expression. When not provided, the default widget will be chosen based on
/// value or expected argument type.
///
/// Must be kept in sync with `Widget` type definition in Enso's `Standard.Base.Metadata` module.
/// In order to not ruin forward compatibility, only fields that are currently used by the IDE are
/// specified and deserialized.
#[derive(Debug, serde::Deserialize)]
pub(super) struct WidgetDefinition<'a> {
    /// The display mode of this widget.
    #[serde(default)]
    pub display: widget::Display,
    #[serde(borrow, flatten)]
    pub inner:   WidgetKindDefinition<'a>,
}

/// Part of [`WidgetDefinition`] that is dependant on widget kind.
///
/// NOTE: Using `Cow<'a, str>` instead of `&'a str` is important here, because `serde_json` does not
/// support deserializing into borrowed str when the received value contains escape sequences. In
/// those cases (and ONLY then), the borrow serialization would fail. Using `Cow` allows us to
/// deserialize into borrowed strings when possible, but falls back to allocation in rare cases when
/// it is not.
/// See: https://github.com/serde-rs/json/issues/742
#[derive(Debug, serde::Deserialize)]
#[serde(tag = "constructor")]
pub(super) enum WidgetKindDefinition<'a> {
    /// A single value widget (dropdown).
    #[serde(rename = "Single_Choice")]
    SingleChoice {
        /// The text that is displayed when no value is chosen. By default, the parameter name is
        /// used.
        #[serde(borrow, default)]
        label:  Option<Cow<'a, str>>,
        /// A list of choices to display.
        #[serde(borrow, default)]
        values: Vec<Choice<'a>>,
    },

    /// A list editor widget producing a Vector. Items can be dragged around to change the order,
    /// or dragged out to be deleted from the Vector.
    #[serde(rename = "List_Editor", alias = "Vector_Editor")]
    ListEditor {
        /// The widget to use for editing the items.
        #[serde(borrow, alias = "item_editor")]
        item_widget:  Box<WidgetDefinition<'a>>,
        /// The default value for new items inserted when the user adds a new element.
        #[serde(borrow)]
        item_default: Cow<'a, str>,
    },

    /// A multi value widget.
    #[serde(rename = "Multi_Choice")]
    MultipleChoice,

    /// A code parameter.
    #[serde(rename = "Code_Input")]
    CodeInput,

    /// A boolean parameter.
    #[serde(rename = "Boolean_Input")]
    BooleanInput,

    /// A numeric parameter.
    #[serde(rename = "Numeric_Input")]
    NumericInput,

    /// A text widget.
    #[serde(rename = "Text_Input")]
    TextInput,

    /// A folder chooser.
    #[serde(rename = "Folder_Browse")]
    FolderBrowse,

    /// A file chooser.
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
    /// The value of the choice. Must be a valid Enso expression.
    pub value: Cow<'a, str>,
    /// Custom label to display in the dropdown. If not provided, IDE will create a label based on
    /// value.
    #[serde(borrow)]
    pub label: Option<Cow<'a, str>>,
}
