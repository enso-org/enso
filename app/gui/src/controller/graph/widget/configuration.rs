//! This module contains the mappings of widget visualization responses into metadata structs used
//! by the views.

use crate::prelude::*;

use crate::model::execution_context::VisualizationUpdateData;

use super::response;
use ide_view::graph_editor::component::node::input::widget;
use ide_view::graph_editor::ArgumentWidgetConfig;



/// =====================================
/// == deserialize_widget_definitions ===
/// =====================================

/// Deserialize a list of widget configurations from definitions provided in visualization update
/// data. Allows for partial deserialization: if any of the widget definitions fails to deserialize,
/// it will be skipped, but the deserialization will continue. All errors are returned as a separate
/// list.
pub fn deserialize_widget_definitions(
    data: &VisualizationUpdateData,
) -> (Vec<ArgumentWidgetConfig>, Vec<failure::Error>) {
    match serde_json::from_slice::<response::WidgetDefinitions>(data) {
        Ok(response) => {
            let updates = response.into_iter().map(
                |(argument_name, fallable_widget)| -> FallibleResult<ArgumentWidgetConfig> {
                    let widget: Option<response::WidgetDefinition> =
                        fallable_widget.widget.map_err(|e| {
                            let msg = "Failed to deserialize widget data for argument";
                            e.context(format!("{msg} '{argument_name}'"))
                        })?;
                    let meta = widget.map(to_configuration);
                    let argument_name = argument_name.into_owned();
                    Ok(ArgumentWidgetConfig { argument_name, config: meta })
                },
            );

            updates.partition_result()
        }
        Err(err) => {
            let msg = "Failed to deserialize a list of arguments in widget response";
            let err = err.context(msg).into();
            (default(), vec![err])
        }
    }
}

/// == Conversion to Widget Configuration IDE structs ===

/// Convert a widget definition from the engine response into a IDE internal widget configuration
/// struct. See [`widget::Configuration`] for more information.
fn to_configuration(resp: response::WidgetDefinition) -> widget::Configuration {
    widget::Configuration { display: resp.display, kind: to_kind(resp.inner), has_port: true }
}

fn to_kind(inner: response::WidgetKindDefinition) -> widget::DynConfig {
    match inner {
        response::WidgetKindDefinition::SingleChoice { label, values } =>
            widget::single_choice::Config {
                label:   label.map(Into::into),
                entries: Rc::new(to_entries(values)),
            }
            .into(),
        response::WidgetKindDefinition::ListEditor { item_widget, item_default } =>
            widget::list_editor::Config {
                item_widget:  Some(Rc::new(to_configuration(*item_widget))),
                item_default: ImString::from(item_default).into(),
            }
            .into(),
        _ => widget::label::Config::default().into(),
    }
}

fn to_entries(choices: Vec<response::Choice>) -> Vec<widget::Entry> {
    choices.into_iter().map(to_entry).collect()
}

fn to_entry(choice: response::Choice) -> widget::Entry {
    let value: ImString = choice.value.into();
    let label = choice.label.map_or_else(|| value.clone(), |label| label.into());
    widget::Entry { required_import: None, value, label }
}
