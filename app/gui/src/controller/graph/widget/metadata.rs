//! This module contains the mappings of widget visualization responses into metadata structs used
//! by the views.

use crate::prelude::*;

use crate::model::execution_context::VisualizationUpdateData;

use super::response;
use ide_view::graph_editor::component::node::input::widget;
use ide_view::graph_editor::WidgetUpdate;


/// Deserialize a list of widget configurations from  visualization update data. Allows for partial
/// deserialization: if any of the widget configurations fails to deserialize, it will be skipped,
/// but the deserialization will continue. All errors are returned as a separate list.
pub fn deserialize_widget_update(
    data: &VisualizationUpdateData,
) -> (Vec<WidgetUpdate>, Vec<failure::Error>) {
    match serde_json::from_slice::<response::WidgetDefinitions>(data) {
        Ok(response) => {
            let updates = response.into_iter().map(
                |(argument_name, fallable_widget)| -> FallibleResult<WidgetUpdate> {
                    let widget: Option<response::WidgetDefinition> =
                        fallable_widget.widget.map_err(|e| {
                            let msg = "Failed to deserialize widget data for argument";
                            e.context(format!("{msg} '{argument_name}'"))
                        })?;
                    let meta = widget.map(to_metadata);
                    let argument_name = argument_name.to_owned();
                    Ok(WidgetUpdate { argument_name, meta })
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

fn to_metadata(resp: response::WidgetDefinition) -> widget::Metadata {
    widget::Metadata { display: resp.display, config: to_config(resp.inner), has_port: true }
}

fn to_config(inner: response::WidgetKindConfiguration) -> widget::Config {
    match inner {
        response::WidgetKindConfiguration::SingleChoice { label, values } =>
            widget::single_choice::Config {
                label:   label.map(Into::into),
                entries: Rc::new(to_entries(&values)),
            }
            .into(),
        response::WidgetKindConfiguration::ListEditor { item_editor, item_default } =>
            widget::vector_editor::Config {
                item_editor:  Some(Rc::new(to_metadata(*item_editor))),
                item_default: item_default.into(),
            }
            .into(),
        _ => widget::label::Config::default().into(),
    }
}

fn to_entries(choices: &[response::Choice]) -> Vec<widget::Entry> {
    choices.iter().map(to_entry).collect()
}

fn to_entry(choice: &response::Choice) -> widget::Entry {
    let value: ImString = (&choice.value).into();
    let label = choice.label.as_ref().map_or_else(|| value.clone(), |label| label.into());
    widget::Entry { required_import: None, value, label }
}
