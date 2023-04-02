//! This module contains the mappings of widget visualization responses into metadata structs used
//! by the views.

use crate::prelude::*;

use crate::model::execution_context::VisualizationUpdateData;

use super::response;
use ide_view::graph_editor::component::node::input::widget;
use ide_view::graph_editor::WidgetUpdate;



pub fn deserialize_widget_update(
    data: &VisualizationUpdateData,
) -> (Vec<WidgetUpdate>, Vec<failure::Error>) {
    match serde_json::from_slice::<response::WidgetUpdates>(data) {
        Ok(response) => {
            let updates = response.into_iter().map(
                |(argument_name, fallable_widget)| -> FallibleResult<WidgetUpdate> {
                    let widget: Option<response::Widget> = fallable_widget.0.map_err(|e| {
                        e.context(format!(
                            "Failed to deserialize widget data for argument '{argument_name}'"
                        ))
                    })?;
                    let meta = widget.map(map_metadata);
                    let argument_name = argument_name.to_owned();
                    Ok(WidgetUpdate { argument_name, meta })
                },
            );

            updates.partition_result()
        }
        Err(err) => {
            let err =
                err.context("Failed to deserialize a list of arguments in widget response").into();
            (default(), vec![err])
        }
    }
}

fn map_metadata(resp: response::Widget) -> widget::Metadata {
    widget::Metadata { display: resp.display, config: map_config(resp.inner) }
}

fn map_config(inner: response::WidgetSpecific) -> widget::Config {
    match inner {
        response::WidgetSpecific::SingleChoice { label, values } => widget::single_choice::Config {
            label:   label.map(Into::into),
            entries: Rc::new(map_entries(&values)),
        }
        .into(),
        _ => widget::label::Config::default().into(),
    }
}

fn map_entries(choices: &[response::Choice]) -> Vec<widget::Entry> {
    choices.iter().map(map_entry).collect()
}

fn map_entry(choice: &response::Choice) -> widget::Entry {
    let value: ImString = (&choice.value).into();
    let label = choice.label.as_ref().map_or_else(|| value.clone(), |label| label.into());
    widget::Entry { required_import: None, value, label }
}
