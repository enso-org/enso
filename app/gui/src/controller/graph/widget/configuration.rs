//! This module contains the mappings of widget visualization responses into metadata structs used
//! by the views.

use crate::prelude::*;

use crate::model::execution_context::VisualizationUpdateData;

use super::response;
use enso_suggestion_database::entry::argument_tag_values;
use enso_suggestion_database::SuggestionDatabase;
use ide_view::graph_editor::component::node::input::widget;
use ide_view::graph_editor::component::node::input::widget::single_choice::ChoiceArgConfig;
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
    db: &SuggestionDatabase,
    parser: &parser::Parser,
) -> (Vec<ArgumentWidgetConfig>, Vec<failure::Error>) {
    match serde_json::from_slice::<response::WidgetDefinitions>(data) {
        Ok(response) => {
            let updates = response.into_iter().map(|arg| -> FallibleResult<ArgumentWidgetConfig> {
                let widget: Option<response::WidgetDefinition> = arg.data.widget.map_err(|e| {
                    let msg = "Failed to deserialize widget data for argument";
                    e.context(format!("{msg} '{}'", arg.name))
                })?;
                let meta = widget.map(|resp| to_configuration(resp, db, parser));
                let argument_name = arg.name.into_owned();
                Ok(ArgumentWidgetConfig { argument_name, config: meta })
            });

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
fn to_configuration(
    resp: response::WidgetDefinition,
    db: &SuggestionDatabase,
    parser: &parser::Parser,
) -> widget::Configuration {
    widget::Configuration {
        display:  resp.display,
        kind:     to_kind(resp.inner, db, parser),
        has_port: true,
    }
}

fn to_kind(
    inner: response::WidgetKindDefinition,
    db: &SuggestionDatabase,
    parser: &parser::Parser,
) -> widget::DynConfig {
    match inner {
        response::WidgetKindDefinition::SingleChoice { label, values } => {
            let (choices, arguments) = to_choices_and_arguments(values, db, parser);
            widget::single_choice::Config {
                label: label.map(Into::into),
                choices: Rc::new(choices),
                arguments,
            }
            .into()
        }
        response::WidgetKindDefinition::ListEditor { item_widget, item_default } =>
            widget::list_editor::Config {
                item_widget:  Some(Rc::new(to_configuration(*item_widget, db, parser))),
                item_default: ImString::from(item_default).into(),
            }
            .into(),
        _ => widget::label::Config::default().into(),
    }
}

fn to_choices_and_arguments(
    choices: Vec<response::Choice>,
    db: &SuggestionDatabase,
    parser: &parser::Parser,
) -> (Vec<widget::Choice>, Vec<ChoiceArgConfig>) {
    let mut args = Vec::new();

    let expressions = choices.iter().map(|c| c.value.as_ref());
    let as_tags = argument_tag_values(expressions, db, parser);

    let entries = choices
        .into_iter()
        .enumerate()
        .zip(as_tags)
        .map(|((index, choice), tag)| to_widget_choice(choice, index, db, parser, tag, &mut args))
        .collect();
    (entries, args)
}

fn to_widget_choice(
    choice: response::Choice,
    choice_index: usize,
    db: &SuggestionDatabase,
    parser: &parser::Parser,
    tag: span_tree::TagValue,
    arguments: &mut Vec<ChoiceArgConfig>,
) -> widget::Choice {
    let value: ImString = tag.expression.into();
    let label = choice.label.map_or_else(|| value.clone(), |label| label.into());

    arguments.reserve(choice.parameters.len());
    for arg in choice.parameters {
        match arg.data.widget {
            Ok(None) => {}
            Ok(Some(config)) => {
                let configuration = to_configuration(config, db, parser);
                let arg = ChoiceArgConfig { choice_index, name: arg.name.into(), configuration };
                arguments.push(arg);
            }
            Err(err) => {
                let msg = "Failed to deserialize nested widget data for argument";
                error!("{:?}", err.context(format!("{msg} '{}'", arg.name)));
            }
        }
    }

    widget::Choice { required_import: tag.required_import.map(Into::into), value, label }
}
