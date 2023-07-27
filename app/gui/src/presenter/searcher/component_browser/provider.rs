//! A module with the providers for searcher view, taking content from the Action List controller.

use crate::prelude::*;

use crate::controller::searcher::component;

use enso_frp as frp;
use enso_suggestion_database::entry::for_each_kind_variant;
use ide_view::component_browser::component_list_panel;
use ide_view::component_browser::component_list_panel::grid as component_grid;



// =======================================
// === ControllerComponentsProviderExt ===
// =======================================

/// An extension for controller's component list adding useful functions for conversions between
/// controller and view structures.
pub trait ControllerComponentsProvider {
    /// Create information about content for the view.
    fn create_grid_content_info(&self) -> component_grid::content::Info;

    /// Create a view's model for given entry.
    fn get_entry_model(
        &self,
        entry_id: component_grid::EntryId,
    ) -> Option<component_grid::EntryModel>;
}


// === Implementation ===

impl ControllerComponentsProvider for component::List {
    fn create_grid_content_info(&self) -> component_grid::content::Info {
        component_list_panel::grid::content::Info {
            entry_count: self.displayed().len(),
            groups:      self
                .groups()
                .iter()
                .enumerate()
                .map(|(id, group)| component_grid::content::Group { id, color: group.color })
                .collect(),
            is_filtered: self.is_filtered(),
        }
    }

    fn get_entry_model(
        &self,
        entry_id: component_grid::EntryId,
    ) -> Option<component_grid::EntryModel> {
        let component = self.displayed().get(entry_id)?;
        Some(component_to_entry_model(component))
    }
}


// === ControllerComponentsProviderExt Helper Functions ===

macro_rules! kind_to_icon {
    ([ $( $variant:ident ),* ] $kind:ident) => {
        {
            use component_grid::entry::icon::Id;
            use model::suggestion_database::entry::Kind;
            match $kind {
                $( Kind::$variant => Id::$variant, )*
            }
        }
    }
}

fn component_to_entry_model(component: &component::Component) -> component_grid::EntryModel {
    let can_be_entered = component.can_be_entered();
    let label = component.matched_label();
    let icon = match &component.suggestion {
        component::Suggestion::FromDatabase { entry, .. } => {
            let kind = entry.kind;
            let icon_name = entry.icon_name.as_ref();
            let icon = icon_name.and_then(|n| n.to_pascal_case().parse().ok());
            icon.unwrap_or_else(|| for_each_kind_variant!(kind_to_icon(kind)))
        }
        component::Suggestion::Virtual { snippet } => snippet.icon,
    };
    let caption = label.text;
    let highlighted = Rc::new(label.highlights);
    let group = component.group_id;
    component_grid::EntryModel { caption, highlighted, icon, can_be_entered, group }
}



// ===========================
// === provider::Component ===
// ===========================

/// An object providing element models for view's [grid](component_list_panel::grid::View) derived
/// from [controller's provider](ontroller::searcher::ComponentsProvider).
///
/// During construction an FRP network is set up to answer the `model_for_header_needed` and
/// `model_for_entry_needed` events received from the view. These connections are removed once this
/// structure is dropped.
#[derive(Debug)]
pub struct Component {
    _network: frp::Network,
}

impl Component {
    /// Initialize the [`Component`] provider, setting up proper connections to feed
    /// [grid](component_list_panel::grid::View) with entries and headers models.
    pub fn provide_new_list(
        list: &Rc<component::List>,
        grid: &component_list_panel::grid::View,
    ) -> Self {
        let network = frp::Network::new("presenter::searcher::provider::Component");

        // We capture weak reference to list, because searcher controllers sometimes calls
        // [`Rc::make_mut`] on it and we want to keep it from cloning entire list.
        let weak_list = Rc::downgrade(list);

        frp::extend! { network
            // Beware! We cannot connect the expression `grid.model_for...` to grid inputs, because
            // the connections alone does not belong to network - thus when provider's network would
            // be dropped, the connections would remain. Therefore we create intermediate nodes
            // `entry_model` and more if needed.
            entry_model <- grid.model_for_entry_needed.filter_map(f!([weak_list](&id)
                // If the weak was disassociated, then we should soon receive a new component list,
                // or the Component Browser is about to be hidden.
                weak_list.upgrade().and_then(|list| Some((id, list.get_entry_model(id)?)))
            ));
            grid.model_for_entry <+ entry_model;
        }
        let content = list.create_grid_content_info();
        grid.reset(content);
        Self { _network: network }
    }
}
