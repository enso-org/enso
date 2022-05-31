use crate::controller::searcher::component::Component;
use crate::model::execution_context;
use crate::model::suggestion_database;
use crate::prelude::*;

#[derive(Clone, Debug)]
pub struct Data {
    pub name:    ImString,
    // FIXME[mc]: add color
    pub entries: RefCell<Vec<Component>>,
    pub visible: Cell<bool>,
}

impl Data {
    fn new_empty_visible(name: impl Into<ImString>) -> Self {
        Data { name: name.into(), entries: default(), visible: Cell::new(true) }
    }
}

#[derive(Clone, CloneRef, Debug, AsRef, Deref)]
pub struct Group {
    data: Rc<Data>,
}

impl Group {
    pub fn from_entry(entry: &suggestion_database::Entry) -> Self {
        let name: String = if entry.module.is_top_module() {
            (&entry.module).into()
        } else {
            entry.module.name().into()
        };
        Self { data: Rc::new(Data::new_empty_visible(name)) }
    }

    pub fn from_execution_context_component_group(
        group: execution_context::ComponentGroup,
        suggestion_db: &model::SuggestionDatabase,
    ) -> Self {
        let components = group.components.iter().filter_map(|qualified_name| {
            let (id, suggestion) = suggestion_db.lookup_by_qualified_name(qualified_name)?;
            Some(Component { suggestion_id: Immutable(id), suggestion, match_info: default() })
        });
        let entries = RefCell::new(components.collect());
        let data = Data { name: group.name, visible: Cell::new(true), entries };
        Self { data: Rc::new(data) }
    }

    pub fn check_visibility(&self) {
        let entries = self.entries.borrow();
        let filtered_out = entries.iter().filter(|c| c.is_filtered_out());
        let visible = filtered_out.count() < entries.len();
        self.visible.set(visible);
    }
}

#[derive(Clone, CloneRef, Debug, Default, AsRef, Deref)]
pub struct List {
    groups: Rc<Vec<Group>>,
}

impl List {
    pub fn new_unsorted(groups: Vec<Group>) -> Self {
        Self { groups: Rc::new(groups) }
    }

    fn iter_visible(&self) -> impl Iterator<Item = &Group> {
        self.groups.iter().filter(|g| g.visible.get())
    }
}

#[derive(Clone, Debug, Default, AsRef, Deref, AsMut, DerefMut)]
pub struct ListBuilder {
    pub groups: Vec<Group>,
}

impl ListBuilder {
    pub fn build(mut self) -> List {
        // The `sort_unstable_by_key` method is not suitable here, because the closure it takes
        // cannot return reference, and we don't want to copy strings here.
        self.groups.sort_unstable_by(|a, b| a.name.cmp(&b.name));
        List { groups: Rc::new(self.groups) }
    }
}
