use crate::controller::searcher::component::Component;
use crate::model::suggestion_database;
use crate::prelude::*;

#[derive(Clone, Debug)]
struct Data {
    pub name:    ImString,
    pub entries: RefCell<Vec<Component>>,
    pub visible: bool,
}

impl Data {
    fn new_empty_visible(name: impl Into<ImString>) -> Self {
        Data { name: name.into(), entries: default(), visible: true }
    }
}

#[derive(Clone, CloneRef, Debug, AsRef, Deref)]
pub struct Group {
    data: Rc<Data>,
}

impl Group {
    pub fn from_entry(entry: &suggestion_database::Entry) -> Self {
        let name: String = if entry.module.is_top_module() {
            entry.module.into()
        } else {
            entry.module.name().into()
        };
        Self { data: Rc::new(Data::new_empty_visible(name)) }
    }
}

#[derive(Clone, CloneRef, Debug, Default, AsRef, Deref)]
pub struct List {
    groups: Rc<Vec<Group>>,
}

#[derive(Clone, Debug, Default, AsRef, Deref, AsMut, DerefMut)]
pub struct ListBuilder {
    pub groups: Vec<Group>,
}

impl ListBuilder {
    fn build(mut self) -> List {
        self.groups.sort_unstable_by_key(|group| &group.name);
        List { groups: Rc::new(self.groups) }
    }
}
