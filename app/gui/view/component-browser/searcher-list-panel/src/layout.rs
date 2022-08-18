use crate::prelude::*;

use ensogl_grid_view::Col;
use ensogl_grid_view::Row;
use ide_view_component_group::set::GroupId;
use ide_view_component_group::set::SectionId;

pub const HEADER_HEIGHT: usize = 1;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum GroupElement {
    Header,
    Entry(usize),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Group {
    pub id:              GroupId,
    pub height:          usize,
    pub original_height: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct LaidGroup<'a> {
    header_row: Row,
    column:     Col,
    group:      &'a Group,
}

impl<'a> LaidGroup<'a> {
    fn rows(&self) -> Range<Row> {
        self.header_row..(self.header_row + self.group.height + HEADER_HEIGHT)
    }

    fn element_at_row(&self, row: Row) -> Option<ElementId> {
        let element = self.rows().contains(&row).as_some_from(|| {
            if row < self.header_row + HEADER_HEIGHT {
                GroupElement::Header
            } else {
                GroupElement::Entry(row - self.header_row - HEADER_HEIGHT)
            }
        });
        element.map(|element| ElementId { group: self.group.id, element })
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ElementId {
    group:   GroupId,
    element: GroupElement,
}

#[derive(Clone, Debug)]
struct Column {
    groups:  BTreeMap<Row, Group>,
    top_row: Row,
}

#[derive(Clone, Debug)]
pub struct Layout {
    columns:                   Vec<Column>,
    positions:                 HashMap<GroupId, (Row, Col)>,
    local_scope_section_start: Row,
    local_scope_entry_count:   usize,
    row_count:                 Row,
}

impl Layout {
    pub fn new(row_count: Row, column_count: Col, local_scope_entry_count: usize) -> Self {
        let local_scope_rows = local_scope_entry_count.div_ceil(column_count);
        let local_scope_section_start = row_count - local_scope_rows;
        let columns =
            iter::repeat_with(|| Column { groups: default(), top_row: local_scope_section_start })
                .take(column_count)
                .collect_vec();
        let positions = default();
        Self { columns, positions, local_scope_section_start, local_scope_entry_count, row_count }
    }

    pub fn

    pub fn group_at_location(&self, row: Row, column: Col) -> Option<LaidGroup> {
        let groups_in_col = &self.columns.get(column)?.groups;
        let (group_before_row, group_before) = groups_in_col.range(..=row).last()?;
        let group_end = group_before_row + group_before.height + HEADER_HEIGHT;
        (group_end > row).as_some_from(|| LaidGroup {
            header_row: *group_before_row,
            column,
            group: group_before,
        })
    }

    pub fn element_at_location(&self, row: Row, column: Col) -> Option<ElementId> {
        if row >= self.local_scope_section_start {
            let index = (row - self.local_scope_section_start) * self.columns.len() + column;
            (index < self.local_scope_entry_count).as_some_from(|| ElementId {
                group:   GroupId::local_scope_group(),
                element: GroupElement::Entry(index),
            })
        } else {
            println!("element_at_location: {row}, {column}");
            let group = self.group_at_location(row, column)?;
            println!("got the group: {group:?}");
            group.element_at_row(row)
        }
    }

    pub fn location_of_element(&self, element: ElementId) -> Option<(Row, Col)> {
        if element.group.section == SectionId::LocalScope {
            match element.element {
                GroupElement::Header => None,
                GroupElement::Entry(index) => {
                    let row = self.local_scope_section_start + index / self.columns.len();
                    let col = index % self.columns.len();
                    Some((row, col))
                }
            }
        } else {
            let &(header_pos, col) = self.positions.get(&element.group)?;
            match element.element {
                GroupElement::Header => Some((header_pos, col)),
                GroupElement::Entry(index) => Some((header_pos + HEADER_HEIGHT + index, col)),
            }
        }
    }

    pub fn push_group(&mut self, column: Col, group: Group) -> Row {
        let group_column = &mut self.columns[column];
        let next_header_row = group_column.top_row - group.height - HEADER_HEIGHT;
        group_column.groups.insert(next_header_row, group);
        group_column.top_row = next_header_row;
        self.positions.insert(group.id, (next_header_row, column));
        next_header_row
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use ide_view_component_group::set::SectionId;

    const LEFT: usize = 0;
    const CENTER: usize = 1;
    const RIGHT: usize = 2;

    #[test]
    fn group_layout() {
        let mut layout = Layout::new(10, 3, 8);
        let group_ids =
            (0..6).map(|index| GroupId { section: SectionId::Favorites, index }).collect_vec();
        let group_sizes = vec![2, 1, 3, 3, 2, 1];
        let group_columns = vec![CENTER, LEFT, RIGHT, CENTER, LEFT, RIGHT];
        for ((id, size), column) in
            group_ids.iter().zip(group_sizes.into_iter()).zip(group_columns.into_iter())
        {
            layout.push_group(column, Group {
                id:              *id,
                height:          size,
                original_height: size,
            });
        }

        let header_of =
            |group_idx| ElementId { group: group_ids[group_idx], element: GroupElement::Header };
        let entry_of = |group_idx, entry_idx| ElementId {
            group:   group_ids[group_idx],
            element: GroupElement::Entry(entry_idx),
        };
        let local_scope_entry = |entry_idx| ElementId {
            group:   GroupId::local_scope_group(),
            element: GroupElement::Entry(entry_idx),
        };

        assert_eq!(layout.element_at_location(0, LEFT), None);
        assert_eq!(layout.element_at_location(0, CENTER), Some(header_of(3)));
        assert_eq!(layout.element_at_location(0, RIGHT), None);
        assert_eq!(layout.element_at_location(1, LEFT), None);
        assert_eq!(layout.element_at_location(1, CENTER), Some(entry_of(3, 0)));
        assert_eq!(layout.element_at_location(1, RIGHT), Some(header_of(5)));
        assert_eq!(layout.element_at_location(2, LEFT), Some(header_of(4)));
        assert_eq!(layout.element_at_location(2, CENTER), Some(entry_of(3, 1)));
        assert_eq!(layout.element_at_location(2, RIGHT), Some(entry_of(5, 0)));
        assert_eq!(layout.element_at_location(5, LEFT), Some(header_of(1)));
        assert_eq!(layout.element_at_location(5, CENTER), Some(entry_of(0, 0)));
        assert_eq!(layout.element_at_location(5, RIGHT), Some(entry_of(2, 1)));
        assert_eq!(layout.element_at_location(6, LEFT), Some(entry_of(1, 0)));
        assert_eq!(layout.element_at_location(6, CENTER), Some(entry_of(0, 1)));
        assert_eq!(layout.element_at_location(6, RIGHT), Some(entry_of(2, 2)));
        assert_eq!(layout.element_at_location(7, LEFT), Some(local_scope_entry(0)));
        assert_eq!(layout.element_at_location(7, CENTER), Some(local_scope_entry(1)));
        assert_eq!(layout.element_at_location(7, RIGHT), Some(local_scope_entry(2)));
        assert_eq!(layout.element_at_location(9, LEFT), Some(local_scope_entry(6)));
        assert_eq!(layout.element_at_location(9, CENTER), Some(local_scope_entry(7)));
        assert_eq!(layout.element_at_location(9, RIGHT), None);

        assert_eq!(layout.location_of_element(header_of(3)), Some((0, CENTER)));
        assert_eq!(layout.location_of_element(entry_of(3, 0)), Some((1, CENTER)));
        assert_eq!(layout.location_of_element(header_of(5)), Some((1, RIGHT)));
        assert_eq!(layout.location_of_element(header_of(4)), Some((2, LEFT)));
        assert_eq!(layout.location_of_element(entry_of(3, 1)), Some((2, CENTER)));
        assert_eq!(layout.location_of_element(entry_of(5, 0)), Some((2, RIGHT)));
        assert_eq!(layout.location_of_element(header_of(1)), Some((5, LEFT)));
        assert_eq!(layout.location_of_element(entry_of(0, 0)), Some((5, CENTER)));
        assert_eq!(layout.location_of_element(entry_of(2, 1)), Some((5, RIGHT)));
        assert_eq!(layout.location_of_element(entry_of(1, 0)), Some((6, LEFT)));
        assert_eq!(layout.location_of_element(entry_of(0, 1)), Some((6, CENTER)));
        assert_eq!(layout.location_of_element(entry_of(2, 2)), Some((6, RIGHT)));
        assert_eq!(layout.location_of_element(local_scope_entry(0)), Some((7, LEFT)));
        assert_eq!(layout.location_of_element(local_scope_entry(1)), Some((7, CENTER)));
        assert_eq!(layout.location_of_element(local_scope_entry(2)), Some((7, RIGHT)));
        assert_eq!(layout.location_of_element(local_scope_entry(6)), Some((9, LEFT)));
        assert_eq!(layout.location_of_element(local_scope_entry(7)), Some((9, CENTER)));
    }

    #[test]
    fn group_layouts_with_empty_column_and_local_scope() {
        let mut layout = Layout::new(3, 3, 0);
        let group = Group {
            id:              GroupId { section: SectionId::Favorites, index: 0 },
            height:          2,
            original_height: 2,
        };
        layout.push_group(CENTER, group);

        assert_eq!(layout.element_at_location(2, LEFT), None);
        assert_eq!(
            layout.element_at_location(2, CENTER),
            Some(ElementId {
                group:   GroupId { section: SectionId::Favorites, index: 0 },
                element: GroupElement::Entry(1),
            })
        );
        assert_eq!(layout.element_at_location(2, RIGHT), None);
        assert_eq!(layout.element_at_location(3, LEFT), None);
        assert_eq!(layout.element_at_location(3, CENTER), None);
        assert_eq!(layout.element_at_location(3, RIGHT), None);
    }
}
