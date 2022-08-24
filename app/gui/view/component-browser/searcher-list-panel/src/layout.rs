//! A module with description how the list of Component Groups is laid out in the Component Panel
//! List.

use crate::prelude::*;

use ensogl_grid_view::Col;
use ensogl_grid_view::Row;
use ide_view_component_group::set::GroupId;
use ide_view_component_group::set::SectionId;



// =================
// === Constants ===
// =================

/// Height of the header of the component group. This value is added to the group's number of
/// entries to get the total height.
pub const HEADER_HEIGHT_IN_ROWS: usize = 1;



// =================
// === ElementId ===
// =================

/// An identifier of element inside a concrete group: entry (by entry index) or header.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum GroupElement {
    /// A group's header.
    Header,
    /// A group's normal entry with index.
    Entry(usize),
}

/// An identifier of some group's element in Component Browser.
#[allow(missing_docs)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ElementId {
    pub group:   GroupId,
    pub element: GroupElement,
}



// =============
// === Group ===
// =============

/// A information about group needed to compute the Component Panel List layout.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Group {
    /// The group identifier.
    pub id:              GroupId,
    /// Height of group in rows, not counting the header nor filtered-out entries.
    pub height:          usize,
    /// Height of group in rows if no entry is filtered out, not counting the header.
    pub original_height: usize,
}


// === LaidGroup ===

/// The information of group in the layout.
#[derive(Copy, Clone, Debug)]
pub struct LaidGroup<'a> {
    /// The row where header is placed.
    pub header_row: Row,
    /// The column where the group is placed.
    pub column:     Col,
    /// The reference to the group information in [`Layout`] structure.
    pub group:      &'a Group,
}

impl<'a> LaidGroup<'a> {
    /// The range of rows where the group spans, _including_ the header.
    pub fn rows(&self) -> Range<Row> {
        self.header_row..(self.header_row + self.group.height + HEADER_HEIGHT_IN_ROWS)
    }

    /// The id of element at given row, or `None` if row is outside the group.
    pub fn element_at_row(&self, row: Row) -> Option<ElementId> {
        let element = self.rows().contains(&row).as_some_from(|| {
            if row < self.header_row + HEADER_HEIGHT_IN_ROWS {
                GroupElement::Header
            } else {
                GroupElement::Entry(row - self.header_row - HEADER_HEIGHT_IN_ROWS)
            }
        });
        element.map(|element| ElementId { group: self.group.id, element })
    }
}



// ==============
// === Layout ===
// ==============

/// The info about single column in [`Layout`].
#[derive(Clone, Debug)]
struct Column {
    /// A mapping between group position and the [`Group`] info. The keys in map are the rows with
    /// given group's header.
    groups:  BTreeMap<Row, Group>,
    /// The top occupied row in this group. If there is no group in column, it contains the first
    /// row of "Local scope" section. If the "Local scope" section is also empty, it's an index
    /// of row after last row in grid.
    top_row: Row,
}

/// The Component List Panel Layout information.
///
/// This structure allows for organizing the component groups according to received group
/// information about their heights. It provides information about where given group is laid out
/// in Grid View, and what group element is at given location (row and column).
#[derive(Clone, Debug)]
pub struct Layout {
    columns:                   Vec<Column>,
    positions:                 HashMap<GroupId, (Row, Col)>,
    local_scope_section_start: Row,
    local_scope_entry_count:   usize,
    row_count:                 Row,
}

impl Layout {
    /// Create layout without standard (not the "Local Scope") groups.
    ///
    /// The layout will be completely empty if `local_scope_entry_count` will be 0.
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

    /// Create the layout with given groups arranged in columns.
    pub fn create_from_arranged_groups<const COLUMN_COUNT: usize>(
        groups: [Vec<Group>; COLUMN_COUNT],
        local_scope_entry_count: usize,
    ) -> Self {
        let local_scope_rows = local_scope_entry_count.div_ceil(COLUMN_COUNT);
        let col_heights: [usize; COLUMN_COUNT] = groups
            .each_ref()
            .map(|v| v.iter().map(|g| g.original_height + HEADER_HEIGHT_IN_ROWS).sum());
        let groups_rows = col_heights.into_iter().max().unwrap_or_default();
        let all_rows = local_scope_rows + groups_rows;
        let mut this = Self::new(all_rows, COLUMN_COUNT, local_scope_entry_count);
        let with_col_index = groups.into_iter().enumerate().map(Self::column_to_col_group_pairs);
        for (column, group) in with_col_index.flatten() {
            this.push_group(column, group);
        }
        this
    }

    fn column_to_col_group_pairs(
        (index, column): (Col, Vec<Group>),
    ) -> impl Iterator<Item = (Col, Group)> {
        column.into_iter().map(move |group| (index, group))
    }

    /// Get the information about group other than "Local Scope" occupying given location.
    ///
    /// If there is no group there, or it's "Local Scope" section, `None` is returned.
    pub fn group_at_location(&self, row: Row, column: Col) -> Option<LaidGroup> {
        let groups_in_col = &self.columns.get(column)?.groups;
        let (group_before_row, group_before) = groups_in_col.range(..=row).last()?;
        let group_end = group_before_row + group_before.height + HEADER_HEIGHT_IN_ROWS;
        (group_end > row).as_some_from(|| LaidGroup {
            header_row: *group_before_row,
            column,
            group: group_before,
        })
    }

    /// Get the information what element is at given location.
    pub fn element_at_location(&self, row: Row, column: Col) -> Option<ElementId> {
        if row >= self.local_scope_section_start {
            let index = (row - self.local_scope_section_start) * self.columns.len() + column;
            (index < self.local_scope_entry_count).as_some_from(|| ElementId {
                group:   GroupId::local_scope_group(),
                element: GroupElement::Entry(index),
            })
        } else {
            let group = self.group_at_location(row, column)?;
            group.element_at_row(row)
        }
    }

    /// Return the location of element in Grid View.
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
                GroupElement::Entry(index) =>
                    Some((header_pos + HEADER_HEIGHT_IN_ROWS + index, col)),
            }
        }
    }

    /// Add group to the top of given column.
    pub fn push_group(&mut self, column: Col, group: Group) -> Row {
        let group_column = &mut self.columns[column];
        let next_header_row = group_column.top_row - group.height - HEADER_HEIGHT_IN_ROWS;
        group_column.groups.insert(next_header_row, group);
        group_column.top_row = next_header_row;
        self.positions.insert(group.id, (next_header_row, column));
        next_header_row
    }

    /// The topmost row containing any group.
    pub fn top_row(&self) -> Row {
        self.columns.iter().map(|column| column.top_row).min().unwrap_or(self.row_count)
    }

    /// The topmost row containing any group in given column.
    pub fn column_top_row(&self, column: Col) -> Row {
        self.columns.get(column).map_or(self.row_count, |c| c.top_row)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use ide_view_component_group::set::SectionId;

    const LEFT: usize = 0;
    const CENTER: usize = 1;
    const RIGHT: usize = 2;

    #[test]
    fn group_layout() {
        let group_ids =
            (0..6).map(|index| GroupId { section: SectionId::Favorites, index }).collect_vec();
        let group_sizes = vec![2, 1, 3, 3, 2, 1];
        let group_data = group_ids.iter().zip(group_sizes.into_iter());
        let mk_group = |(id, size): (&GroupId, usize)| Group {
            id:              *id,
            height:          size,
            original_height: size,
        };
        let groups = group_data.map(mk_group).collect_vec();
        let groups_in_columns =
            [vec![groups[1], groups[4]], vec![groups[0], groups[3]], vec![groups[2], groups[5]]];
        let layout = Layout::create_from_arranged_groups(groups_in_columns, 8);

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
