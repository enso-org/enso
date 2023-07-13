//! A module with description how the list of Component Groups is laid out in the Component Panel
//! List.

use crate::prelude::*;

use crate::content::ElementId;
use crate::content::ElementInGroup;
use crate::content::Group;
use crate::content::GroupId;
use crate::content::SectionId;

use ensogl_grid_view::Col;
use ensogl_grid_view::Row;
use std::collections::hash_map::Entry;



// =============
// === Group ===
// =============


// === LaidGroup ===

/// The information of group in the layout.
#[derive(Copy, Clone, Debug)]
pub struct LaidGroup<'a> {
    /// The first row of the group. It may be a header.
    pub first_row:         Row,
    /// First row with group's content. It may differ from the `first_row` if group contains
    /// a header.
    pub first_content_row: Row,
    /// The column where the group is placed.
    pub column:            Col,
    /// The reference to the group information in [`Layout`] structure.
    pub group:             &'a Group,
}

impl<'a> LaidGroup<'a> {
    fn from_map_entry(column: Col, entry: (&Row, &'a Group), header_height: Row) -> Self {
        let (&first_row, group) = entry;
        Self { first_row, first_content_row: first_row + header_height, column, group }
    }

    /// The range of rows where the group spans, _including_ the header.
    pub fn rows(&self) -> Range<Row> {
        self.first_row..(self.first_content_row + self.group.height)
    }

    /// The id of element at given row, or `None` if row is outside the group.
    pub fn element_at_row(&self, row: Row) -> Option<ElementId> {
        let rows = self.rows();
        let element = rows.contains(&row).then(|| {
            if row < self.first_content_row {
                ElementInGroup::Header
            } else {
                // We may unwrap here, as we check above if rows.contains(&row) - if range contains
                // something, it has to have last element.
                ElementInGroup::Entry(rows.last().unwrap() - row)
            }
        });
        element.map(|element| ElementId { group: self.group.id, element })
    }

    /// Check if there are rows assigned to group's header.
    pub fn has_header(&self) -> bool {
        self.first_row < self.first_content_row
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
    groups:        BTreeMap<Row, Group>,
    /// The top occupied row in this group. If there is no group in column, it contains the first
    /// row of "Local scope" section. If the "Local scope" section is also empty, it's an index
    /// of row after last row in grid.
    top_row:       Row,
    section_range: HashMap<SectionId, Range<Row>>,
}

impl Column {
    fn new_empty(local_scope_first_row: Row) -> Self {
        Self {
            groups:        default(),
            top_row:       local_scope_first_row,
            section_range: default(),
        }
    }
}

/// The Component List Panel Layout information.
///
/// This structure allows for organizing the component groups according to received group
/// information about their heights. It provides information about where given group is laid out
/// in Grid View, and what group element is at given location (row and column).
#[derive(Clone, Debug, Default)]
pub struct Layout<const HEADER_HEIGHT: Row = 0> {
    columns:                 Vec<Column>,
    positions:               HashMap<GroupId, (Range<Row>, Col)>,
    local_scope_first_row:   Row,
    local_scope_entry_count: usize,
    row_count:               Row,
}

impl<const HEADER_HEIGHT: Row> Layout<HEADER_HEIGHT> {
    /// Create layout without standard (not the "Local Scope") groups.
    ///
    /// The layout will be completely empty if `local_scope_entry_count` will be 0.
    pub fn new(row_count: Row, column_count: Col, local_scope_entry_count: usize) -> Self {
        let local_scope_rows = local_scope_entry_count.div_ceil(column_count);
        let local_scope_first_row = row_count - local_scope_rows;
        let columns = iter::repeat_with(|| Column::new_empty(local_scope_first_row))
            .take(column_count)
            .collect_vec();
        let positions = default();
        Self { columns, positions, local_scope_first_row, local_scope_entry_count, row_count }
    }

    /// Create the layout with given groups arranged in columns. The groups without entries will be
    /// filtered out.
    pub fn create_from_arranged_groups<const COLUMN_COUNT: usize>(
        groups: [Vec<Group>; COLUMN_COUNT],
        local_scope_entry_count: usize,
    ) -> Self {
        let local_scope_rows = local_scope_entry_count.div_ceil(COLUMN_COUNT);
        let filtered_groups = groups.map(|mut g| {
            g.drain_filter(|g| g.height == 0);
            g
        });
        let col_heights: [usize; COLUMN_COUNT] =
            filtered_groups.each_ref().map(|v| v.iter().map(|g| g.height + HEADER_HEIGHT).sum());
        let groups_rows = col_heights.into_iter().max().unwrap_or_default();
        let all_rows = local_scope_rows + groups_rows;
        let mut this = Self::new(all_rows, COLUMN_COUNT, local_scope_entry_count);
        let with_col_index =
            filtered_groups.into_iter().enumerate().map(Self::column_to_col_group_pairs);
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

    /// Number of rows in layout.
    pub fn row_count(&self) -> Row {
        self.row_count
    }

    /// Number of columns in layout.
    pub fn column_count(&self) -> Col {
        self.columns.len()
    }

    /// Get the information about group other than "Local Scope" occupying given location.
    ///
    /// If there is no group there, or it's "Local Scope" section, `None` is returned.
    pub fn group_at_location(&self, row: Row, column: Col) -> Option<LaidGroup> {
        let groups_in_col = &self.columns.get(column)?.groups;
        let group_before_entry = groups_in_col.range(..=row).last()?;
        let group_before = LaidGroup::from_map_entry(column, group_before_entry, HEADER_HEIGHT);
        group_before.rows().contains(&row).as_some(group_before)
    }

    /// Return the group which is in the same column and entirely above given location.
    pub fn group_above_location(&self, row: Row, column: Col) -> Option<LaidGroup> {
        let groups_in_col = &self.columns.get(column)?.groups;
        let mut groups_before = groups_in_col.range(..row).rev();
        let first_before = LaidGroup::from_map_entry(column, groups_before.next()?, HEADER_HEIGHT);
        if first_before.rows().contains(&row) {
            let second_before =
                LaidGroup::from_map_entry(column, groups_before.next()?, HEADER_HEIGHT);
            Some(second_before)
        } else {
            Some(first_before)
        }
    }

    /// Return the group which is in the same column and entirely below given location. The
    /// Local Scope group is ignored.
    pub fn group_below_location(&self, row: Row, column: Col) -> Option<LaidGroup> {
        let groups_in_col = &self.columns.get(column)?.groups;
        let group_after = groups_in_col.range((row + 1)..).next()?;
        Some(LaidGroup::from_map_entry(column, group_after, HEADER_HEIGHT))
    }

    /// Get the information what element is at given location.
    pub fn element_at_location(&self, row: Row, column: Col) -> Option<ElementId> {
        if row >= self.local_scope_first_row {
            let index = (row - self.local_scope_first_row) * self.columns.len() + column;
            (index < self.local_scope_entry_count).as_some_from(|| ElementId {
                group:   GroupId::local_scope_group(),
                element: ElementInGroup::Entry(index),
            })
        } else {
            let group = self.group_at_location(row, column)?;
            group.element_at_row(row)
        }
    }

    /// Return the location of element in Grid View.
    pub fn location_of_element(&self, element: ElementId) -> Option<(Row, Col)> {
        self.positions
            .get(&element.group)
            .cloned()
            .and_then(|(rows, col)| {
                let header_pos = rows.start;
                match element.element {
                    ElementInGroup::Header => Some((header_pos, col)),
                    ElementInGroup::Entry(index) => Some((rows.last()? - index, col)),
                }
            })
            .or_else(|| {
                (element.group.section == SectionId::LocalScope).and_option_from(|| {
                    match element.element {
                        ElementInGroup::Header => None,
                        ElementInGroup::Entry(index) => {
                            let row = self.local_scope_first_row + index / self.columns.len();
                            let col = index % self.columns.len();
                            Some((row, col))
                        }
                    }
                })
            })
    }

    /// Return the location of given group. Returns [`None`] if there's no such group in the layout
    /// or it's the Local Scope group.
    pub fn location_of_group(&self, group: GroupId) -> Option<(Range<Row>, Col)> {
        self.positions.get(&group).cloned()
    }

    /// Return the minimum row range containing all entries from given section in a column.
    pub fn section_rows_at_column(&self, section: SectionId, column: Col) -> Option<Range<Row>> {
        self.columns
            .get(column)?
            .section_range
            .get(&section)
            .cloned()
            .or_else(|| (section == SectionId::LocalScope).as_some_from(|| self.local_scope_rows()))
    }

    /// Return the range of rows taken by Local Scope group.
    pub fn local_scope_rows(&self) -> Range<Row> {
        self.local_scope_first_row..self.row_count
    }

    /// Add group to the top of given column.
    pub fn push_group(&mut self, column: Col, group: Group) -> Row {
        let group_column = &mut self.columns[column];
        let prev_header_row = group_column.top_row;
        let next_header_row = group_column.top_row - group.height - HEADER_HEIGHT;
        let group_rows = next_header_row..prev_header_row;
        group_column.groups.insert(next_header_row, group);
        group_column.top_row = next_header_row;
        match group_column.section_range.entry(group.id.section) {
            Entry::Occupied(mut entry) => {
                let range = entry.get_mut();
                range.start = range.start.min(group_rows.start);
                range.end = range.end.max(group_rows.end);
            }
            Entry::Vacant(entry) => {
                entry.insert(group_rows.clone());
            }
        }
        self.positions.insert(group.id, (group_rows, column));
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

    use crate::column::*;

    #[test]
    fn checking_element_at_location_and_location_of_element() {
        let group_ids =
            (0..6).map(|index| GroupId { section: SectionId::Popular, index }).collect_vec();
        let group_sizes = vec![2, 1, 3, 3, 2, 1];
        let group_data = group_ids.iter().zip(group_sizes.into_iter());
        let mk_group = |(id, size): (&GroupId, usize)| Group {
            id:               *id,
            height:           size,
            original_height:  size,
            color:            default(),
            best_match_score: default(),
        };
        let groups = group_data.map(mk_group).collect_vec();
        let groups_in_columns =
            [vec![groups[1], groups[4]], vec![groups[0], groups[3]], vec![groups[2], groups[5]]];
        let layout = Layout::<1>::create_from_arranged_groups(groups_in_columns, 8);

        let header_of = |group_idx| ElementId {
            group:   group_ids[group_idx],
            element: ElementInGroup::Header,
        };
        let entry_of = |group_idx, entry_idx| ElementId {
            group:   group_ids[group_idx],
            element: ElementInGroup::Entry(entry_idx),
        };
        let local_scope_entry = |entry_idx| ElementId {
            group:   GroupId::local_scope_group(),
            element: ElementInGroup::Entry(entry_idx),
        };

        // Check all possible locations.
        assert_eq!(layout.element_at_location(0, LEFT), None);
        assert_eq!(layout.element_at_location(0, CENTER), Some(header_of(3)));
        assert_eq!(layout.element_at_location(0, RIGHT), None);
        assert_eq!(layout.element_at_location(1, LEFT), None);
        assert_eq!(layout.element_at_location(1, CENTER), Some(entry_of(3, 2)));
        assert_eq!(layout.element_at_location(1, RIGHT), Some(header_of(5)));
        assert_eq!(layout.element_at_location(2, LEFT), Some(header_of(4)));
        assert_eq!(layout.element_at_location(2, CENTER), Some(entry_of(3, 1)));
        assert_eq!(layout.element_at_location(2, RIGHT), Some(entry_of(5, 0)));
        assert_eq!(layout.element_at_location(5, LEFT), Some(header_of(1)));
        assert_eq!(layout.element_at_location(5, CENTER), Some(entry_of(0, 1)));
        assert_eq!(layout.element_at_location(5, RIGHT), Some(entry_of(2, 1)));
        assert_eq!(layout.element_at_location(6, LEFT), Some(entry_of(1, 0)));
        assert_eq!(layout.element_at_location(6, CENTER), Some(entry_of(0, 0)));
        assert_eq!(layout.element_at_location(6, RIGHT), Some(entry_of(2, 0)));
        assert_eq!(layout.element_at_location(7, LEFT), Some(local_scope_entry(0)));
        assert_eq!(layout.element_at_location(7, CENTER), Some(local_scope_entry(1)));
        assert_eq!(layout.element_at_location(7, RIGHT), Some(local_scope_entry(2)));
        assert_eq!(layout.element_at_location(9, LEFT), Some(local_scope_entry(6)));
        assert_eq!(layout.element_at_location(9, CENTER), Some(local_scope_entry(7)));
        assert_eq!(layout.element_at_location(9, RIGHT), None);

        // Check location of all possible elements
        assert_eq!(layout.location_of_element(header_of(3)), Some((0, CENTER)));
        assert_eq!(layout.location_of_element(entry_of(3, 2)), Some((1, CENTER)));
        assert_eq!(layout.location_of_element(header_of(5)), Some((1, RIGHT)));
        assert_eq!(layout.location_of_element(header_of(4)), Some((2, LEFT)));
        assert_eq!(layout.location_of_element(entry_of(3, 1)), Some((2, CENTER)));
        assert_eq!(layout.location_of_element(entry_of(5, 0)), Some((2, RIGHT)));
        assert_eq!(layout.location_of_element(header_of(1)), Some((5, LEFT)));
        assert_eq!(layout.location_of_element(entry_of(0, 1)), Some((5, CENTER)));
        assert_eq!(layout.location_of_element(entry_of(2, 1)), Some((5, RIGHT)));
        assert_eq!(layout.location_of_element(entry_of(1, 0)), Some((6, LEFT)));
        assert_eq!(layout.location_of_element(entry_of(0, 0)), Some((6, CENTER)));
        assert_eq!(layout.location_of_element(entry_of(2, 0)), Some((6, RIGHT)));
        assert_eq!(layout.location_of_element(local_scope_entry(0)), Some((7, LEFT)));
        assert_eq!(layout.location_of_element(local_scope_entry(1)), Some((7, CENTER)));
        assert_eq!(layout.location_of_element(local_scope_entry(2)), Some((7, RIGHT)));
        assert_eq!(layout.location_of_element(local_scope_entry(6)), Some((9, LEFT)));
        assert_eq!(layout.location_of_element(local_scope_entry(7)), Some((9, CENTER)));
    }

    #[test]
    fn checking_element_at_location_empty_column_and_empty_local_scope() {
        let mut layout = Layout::<1>::new(3, 3, 0);
        let group = Group {
            id:               GroupId { section: SectionId::Popular, index: 0 },
            height:           2,
            original_height:  2,
            color:            default(),
            best_match_score: default(),
        };
        layout.push_group(CENTER, group);

        assert_eq!(layout.element_at_location(2, LEFT), None);
        assert_eq!(
            layout.element_at_location(2, CENTER),
            Some(ElementId {
                group:   GroupId { section: SectionId::Popular, index: 0 },
                element: ElementInGroup::Entry(0),
            })
        );
        assert_eq!(layout.element_at_location(2, RIGHT), None);
        assert_eq!(layout.element_at_location(3, LEFT), None);
        assert_eq!(layout.element_at_location(3, CENTER), None);
        assert_eq!(layout.element_at_location(3, RIGHT), None);
    }

    #[test]
    fn section_ranges_in_layout() {
        let mut layout = Layout::<1>::new(9, 3, 0);
        let popular_group_ids = (0..6).map(|index| GroupId { section: SectionId::Popular, index });
        let submodule_group_ids =
            (6..8).map(|index| GroupId { section: SectionId::Namespace(0), index });
        let group_ids = popular_group_ids.chain(submodule_group_ids);
        let group_sizes = [1, 2, 3].into_iter().cycle();
        let group_columns = [CENTER, LEFT, RIGHT].into_iter().cycle();
        let groups = group_ids.zip(group_sizes).map(|(id, size)| Group {
            id,
            height: size,
            original_height: size,
            color: None,
            best_match_score: default(),
        });
        for (group, col) in groups.zip(group_columns) {
            layout.push_group(col, group);
        }

        assert_eq!(layout.section_rows_at_column(SectionId::Popular, LEFT), Some(3..9));
        assert_eq!(layout.section_rows_at_column(SectionId::Popular, CENTER), Some(5..9));
        assert_eq!(layout.section_rows_at_column(SectionId::Popular, RIGHT), Some(1..9));
        assert_eq!(layout.section_rows_at_column(SectionId::Namespace(0), LEFT), Some(0..3));
        assert_eq!(layout.section_rows_at_column(SectionId::Namespace(0), CENTER), Some(3..5));
        assert_eq!(layout.section_rows_at_column(SectionId::Namespace(0), RIGHT), None);
    }

    #[test]
    fn groups_above_and_below() {
        // Layout constructed:
        // ```
        // Rows
        // -       +-----+
        // 0       |  6  |
        // -       |     |
        // 1       |     |
        // - +-----+-----+-----+
        // 2 |  4  |  3  |  5  |
        // - |     |     |     |
        // 3 |     |     |     |
        // - +-----+     +-----+
        // 4 |  1  |     |  2  |
        // - |     +-----+     |
        // 5 |     |  0  |     |
        // - |     |     |     |
        // 6 |     |     |     |
        // - +-----+-----+-----+
        // ```
        let mut layout = Layout::<1>::new(7, 3, 0);
        let group_ids = (0..7).map(|index| GroupId { section: SectionId::Popular, index });
        let group_sizes = [1, 2].into_iter().cycle();
        let group_columns = [CENTER, LEFT, RIGHT].into_iter().cycle();
        let groups = group_ids.zip(group_sizes).map(|(id, size)| Group {
            id,
            height: size,
            original_height: size,
            color: None,
            best_match_score: default(),
        });
        for (group, col) in groups.zip(group_columns) {
            layout.push_group(col, group);
        }
        fn id_and_rows_from_group(group: Option<LaidGroup<'_>>) -> Option<(usize, Range<Row>)> {
            group.map(|g| (g.group.id.index, g.rows()))
        }

        let group_at =
            |row: Row, col: Col| id_and_rows_from_group(layout.group_at_location(row, col));
        assert_eq!(group_at(0, LEFT), None);
        assert_eq!(group_at(1, LEFT), None);
        assert_eq!(group_at(2, LEFT), Some((4, 2..4)));
        assert_eq!(group_at(3, LEFT), Some((4, 2..4)));
        assert_eq!(group_at(4, LEFT), Some((1, 4..7)));
        assert_eq!(group_at(5, LEFT), Some((1, 4..7)));
        assert_eq!(group_at(6, LEFT), Some((1, 4..7)));
        assert_eq!(group_at(0, CENTER), Some((6, 0..2)));
        assert_eq!(group_at(1, CENTER), Some((6, 0..2)));
        assert_eq!(group_at(2, CENTER), Some((3, 2..5)));
        assert_eq!(group_at(3, CENTER), Some((3, 2..5)));
        assert_eq!(group_at(4, CENTER), Some((3, 2..5)));
        assert_eq!(group_at(5, CENTER), Some((0, 5..7)));
        assert_eq!(group_at(6, CENTER), Some((0, 5..7)));

        let group_above =
            |row: Row, col: Col| id_and_rows_from_group(layout.group_above_location(row, col));
        assert_eq!(group_above(0, LEFT), None);
        assert_eq!(group_above(1, LEFT), None);
        assert_eq!(group_above(2, LEFT), None);
        assert_eq!(group_above(3, LEFT), None);
        assert_eq!(group_above(4, LEFT), Some((4, 2..4)));
        assert_eq!(group_above(5, LEFT), Some((4, 2..4)));
        assert_eq!(group_above(6, LEFT), Some((4, 2..4)));
        assert_eq!(group_above(0, CENTER), None);
        assert_eq!(group_above(1, CENTER), None);
        assert_eq!(group_above(2, CENTER), Some((6, 0..2)));
        assert_eq!(group_above(3, CENTER), Some((6, 0..2)));
        assert_eq!(group_above(4, CENTER), Some((6, 0..2)));
        assert_eq!(group_above(5, CENTER), Some((3, 2..5)));
        assert_eq!(group_above(6, CENTER), Some((3, 2..5)));

        let group_below =
            |row: Row, col: Col| id_and_rows_from_group(layout.group_below_location(row, col));
        assert_eq!(group_below(0, LEFT), Some((4, 2..4)));
        assert_eq!(group_below(1, LEFT), Some((4, 2..4)));
        assert_eq!(group_below(2, LEFT), Some((1, 4..7)));
        assert_eq!(group_below(3, LEFT), Some((1, 4..7)));
        assert_eq!(group_below(4, LEFT), None);
        assert_eq!(group_below(5, LEFT), None);
        assert_eq!(group_below(6, LEFT), None);
        assert_eq!(group_below(0, CENTER), Some((3, 2..5)));
        assert_eq!(group_below(1, CENTER), Some((3, 2..5)));
        assert_eq!(group_below(2, CENTER), Some((0, 5..7)));
        assert_eq!(group_below(3, CENTER), Some((0, 5..7)));
        assert_eq!(group_below(4, CENTER), Some((0, 5..7)));
        assert_eq!(group_below(5, CENTER), None);
        assert_eq!(group_below(6, CENTER), None);
    }
}
