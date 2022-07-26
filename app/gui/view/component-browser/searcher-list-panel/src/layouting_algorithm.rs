use ensogl_core::prelude::*;

fn arrange(groups: Vec<(usize, usize)>) -> Vec<Vec<usize>> {
    let mut columns = vec![vec![]; 3];
    const LEFT: usize = 0;
    const CENTER: usize = 1;
    const RIGHT: usize = 2;

    fn height(column: &[(usize, usize)]) -> usize {
        column.into_iter().map(|(_id, size)| size).sum()
    }

    fn fill_till_max(
        column: &mut Vec<(usize, usize)>,
        max: usize,
        mut groups: impl Iterator<Item = (usize, usize)>,
    ) {
        while height(column) < max {
            if let Some(group) = groups.next() {
                column.push(group);
            } else {
                break;
            }
        }
    }

    let mut iter = groups.into_iter().peekable();

    if let Some(group) = iter.next() {
        columns[CENTER].push(group);
    }
    let mut max = height(&columns[CENTER]);
    while iter.peek().is_some() {
        if let Some(group) = iter.next() {
            columns[LEFT].push(group);
        }
        if let Some(group) = iter.next() {
            columns[RIGHT].push(group);
        }
        if let Some(group) = iter.next() {
            columns[CENTER].push(group);
        }

        fill_till_max(&mut columns[LEFT], max, &mut iter);
        fill_till_max(&mut columns[RIGHT], max, &mut iter);
        max = height(&columns[CENTER]);
    }

    columns.into_iter().map(|column| column.into_iter().map(|(id, _)| id).collect()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_case_from_design_doc() {
        let groups: Vec<(usize, usize)> =
            vec![(1, 4), (2, 4), (3, 3), (4, 3), (5, 2), (6, 3), (7, 2)];
        let arranged = arrange(groups);
        let expected: &[Vec<usize>] = &[vec![2, 6], vec![1, 4], vec![3, 5, 7]];
        assert_eq!(&arranged, expected);
    }

    #[test]
    fn test_case_from_ac() {
        let groups: Vec<(usize, usize)> =
            vec![(1, 3), (2, 1), (3, 3), (4, 4), (5, 1), (6, 1), (7, 4), (8, 1), (9, 2), (10, 1)];
        let arranged = arrange(groups);
        let expected: &[Vec<usize>] = &[vec![2, 5, 6, 7], vec![1, 4, 9], vec![3, 8, 10]];
        assert_eq!(&arranged, expected);
    }
}
