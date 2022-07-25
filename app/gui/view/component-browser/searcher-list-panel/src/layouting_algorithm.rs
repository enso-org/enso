use ensogl_core::prelude::*;

fn arrange(groups: Vec<(usize, usize)>) -> Vec<Vec<usize>> {
    let mut columns = vec![vec![]; 3];
    const LEFT: usize = 0;
    const CENTER: usize = 1;
    const RIGHT: usize = 2;

    fn height(column: &[(usize, usize)]) -> usize {
        column.into_iter().map(|(_id, size)| size).sum()
    }

    // 1. The first not yet chosen place in the middle column is chosen and is used to draw the next
    // group. The position of its highest entry is remembered as MAX. Then process to point 2.
    columns[CENTER].push(groups[0]);
    let mut max = height(&columns[CENTER]);
    let mut i = 1;
    while i < groups.len() {
        // 2.If the first not yet chosen place in the left column starts below MAX, it is chosen and
        // is used to draw the next group. Process to point 3.
        if height(&columns[LEFT]) < max {
            columns[LEFT].push(groups[i]);
        }
        i += 1;
        if i >= groups.len() {
            break;
        }
        // 3. If the first not yet chosen place in the right column starts below MAX, it is chosen
        // and is used to draw the next group. Process to point 4.
        if height(&columns[RIGHT]) < max {
            columns[RIGHT].push(groups[i]);
        }
        i += 1;
        if i >= groups.len() {
            break;
        }
        // 4.The first not yet chosen place in the middle column is chosen and is used to draw the
        // next group. The position of its highest entry is remembered as NEXT_MAX. Then
        // process to point 5.
        columns[CENTER].push(groups[i]);
        let next_max = height(&columns[CENTER]);
        i += 1;
        if i >= groups.len() {
            break;
        }
        // 5. If the first not yet chosen place in the left column starts below MAX, it is chosen,
        // is used to draw the next group, and then algorithm loops back to point 5.
        while height(&columns[LEFT]) < max && i < groups.len() {
            columns[LEFT].push(groups[i]);
            i += 1;
        }
        // 6.If the first not yet chosen place in the right column starts below MAX, it is chosen,
        // is used to draw the next group, and then algorithm loops back to point 6.
        while height(&columns[RIGHT]) < max && i < groups.len() {
            columns[RIGHT].push(groups[i]);
            i += 1;
        }
        // 7. MAX = NEXT_MAX. Process to point 2.
        max = next_max;
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
