//! A module with functions used to support working with text representation of the language.

use ast::IdMap;
use data::text::Size;
use data::text::Span;



// ================
// === Text API ===
// ================

/// Update IdMap to reflect the recent code change.
///
// TODO[ao]: It's a really minimalistic algorithm, just extends/shrinks span without caring about
// code structure.
pub fn apply_code_change_to_id_map(id_map:&mut IdMap, removed:&Span, inserted:&str) {
    let vector = &mut id_map.vec;
    let inserted_len  = Size::new(inserted.chars().count());
    vector.drain_filter(|(span,_)| removed.contains_span(&span));
    for (span, _) in vector {
        if span.index >= removed.end() {
            span.index += inserted_len;
            span.index -= removed.size;
        } else if span.index >= removed.index {
            let removed_chars = removed.end() - span.index;
            span.index = removed.index + inserted_len;
            span.size -= removed_chars;
        } else if span.end() >= removed.index {
            let removed_chars = (span.end() - removed.index).min(removed.size);
            span.size -= removed_chars;
            span.size += inserted_len;
        }
    }
}



#[cfg(test)]
mod test {
    use super::*;

    use ast::IdMap;
    use data::text::Index;
    use data::text::Size;
    use data::text::Span;
    use uuid::Uuid;

    #[test]
    fn applying_code_changes_to_id_map() {
        let uuid1 = Uuid::new_v4();
        let uuid2 = Uuid::new_v4();
        let uuid3 = Uuid::new_v4();
        let uuid4 = Uuid::new_v4();
        let uuid5 = Uuid::new_v4();
        let mut id_map = IdMap::new(vec!
        [ (Span::new(Index::new(0) , Size::new(3)), uuid1)
          , (Span::new(Index::new(5) , Size::new(2)), uuid2)
          , (Span::new(Index::new(7) , Size::new(2)), uuid3)
          , (Span::new(Index::new(9) , Size::new(2)), uuid4)
          , (Span::new(Index::new(13), Size::new(2)), uuid5)
        ]);

        apply_code_change_to_id_map(&mut id_map, &Span::new(Index::new(6),Size::new(4)), "a test");
        let expected = IdMap::new(vec!
        [ (Span::new(Index::new(0) , Size::new(3)), uuid1)
          , (Span::new(Index::new(5) , Size::new(7)), uuid2)
          , (Span::new(Index::new(12), Size::new(1)), uuid4)
          , (Span::new(Index::new(15), Size::new(2)), uuid5)
        ]);
        assert_eq!(expected, id_map);

        apply_code_change_to_id_map(&mut id_map, &Span::new(Index::new(12), Size::new(2)), "x");
        let expected = IdMap::new(vec!
        [ (Span::new(Index::new(0) , Size::new(3)), uuid1)
          , (Span::new(Index::new(5) , Size::new(8)), uuid2)
          , (Span::new(Index::new(14), Size::new(2)), uuid5)
        ]);
        assert_eq!(expected, id_map);
    }
}
