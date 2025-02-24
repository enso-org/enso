## Enso Signatures 1.0
## module Standard.Table.Internal.Multi_Value_Key
- type Ordered_Multi_Value_Key
    - Key columns:(Standard.Base.Data.Vector.Vector Standard.Table.Column.Column) flip_direction:(Standard.Base.Data.Vector.Vector Standard.Base.Data.Boolean.Boolean) row_index:Standard.Base.Data.Numbers.Integer
    - from_row columns:Standard.Base.Any.Any flip_directions:Standard.Base.Any.Any row_index:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Ordered_Multi_Value_Key_Comparator
    - compare x:Standard.Base.Any.Any y:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - hash x:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Unordered_Multi_Value_Key
    - Key hash_code:Standard.Base.Data.Numbers.Integer columns:(Standard.Base.Data.Vector.Vector Standard.Table.Column.Column) row_index:Standard.Base.Data.Numbers.Integer
    - from_row columns:Standard.Base.Any.Any row_index:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - validate_grouping_columns columns:Standard.Base.Any.Any problem_builder:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Unordered_Multi_Value_Key_Comparator
    - compare x:Standard.Base.Any.Any y:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - hash x:Standard.Base.Any.Any -> Standard.Base.Any.Any
- Standard.Base.Data.Ordering.Comparable.from that:Standard.Table.Internal.Multi_Value_Key.Unordered_Multi_Value_Key -> Standard.Base.Data.Ordering.Comparable
- Standard.Base.Data.Ordering.Comparable.from that:Standard.Table.Internal.Multi_Value_Key.Ordered_Multi_Value_Key -> Standard.Base.Data.Ordering.Comparable
