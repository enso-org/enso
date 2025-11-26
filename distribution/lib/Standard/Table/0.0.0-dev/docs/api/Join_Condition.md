## Enso Signatures 1.0
## module Standard.Table.Join_Condition
- type Join_Condition
    - Between left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right_lower:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right_upper:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Custom_SQL sql:Standard.Base.Data.Text.Text= left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Equals left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)=
    - Equals_Ignore_Case left:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= right:(Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= locale:Standard.Base.Data.Locale.Locale=
- Standard.Table.Join_Condition.Join_Condition.from that:Standard.Base.Data.Text.Text -> Standard.Table.Join_Condition.Join_Condition
