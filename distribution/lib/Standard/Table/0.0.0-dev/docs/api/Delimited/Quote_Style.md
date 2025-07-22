## Enso Signatures 1.0
## module Standard.Table.Delimited.Quote_Style
- type Quote_Style
    - Always_Quote quote:Standard.Base.Data.Text.Text= quote_escape:Standard.Base.Data.Text.Text=
    - No_Quotes
    - With_Quotes quote:Standard.Base.Data.Text.Text= quote_escape:Standard.Base.Data.Text.Text=
- Standard.Table.Delimited.Quote_Style.Quote_Style.from that:Standard.Base.Data.Json.JS_Object -> Standard.Table.Delimited.Quote_Style.Quote_Style
