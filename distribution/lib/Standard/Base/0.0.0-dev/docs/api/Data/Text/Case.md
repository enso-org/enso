## Enso Signatures 1.0
## module Standard.Base.Data.Text.Case
- type Case
    - Lower
    - Proper
    - Title
    - Upper
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - to_java self -> Standard.Base.Data.Text.Case.Java_Case
    - to_text self -> Standard.Base.Data.Text.Text
