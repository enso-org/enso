## Enso Signatures 1.0
## module Standard.AWS.AWS_Region
- type AWS_Region
    - Default
    - Default_With_Profile profile_name:Standard.Base.Data.Text.Text
    - Region id:Standard.Base.Data.Text.Text
    - all_region_ids -> (Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - as_java self -> Standard.AWS.AWS_Region.AWSRegion
    - default_widget display:Standard.Base.Metadata.Display= -> Standard.Base.Metadata.Widget
    - resolve_id self -> Standard.Base.Data.Text.Text
- fallback_region -> Standard.Base.Any.Any
