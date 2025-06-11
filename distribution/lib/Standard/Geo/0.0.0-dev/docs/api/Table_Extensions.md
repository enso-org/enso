## Enso Signatures 1.0
## module Standard.Geo.Table_Extensions
- type Distance_Units
    - Feet
    - Kilometers
    - Meters
    - Miles
    - Smoots
    - Yards
    - from_meters self meters:Standard.Base.Data.Numbers.Float -> Standard.Base.Data.Numbers.Float
- Standard.Table.Table.Table.geo_distance self lat1:(Standard.Base.Data.Numbers.Float|Standard.Table.Column.Column|Standard.Table.Column_Ref.Column_Ref|Standard.Table.Expression.Expression)= long1:(Standard.Base.Data.Numbers.Float|Standard.Table.Column.Column|Standard.Table.Column_Ref.Column_Ref|Standard.Table.Expression.Expression)= lat2:(Standard.Base.Data.Numbers.Float|Standard.Table.Column.Column|Standard.Table.Column_Ref.Column_Ref|Standard.Table.Expression.Expression)= long2:(Standard.Base.Data.Numbers.Float|Standard.Table.Column.Column|Standard.Table.Column_Ref.Column_Ref|Standard.Table.Expression.Expression)= units:Standard.Geo.Table_Extensions.Distance_Units= as:Standard.Base.Data.Text.Text= -> Standard.Table.Table.Table
