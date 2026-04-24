## Enso Signatures 1.0
## module Standard.Geo.Geo
- type Geo
    - distance lat1:Standard.Base.Data.Numbers.Float= long1:Standard.Base.Data.Numbers.Float= lat2:Standard.Base.Data.Numbers.Float= long2:Standard.Base.Data.Numbers.Float= units:Standard.Geo.Table_Extensions.Distance_Units= -> Standard.Base.Data.Numbers.Float
    - geo_json_to_table geo_json:Standard.Base.Any.Any fields:Standard.Base.Any.Any= -> Standard.Base.Any.Any
    - point latitude:Standard.Base.Data.Numbers.Float longitude:Standard.Base.Data.Numbers.Float elevation:Standard.Base.Data.Numbers.Float= -> Standard.Table.Table.Table
