## Enso Signatures 1.0
## module Standard.Table.Refined_Types.Spatial_Column
- type Spatial_Column
    - Value column:Standard.Base.Any.Any parent_table:Standard.Table.Spatial_Table.Spatial_Table
    - st_area self -> Standard.Base.Any.Any
    - st_centroid self -> Standard.Base.Any.Any
    - st_distance self to_column:(Standard.Table.Column.Column|Standard.Table.Expression.Expression|Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= -> Standard.Base.Any.Any
    - st_extent self -> Standard.Base.Any.Any
    - st_geometry_type self -> Standard.Base.Any.Any
    - st_latitude self value:Standard.Table.Spatial_Table.Lat_Long_Type= -> Standard.Base.Any.Any
    - st_length self to_column:(Standard.Table.Column.Column|Standard.Table.Expression.Expression|Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= -> Standard.Base.Any.Any
    - st_longitude self value:Standard.Table.Spatial_Table.Lat_Long_Type= -> Standard.Base.Any.Any
    - st_perimeter self -> Standard.Base.Any.Any
    - st_to_geojson self -> Standard.Base.Any.Any
    - st_to_wkt self -> Standard.Base.Any.Any
- type Spatial_Input_Column
    - Value column:Standard.Base.Any.Any parent_table:Standard.Table.Spatial_Table.Spatial_Table
    - st_from_geojson self -> Standard.Base.Any.Any
    - st_from_wkt self -> Standard.Base.Any.Any
    - st_point self latitude:(Standard.Table.Column.Column|Standard.Table.Expression.Expression|Standard.Base.Data.Text.Text|Standard.Base.Data.Numbers.Integer)= -> Standard.Base.Any.Any
