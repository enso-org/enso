# Standard.Geo

Geographic types and utilities: latitude/longitude points, great-circle
distances, GeoJSON to `Table` conversion, distance unit selection.

## Main entry points

- `Geo.point latitude longitude elevation=0` — build a JSON point object.
- `Geo.distance lat1 lon1 lat2 lon2 unit=..Kilometers` — great-circle distance
  between two coordinates.
- `Geo.geo_json_to_table json fields` — flatten a GeoJSON `FeatureCollection`
  into rows.
- `..Meters`, `..Kilometers`, `..Feet`, `..Miles` — `Distance_Units` autoscope
  variants.
- `table.geo_distance lat1_col lon1_col lat2_col lon2_col unit out_col` — add a
  per-row distance column to a `Table`.

## Common usage

```
point = Geo.point 51.509865 -0.118092

d_km = Geo.distance 51.510357 -0.116773 38.889931 -77.009003 ..Kilometers
d_mi = Geo.distance 51.510357 -0.116773 38.889931 -77.009003 ..Miles

result_table = table.geo_distance (..Name "lat1") (..Name "lng1") (..Name "lat2") (..Name "lng2") ..Kilometers "distance_km"
```

## Layout

- `src/Geo.enso` — point creation, distance, GeoJSON to table.
- `src/Table_Extensions.enso` — `Table.geo_distance` method, `Distance_Units`
  enum.
- `src/Helpers.enso` — private GeoJSON parsing utilities (do not import).

## Things to avoid in generated code

- Coordinates outside WGS84 bounds (latitude `[-90, 90]`, longitude
  `[-180, 180]`) — results are unspecified.
- Passing column references to `geo_distance` as bare strings — use
  `..Name "col_name"` (autoscope to `Column_Selector`) so column resolution
  matches the rest of the `Table` API.
- Trusting GeoJSON without schema validation; malformed structures produce
  errors at parse time.

## Where to read more

- `src/Geo.enso` — distance and GeoJSON helpers with doc blocks.
- `src/Table_Extensions.enso` — column-based `geo_distance`.
- `test/Geo_Tests/src/Geo_Spec.enso` — every public function exercised.
