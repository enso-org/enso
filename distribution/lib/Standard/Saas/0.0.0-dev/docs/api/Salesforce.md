## Enso Signatures 1.0
## module Standard.Saas.Salesforce
- type Salesforce
    - get_instance_url self -> Standard.Base.Any.Any
    - get_raw_report self name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_report self name:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - get_user self -> Standard.Base.Any.Any
    - initialize credentials:Standard.Base.Any.Any -> Standard.Base.Any.Any
    - raw_table_data_to_table raw_column_data:Standard.Base.Any.Any raw_table_data:Standard.Base.Any.Any -> Standard.Table.Table.Table
    - to_js_object self -> Standard.Base.Any.Any
