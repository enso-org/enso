## Enso Signatures 1.0
## module Standard.Base.System.Encoding.Encoding_Resolver
- type Unicode_BOM
    - UTF_16_BE
    - UTF_16_LE
    - UTF_8
    - all -> Standard.Base.Any.Any
    - as_bytes self -> Standard.Base.Data.Vector.Vector
    - corresponding_encoding self -> Standard.Base.Data.Text.Encoding.Encoding
- resolve_encoding encoding:Standard.Base.Data.Text.Encoding.Encoding buffered_input_stream:Standard.Base.System.Input_Stream.Input_Stream problem_aggregator:Standard.Base.System.Encoding.Encoding_Resolver.DecodingProblemAggregator continuation:Standard.Base.Any.Any -> Standard.Base.Any.Any
