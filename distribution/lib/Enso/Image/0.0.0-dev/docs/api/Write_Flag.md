## Enso Signatures 1.0
## module Standard.Image.Write_Flag
- type Write_Flag
    - JPEG_Chroma_Quality val:Standard.Base.Any.Any=
    - JPEG_Luma_Quality val:Standard.Base.Data.Numbers.Integer=
    - JPEG_Optimize
    - JPEG_Progressive
    - JPEG_Quality val:Standard.Base.Data.Numbers.Integer=
    - PNG_Compression val:Standard.Base.Data.Numbers.Integer=
    - WEBP_Quality val:Standard.Base.Any.Any=
    - to_integer self -> Standard.Base.Any.Any
    - value self -> Standard.Base.Any.Any
