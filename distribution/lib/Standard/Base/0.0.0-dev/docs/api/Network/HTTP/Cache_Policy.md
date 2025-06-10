## Enso Signatures 1.0
## module Standard.Base.Network.HTTP.Cache_Policy
- type Cache_Policy
    - Default
    - No_Cache
    - Use_Cache
    - should_use_cache self request:Standard.Base.Network.HTTP.Request.Request -> Standard.Base.Data.Boolean.Boolean
