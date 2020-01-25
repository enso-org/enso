File Manager consists of pair server and client. They communicate by exchaning
messages, following JSON-RPC 2.0.

# Setup
Establish websocket connection. 

In future it is expected that some kind of authorization will be required by the
server. As of now, its details remain unspecified.

# General protocol
Remote calls made between File Manager client and server follow [JSON-RPC 2.0
protocol](https://www.jsonrpc.org/specification).

There are two primary cases:
* RPC calls from client to [server methods](#Methods);
* [Notifications](#Notifications) sent from server to client.

All messages are text with JSON-encoded values.

File Manager accepts only method calls (request objects).

File Manager responds with call results and may send notifications.

# Methods
| Method        | Input                        | Result     |
|---------------|------------------------------|------------|
| copyDirectory | {from:Path, to:Path}         | ()         |
| copyFile      | {from:Path, to:Path}         | ()         |
| deleteFile    | {path:Path}                  | ()         |
| exists        | {path:Path}                  | Boolean    |
| list          | {path:Path}                  | [Path]     |
| moveDirectory | {from:Path, to:Path}         | ()         |
| moveFile      | {from:Path, to:Path}         | ()         |
| read          | {path:Path}                  | String     |
| status        | {path:Path}                  | Attributes |
| touch         | {path:Path}                  | ()         |
| write         | {path:Path, contents:String} | ()         |
| createWatch   | {path:Path}                  | UUID       |
| deleteWatch   | {watchId:UUID}               | ()         |

Where `()` is a unit value.

# Notifications
Notifications are emitted by the server.

| Method          | Input                       | Result |
|-----------------|-----------------------------|--------|
| filesystemEvent | {path:Path, kind:EventKind} | N/A    |

`filesystemEvent` notification is emitted for paths that are tracked by watch
(i.e. subtree of a location passed to `createWatch` method).

It should be noted that watch notifications are not reliable and significantly
os-dependent.  

# Types
```
Attributes = struct { 
    creationTime      : FileTime,
    lastAccess_time   : FileTime, 
    lastModified_time : FileTime, 
    fileKind          : FileKind,
    byteSize          : u64,
}

EventKind = enum { Created, Deleted, Modified, Overflow }
FileKind  = enum { Directory, RegularFile, SymbolicLink, Other }
```

# JSON Encoding
Struct values are serialized as map, e.g. `{ "field_name" : field_value }`.

Enum values are serialized as map `{ "variant_name" : variant_value }` or just
`"variant_name"` if there variants have no inner value.
Transitive enums (i.e. enums of enums) are flattened, no intermediate variant
names shall appear. 

`()` (unit value) is serialized as `null`.

`FileTime` value is serialized as a string compliant with RFC3339 / ISO8601 text
format, e.g. `"2020-01-07T21:25:26Z"`.

`Path` is serialized as JSON string value, e.g. `"./Main.luna"`.

`UUID` is serialzied as string using 8-4-4-4-12 format, e.g.
`"02723954-fbb0-4641-af53-cec0883f260a"`.

`u64` is an unsigned 64-bit integer value.

## Examples

### Call to `exists` method
#### Request (call)
```json
{
    "jsonrpc" : "2.0",
    "id"      : 0,
    "method"  : "exists",
    "input"   : { "path" : "./Main.luna" }
}
```
#### Response
```json
{
    "jsonrpc" : "2.0",
    "id"      : 0,
    "result"  : true
}
```

### Filesystem Event Notification
#### Request (notification)
```json
{
    "jsonrpc" : "2.0",
    "method"  : "filesystemEvent",
    "params"  : { "path" : "./Main.luna", "kind" : "Modified" }
}
```

Notification requests gets no response.


### `Attributes` structure
`Attributes` value may be serialized to a following JSON:
```json
{
    "creationTime"     : "2020-01-07T21:25:26Z",
    "lastAccessTime"   : "2020-01-21T22:16:51.123994500+00:00",
    "lastModifiedTime" : "2020-01-07T21:25:26Z",
    "fileKind"         : "RegularFile",
    "sizeInBytes"      : 125125
}
```


