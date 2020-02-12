This is a provisional implementation for the file server meant to ease
testing and development of File Manager integration before the "actual"
implementation is ready.

It can be run by a command:
```
cargo run -p file-manager-server
```

By default it listens to port `30616`, use the `ENSO_FILE_MANAGER_PORT` 
environment variable to customize this behaviour.

The protocol is described in [a separate document](../README.md). Only
its subset is provided. The following methods are currently implemented:
`read`, `write`, `exists`, `copyFile`.