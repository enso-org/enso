# Debug Adapter Protocol

[Debug Adapter Protocol](https://www.graalvm.org/latest/tools/dap/) is yet
another instrument available for a Truffle language. The DAP is a native
protocol for VSCode and as such, works only via VSCode. To start Enso with DAP
server waiting for a client to attach, launch enso via:

```
env JAVA_OPTS='-Dpolyglot.dap' ./built-distribution/enso-engine-*/enso-*/bin/enso --run *.enso
```

Once DAP server is started and ready for a client to be attached, the following
output will be printed:

```
[Graal DAP] Starting server and listening on /127.0.0.1:4711
```

In VSCode, create a new
[Launch configuration](https://code.visualstudio.com/docs/editor/debugging#_launch-configurations)
of type `node`, for example your `launch.json` could look like:

\```json
{
    "configurations": [
        {
            "name": "Enso debug",
            "type": "node",
            "debugServer": 4711,
            "request": "attach"
          }
    ]
}
\```

Then you can start debugging via
[Run and Debug view](https://code.visualstudio.com/docs/editor/debugging#_run-and-debug-view) by selecting the `Enso debug` configuration and pressing play:
![image](https://github.com/enso-org/enso/assets/1436948/b939e2a8-7d8e-4286-b7d3-61fbfbd99ce6)



Note that the port 4711 is the default port for DAP.
