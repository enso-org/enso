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

There is a
[Launch configuration](https://code.visualstudio.com/docs/editor/debugging#_launch-configurations)
in the repository in
[.vscode/launch.json](https://github.com/enso-org/enso/blob/a123cd0d9f4b04d05aae7a5231efba554062188f/.vscode/launch.json#L13-L16)
with name `Debug Adapter Protocol`, you can start debugging via
[Run and Debug view](https://code.visualstudio.com/docs/editor/debugging#_run-and-debug-view)
by selecting the `Debug adapter protocol` configuration and pressing play:
![image](https://github.com/enso-org/enso/assets/14013887/7f15abfd-b4fa-45d3-a100-142c465b6444)

Another screenshot showing the DAP in action:
![image](https://github.com/enso-org/enso/assets/14013887/41dd8b80-dbac-4a11-b3e2-97c99e42c507)

Note that the port 4711 is the default port for DAP.
