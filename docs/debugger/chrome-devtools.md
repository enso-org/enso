# Chrome Developer Tools Debugger

As a well written citizen of the [GraalVM](http://graalvm.org) project the Enso
language can be used with existing tools available for the overall platform. One
of them is
[Chrome Debugger](https://www.graalvm.org/22.1/tools/chrome-debugger/) and Enso
language is fully integrated with it. Launch the `bin/enso` executable with
additional `--inspect` option and debug your Enso programs in _Chrome Developer
Tools_.

```bash
enso$ ./built-distribution/enso-engine-*/enso-*/bin/enso --inspect --run ./test/Tests/src/Data/Numbers_Spec.enso
Debugger listening on ws://127.0.0.1:9229/Wugyrg9
For help, see: https://www.graalvm.org/tools/chrome-debugger
E.g. in Chrome open: devtools://devtools/bundled/js_app.html?ws=127.0.0.1:9229/Wugyrg9
```

copy the printed URL into chrome browser and you should see:

![Chrome Debugger](https://user-images.githubusercontent.com/26887752/209614265-684f530e-cf7e-45d5-9450-7ea1e4f65986.png)

Step in, step over, set breakpoints, watch values of the variables as well as
evaluate arbitrary expressions in the console. Note that as of December 2022,
with GraalVM 22.3.0, there is a well-known
[bug in Truffle](https://github.com/oracle/graal/issues/5513) that causes
`NullPointerException` when a host object gets into the chrome inspector. There
is a workaround for that, but it may not work in certain situations. Therefore,
if you encounter `NullPointerException` thrown from

```
at org.graalvm.truffle/com.oracle.truffle.polyglot.PolyglotContextImpl.getContext(PolyglotContextImpl.java:685)
```

simply ignore it. It will be handled within the debugger and should not affect
the rest of the environment.

## Tips and tricks

- Use `env JAVA_OPTS=-Dpolyglot.inspect.Path=enso_debug` to set the chrome to
  use a fixed URL. In this case the URL is
  `devtools://devtools/bundled/js_app.html?ws=127.0.0.1:9229/enso_debug`
