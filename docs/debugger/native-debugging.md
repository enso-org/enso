# Native Debugging

Since 2025.1.1 version Enso comes with first class support for
[native image execution mode](../infrastructure/native-image.md#engine-runner-configuration).
Such a mode is _fast to start_ and is thus really suitable for production. In
majority of cases it behaves exactly the same as so called `--jvm` mode used
during development. As such there shall not be so much need, to debug the
_native image_ itself. Use [runtime debugging](runtime-debugging.md) in the JVM
mode where possible. Should JVM debugging not be enough, continue reading.

## Building for Debugging

By default the
[native image execution mode](../infrastructure/native-image.md#engine-runner-configuration)
doesn't contain enough information for debugging. The _native executable_ needs
to be re-built with such debugging information. Since
[#12993](https://github.com/enso-org/enso/pull/12993) rebuilding is as easy as:

```bash
enso$ ENSO_LAUNCHER=native,-ls,fast,debug sbt buildEngineDistribution
```

and then use VSCode with
[Enso extension](https://marketplace.visualstudio.com/items?itemName=Enso.enso4vscode)
to debug `enso` native executable. Btw. no need for `-ls,fast` - they just make
the build faster.

## Enso VSCode Extension

With the
[Enso extension](https://marketplace.visualstudio.com/items?itemName=Enso.enso4vscode)
one can experience following:

![VSCode](https://github.com/user-attachments/assets/612eca0a-5125-4f57-a7dc-60f7b10ae15a)

The above picture shows what needs to be done in VSCode:

- once the _debug ready_ built is finished

- switch to _"Run and Debug"_ perspective
- select _"Launch Native Image"_ configuration
- select an `.enso` file in editor like `Vector_Spec.enso`
- press F5 to start debugging it

![debugging](https://github.com/user-attachments/assets/f78a7030-0136-4c20-8271-7b7addba0ea0)

- as soon as the output appears
- pause the execution
- inspect stack and local variables
- add breakpoints
- step in, step over, step out, continue
