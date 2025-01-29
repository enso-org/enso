# Enso Compiler IR

Enso IR, currently implemented in Scala, with base class
`org.enso.compiler.core.IR`, is created from the output of the native
[parser](../parser/README.md). The IR is an immutable annotated AST subjected to
multiple passes. Every pass is a class implementing the
`org.enso.compiler.pass.IRPass` interface.

See [Runtime roadmap - static analysis](../runtime-roadmap.md#static-analysis)
for future goals.

## Dumping IR

The IR can be visualized using the `enso.compiler.dumpIr` system property. The
value of the property is a substring of a module name to dump. IRs are dumped
into the [IGV tool](https://www.graalvm.org/latest/tools/igv/) in a similar way
to how GraalVM graphs are dumped, which is documented in
[enso4igv](https://github.com/enso-org/enso/blob/2e714a70ddf12456e9f3fa9e132fd2ac43aa3b77/tools/enso4igv/IGV.md#using-the-igv).

When using the `enso.compiler.dumpIr` property, one has to add
`--add-exports jdk.internal.vm.compiler/org.graalvm.graphio=org.enso.runtime.compiler.dump.igv`
to the `JAVA_OPTS` env var, because the IGV dumper uses an internal package of
GraalVM JDK's module which is not exported by default.

Usage example:

```
$ env JAVA_OPTS='--add-exports jdk.internal.vm.compiler/org.graalvm.graphio=org.enso.runtime.compiler.dump.igv' ./built-distribution/*/bin/enso --vm.D enso.compiler.dumpIr=Vector --no-ir-caches --run tmp.enso
```

The IR graphs are dumped directly to IGV, if it is running, or to the `ir-dumps`
directory in the
[BGV](https://www.graalvm.org/graphio/javadoc/jdk/graal/compiler/graphio/package-summary.html)
format.

### Description of the graphs

For a module, multiple graphs are dumped. Names of the graphs correspond to the
names of the Compiler passes, as can be seen on the screenshot:
![1](https://github.com/user-attachments/assets/eb9bc883-c482-4461-8e38-0b615bec2e83)

Opening the first graph for the `Vector` module is overwhelming, since it has
more than 3000 nodes:
![2](https://github.com/user-attachments/assets/296c0b63-a28c-4b9b-bed7-22c52f9f1cd3)
However, nodes are structured in _blocks_. Blocks can be seen on the right side
in the `Control Flow` tool window.

Zoom into a particular block:

- Double-click on the block with id 1 in the `Control Flow` tool window.
- Click on the `Zoom to Selection` button in the toolbar.
  ![3](https://github.com/user-attachments/assets/dd8a4c7f-b8e4-4356-b1fd-64fa7824134c)

Below the tab, there are all the passes displayed as points (_Phase toolbar_).
Hovering over a point shows the pass name:
![4](https://github.com/user-attachments/assets/b5449f4c-de35-46d4-8fee-e8963cf3d4f1)

Click-and-drag the mouse to select a region of passes. This will display the
difference in the graph between those passes. In the following screenshot, we
can see the difference between the very first pass, and `MethodCalls` pass:
![5](https://github.com/user-attachments/assets/b320d706-e074-4509-a836-5e88e5305a55)
Orange nodes represent those with changed properties.

Clicking on a single changed node, we can see that there is `NEW_passData`
property:
![6](https://github.com/user-attachments/assets/e3ce8f3c-a227-4626-96f8-910dc4d4c31a)
In this case, we can see that `AliasMetadata.ChildScope` passData was added to
that node between the selected passes.

In the following screenshot, we can see that the `8 Function$Lambda` node was
removed (displayed in red) between passes `15: AliasAnalysis` and
`18: SuspendedArguments`:
![7](https://github.com/user-attachments/assets/21164dec-5a2b-449a-a335-9f0f4b60e4d0)
Nodes that were added are displayed in green.

Clicking on a single node, the points representing passes in the _Phase toolbar_
change colors:

- White: No change
- Orange: Property on the node changed.
- Black: Node was removed
- Green: Node was added
  ![8](https://github.com/user-attachments/assets/befc2083-e262-4933-90bc-f1729387d1ee)
