# Launcher Shims

This small project is used to aid with testing the Enso Launcher, primarily its
self-upgrade mechanism.

See
[UpgradeSpec.scala](../../../engine/launcher/src/test/scala/org/enso/launcher/upgrade/UpgradeSpec.scala)
to see the relevant tests.

Our self-upgrade tests need a way to work with multiple launcher binaries which
report different versions. That is needed to be able to test scenarios where an
older version of the launcher downloads a newer one and replaces itself with it.
The launcher version is a part of the launcher binary. We cannot use the same
binary as then it would be impossible to test if the upgrade has actually
happened. We cannot rely on passing any parameters as the upgrade process does
not do that - the inherent property of launcher update is switching the _binary
executable_ that is being used.

One way to achieve different versions is to instrument launcher builds and
create multiple builds with changed versions. However, with how our build
infrastructure works, it is not easily possible to trigger multiple builds with
changed parameters from within a test suite and would require significant
changes that do not seem justified by a single test. Other than that, the
launcher is built using Native Image which is not incremental and takes a
significant amount of time. Building the big binary multiple times would
increase the test time very significantly.

The chosen solution is to build small Rust binaries which act as a proxy - all
that they do is run the original launcher binary, they forward all passed
parameters but modify one special (hidden in normal operations) parameter that
overrides the reported version.

We could not use Bash scripts to achieve the same purpose as it is necessary for
the launcher executables to be really executables - for example on Windows there
are complexities of moving the currently running executable that we want to test
and this would not be properly achieved with a Bash script (which is just a
script opened by a shell/interpreter). So we need actual binary executables. And
given that the language of choice for such usecases in our project is Rust, this
subproject has been created. One more advantage is that the shims compile
extremely fast.

So the purpose of this project is to build small Rust executables whose purpose
is just to run another executable at some fixed path and pass the parameters.
The only difference between each binary is the version that it will use to
override the original launcher version. To see how the emulation of launcher
version works, see
[InternalOpts.scala](engine/launcher/src/main/scala/org/enso/launcher/cli/InternalOpts.scala).

The launcher shim runs the actual launcher executable as its subprocess,
ensuring that the shim is running the whole time the original executable is
running, thus emulating well the conditions of moving a running executable. The
upgrade mechanism also relies on the launcher detecting the location of the
binary being run. Since the subprocess will still find the location of the
original binary which would be wrong, we use a similar mechanism as for
overriding the version, to override the perceived executable location to point
at the location of the shim - thus any operations that need to localize the
launcher binary or move it will work with the shim - as intended for the tests.
This mechanism is also implemented within `InternalOpts.scala`.
