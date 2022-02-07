## Installing GraalVM on M1 Mac

This guide describes how to setup the specific version of GraalVM on M1 Mac
using [`sdkman`](https://sdkman.io/).

To install GraalVM on any other platform, you can use

```
$ sdk install java 21.1.0.r11-grl
```

However, on M1 Macs this won't work, and you need to perform manual actions
described below.

If you are not using `sdkman`, skip the sections marked with `(Optional)`.

#### (Optional) Install sdkman

```
$ curl -s "https://get.sdkman.io" | bash
$ source "$HOME/.sdkman/bin/sdkman-init.sh"
```

You'll want to add `source "$HOME/.sdkman/bin/sdkman-init.sh"` to the end of
your `.profile` here as well.

#### Find out what version you need

Please see variables `graalVersion` and `javaVersion` in
[`build.sbt`](../build.sbt) file. You'll need the GraalVM with the exact same
version and Java version. Currently, it would be version `21.1.0` with Java 11.

#### Install GraalVM

Go to [GraalVM CE releases](https://github.com/graalvm/graalvm-ce-builds) and
download the release tarball. Unpack it and install into the system directory:

```
$ tar xzf graalvm-ce-java11-darwin-amd64-21.1.0.tar.gz
$ sudo mv graalvm-ce-java11-21.1.0 /Library/Java/JavaVirtualMachines
```

If you use MacOS Catalina or later, you need to unquarantine it:

```
$ sudo xattr -r -d com.apple.quarantine /Library/Java/JavaVirtualMachines/graalvm-ce-java11-21.1.0
```

#### (Optional) Add installed JVM to sdkman

```
$ sdk install java 21.1.0.r11-grl /Library/Java/JavaVirtualMachines/graalvm-ce-java11-21.1.0/Contents/Home
$ sdk default java 21.1.0.r11-grl
```
