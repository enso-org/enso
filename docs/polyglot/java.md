---
layout: developer-doc
title: Polyglot Java
category: polyglot
tags: [polyglot, java]
order: 3
---

# Polyglot Java

This document deals with the implementation of polyglot interoperation with Java
in the runtime. Please familiarise yourself with the general operation of
[polyglot bindings](./polyglot-bindings.md).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Class Lookup](#class-lookup)
- [Polyglot Library System](#polyglot-library-system)
- [Polyglot Syntax System](#polyglot-syntax-system)

<!-- /MarkdownTOC -->

## Class Lookup

In order for the Enso runtime to effectively find Java objects for working with
in a polyglot fashion, it will look in the `polyglot/java` subdirectory of an
Enso project. This directory has the following requirements placed on it.

- The top level of the `java` directory should contain only `.jar` files and
  directories.
- Each directory must provide a valid class-path structure, with `.class` files
  at the appropriate points.
- Both `.jar` files and directories are added to the runtime class-path for
  Enso, and hence be made available to Enso programs.

> The actionables for this section are:
>
> - In future, we want to expand this to support `.class` files directly, and
>   maybe even compiling Java code.

## Polyglot Library System

The dynamic polyglot system is a dynamic runtime lookup for Java objects,
allowing Enso code to work with them through a runtime reflection-style
mechanism. It is comprised of the following components:

- `Java.lookup_class : Class.Path -> Maybe Class`: A function that lets users
  look up a class by a given name on the runtime classpath.
- `Polyglot.instantiate : Class -> Object`: A function that lets users
  instantiate a class into an object.
- A whole host of functions on the polyglot type that let you dynamically work
  with object bindings.

An example can be found below:

```ruby
main =
    class = Java.lookup_class "org.enso.example.TestClass"
    instance = Polyglot.instantiate1 class (x -> x * 2)
    method = Polyglot.get_member instance "callFunctionAndIncrement"
    Polyglot.execute1 method 10
```

> The actionables for this section are:
>
> - Expand on the detail when there is time.

## Download a Java Library from Maven Central

A typical use-case when bringing in some popular Java library into Enso
ecosystem is to download it (including is **transitive dependencies**) from
[Maven Central](http://maven.org) - a popular place hosting thousands of Java
libraries. Let's **start from scratch** by creating an _empty Enso project_:

```bash
$ bin/enso --new polydemo
$ cd polydemo
polydemo$ find .
.
./src
./src/Main.enso
./package.yaml
```

To populate the appropriate `polyglot/java` subdirectory, let's create following
two files - `pom.xml` and `assembly.xml` and put them into root of the project,
next to `package.yaml` file. The content of `assembly.xml` is:

```xml
<?xml version="1.0"?>
<assembly xmlns="http://maven.apache.org/plugins/maven-assembly-plugin/assembly/1.1.2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation="http://maven.apache.org/plugins/maven-assembly-plugin/assembly/1.1.2 http://maven.apache.org/xsd/assembly-1.1.2.xsd">

    <id>polyglot</id>
    <formats>
        <format>dir</format>
    </formats>
    <baseDirectory>/</baseDirectory>
    <dependencySets>
        <dependencySet>
            <useProjectArtifact>false</useProjectArtifact>
            <scope>runtime</scope>
            <outputDirectory>/</outputDirectory>
            <outputFileNameMapping>${artifact.artifactId}-${artifact.baseVersion}.${artifact.extension}</outputFileNameMapping>
        </dependencySet>
    </dependencySets>
</assembly>
```

and let the content of the `pom.xml` be:

```xml
<?xml version="1.0"?>
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
  <modelVersion>4.0.0</modelVersion>

  <groupId>com.yourorg.yourproject</groupId>
  <artifactId>download</artifactId>
  <version>1.0-SNAPSHOT</version>
  <packaging>jar</packaging>

  <name>Download JARs for Your Project</name>
  <build>
      <plugins>
          <plugin>
              <artifactId>maven-assembly-plugin</artifactId>
              <version>2.4</version>
              <executions>
                  <execution>
                      <id>download</id>
                      <phase>package</phase>
                      <goals>
                          <goal>single</goal>
                      </goals>
                      <configuration>
                          <outputDirectory>polyglot</outputDirectory>
                          <appendAssemblyId>false</appendAssemblyId>
                          <finalName>java</finalName>
                          <descriptors>
                              <descriptor>assembly.xml</descriptor>
                          </descriptors>
                      </configuration>
                  </execution>
              </executions>
          </plugin>
      </plugins>
  </build>
  <dependencies>
      <dependency>
          <!-- find your favorite Java library at maven.org
               and put the co-ordinates here
            -->
          <groupId>com.google.analytics</groupId>
          <artifactId>google-analytics-data</artifactId>
          <version>0.44.0</version>
      </dependency>
  </dependencies>
</project>
```

The files are instructing [Maven](http://maven.apache.org) - _standard Java
build tool_ - to download
[google-analytics-data library](https://central.sonatype.com/artifact/com.google.analytics/google-analytics-data/0.44.0)
library version `0.44.0` and all _its dependencies_ into your `polyglot/java`
directory. Of course, _feel free to find different library_ on
[Maven central](http://maven.apache.org) to download - edit `pom.xml`
appropriately. Once your files are ready execute:

```bash
polydemo$ ls *ml
assembly.xml  package.yaml  pom.xml

polyglot$ mvn -q package

polydemo$ ls polyglot/java/*.jar
...
```

the [mvn command](http://maven.apache.org) invokes
[Maven](http://maven.apache.org) which in turns downloads all the requested
library JAR files (52 of them in the case of `google-analytics-data`) into
`polyglot/java` directory. Now you are ready to use them.

There is a class `com.google.analytics.data.v1alpha.AlphaAnalyticsDataClient`
among the downloaded libraries, as such let's modify `src/Main.enso` to:

```ruby
polyglot java import com.google.analytics.data.v1alpha.AlphaAnalyticsDataClient

main =
  client = AlphaAnalyticsDataClient.create
  client.close
```

run the project and voilá, the Java classes are available to your Enso sources.

## Polyglot Syntax System

The static system, however, lets us do much better in terms of user experience.
Instead of having to dynamically look things up at runtime, we can instead do
the following:

- Statically resolve imports of polyglot bindings within the project to make
  sure that they are available.
- Create java-compatible object entities that dynamically look up and dispatch
  both static methods on classes (by name), and methods on objects (by name).
  This includes the constructor and field reads.
- This invocation syntax is integrated into Enso as variadic methods, allowing
  us to deal with the inter-language impedance mismatch.
- Due to different semantics of Java calls, currying and over-applying functions
  are necessarily disabled for such calls, instead expecting the exact arguments
  list to be passed.

An example can be found below:

```ruby
polyglot java import com.example.MyClass as MyClassJava

main =
    x = MyClassJava.foo 1 2 3
    inst = MyClassJava.new a b c
    bar = inst.methodName x y
```

> The actionables for this section are:
>
> - Expand on the detail as the implementation becomes clear.
