addSbtPlugin("com.eed3si9n"       % "sbt-assembly"             % "1.1.0")
addSbtPlugin("ch.epfl.scala"      % "sbt-bloop"                % "1.5.3")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.10.1")
addSbtPlugin("com.typesafe.sbt"   % "sbt-license-report"       % "1.2.0")
addSbtPlugin("com.lightbend.sbt"  % "sbt-java-formatter"       % "0.7.0")
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.4.6")

libraryDependencies += "io.circe" %% "circe-yaml" % "0.14.1"
libraryDependencies += "commons-io" % "commons-io" % "2.11.0"
