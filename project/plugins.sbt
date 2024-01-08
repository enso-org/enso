addSbtPlugin("com.eed3si9n"    % "sbt-assembly"       % "2.1.3")
addSbtPlugin("ch.epfl.scala"   % "sbt-bloop"          % "1.5.6")
addSbtPlugin("com.github.sbt"  % "sbt-license-report" % "1.5.0")
addSbtPlugin("org.scalameta"   % "sbt-scalafmt"       % "2.5.0")
addSbtPlugin("com.simplytyped" % "sbt-antlr4"         % "0.8.3")

libraryDependencies += "io.circe"                   %% "circe-yaml"         % "0.14.2"
libraryDependencies += "commons-io"                  % "commons-io"         % "2.12.0"
libraryDependencies += "nl.gn0s1s"                  %% "bump"               % "0.1.3"
libraryDependencies += "com.google.googlejavaformat" % "google-java-format" % "1.18.1"

scalacOptions ++= Seq("-deprecation", "-feature")
