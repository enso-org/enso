package org.enso.launcher.components.runner

import nl.gn0s1s.bump.SemVer

case class RunSettings(version: SemVer, runnerArguments: Seq[String])
