package org.enso.projectmanager.data

import nl.gn0s1s.bump.SemVer

case class EngineVersion(version: SemVer, markedAsBroken: Boolean)
