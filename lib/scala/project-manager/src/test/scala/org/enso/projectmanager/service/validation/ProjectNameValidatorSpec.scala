package org.enso.projectmanager.service.validation

import org.enso.logger.ReportLogsOnFailure
import org.enso.projectmanager.control.effect.Effects
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import zio.{ZAny, ZIO}
import zio.interop.catz.core._

class ProjectNameValidatorSpec
    extends AnyWordSpec
    with Matchers
    with EitherValues
    with Effects
    with ReportLogsOnFailure {

  val projectNameValidator = new ProjectNameValidator[ZIO[ZAny, *, *]]()

  "ProjectNameValidator" must {

    "discard empty name" in {
      projectNameValidator
        .validate("")
        .unsafeRunSync()
        .left
        .value mustEqual ProjectNameValidator.ValidationFailure.EmptyName
    }

    "discard name with only whitespace" in {
      projectNameValidator
        .validate("   ")
        .unsafeRunSync()
        .left
        .value mustEqual ProjectNameValidator.ValidationFailure.EmptyName
    }

    "allow arbitrary non-empty name" in {
      projectNameValidator
        .validate("hello 世界 \uD83D\uDE04")
        .unsafeRunSync()
        .value mustEqual ()
    }

  }
}
