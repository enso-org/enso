package src.main.scala.licenses.report

import java.nio.file.Path

/**
  * Review status of a license within a specific dependency.
  */
sealed trait LicenseReview
object LicenseReview {

  /**
    * The license used by that dependency was not reviewed.
    */
  case object NotReviewed extends LicenseReview

  /**
    * The license has been reviewed and points to the default file for this license type.
    *
    * This status is assigned automatically if the license type for the artifact is already reviewed
    * and there is no custom override for the given dependency.
    *
    * @param path path to the license file, relative to repository root
    * @param allowAdditionalCustomLicenses if set, does not warn about additional licenses that are
    *                                      different than the default one; that should only be used
    *                                      in special circumstances, for example when a dependency
    *                                      contains multiple licenses and one of them is the default
    *                                      one so we want to keep it, but we also want to keep the
    *                                      others
    */
  case class Default(path: Path, allowAdditionalCustomLicenses: Boolean)
      extends LicenseReview

  /**
    * The lciense for that dependency has been overridden with a custom one.
    *
    * The attached files of the dependency should contain a file with the provided filename.
    */
  case class Custom(filename: String) extends LicenseReview
}
