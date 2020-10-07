import com.typesafe.sbt.SbtLicenseReport.autoImportImpl.updateLicenses
import sbt.Keys.{ivyModule, update, updateClassifiers}
import sbt.Project
import src.main.scala.licenses.{
  DistributionDescription,
  SBTDistributionComponent
}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Distribution {

  /**
    * Creates a [[DistributionDescription]].
    */
  def apply(
    name: String,
    sbtComponents: Seq[SBTDistributionComponent]
  ): DistributionDescription =
    DistributionDescription(name, sbtComponents)

  /**
    * A macro that creates [[SBTDistributionComponent]] descriptions from a list
    * of project references.
    */
  def sbtProjects(projects: Project*): Seq[SBTDistributionComponent] =
    macro sbtProjectsImpl

  /**
    * Implementation of the [[sbtProjects]] macro.
    *
    * It triggers execution of the tasks that are used to get information from
    * SBT on each project.
    */
  def sbtProjectsImpl(c: blackbox.Context)(
    projects: c.Expr[Project]*
  ): c.Expr[Seq[SBTDistributionComponent]] = {
    import c.universe._
    val gathered = {
      projects.map(p =>
        reify {
          SBTDistributionComponent(
            p.splice.id,
            (p.splice / updateLicenses).value,
            (p.splice / ivyModule).value,
            (p.splice / updateClassifiers).value
          )
        }
      )
    }
    c.Expr[Seq[SBTDistributionComponent]](
      Apply(reify(Seq).tree, gathered.map(_.tree).toList)
    )
  }
}
