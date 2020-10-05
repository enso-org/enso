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
  def apply(
    name: String,
    sbtComponents: Seq[SBTDistributionComponent]
  ): DistributionDescription =
    DistributionDescription(name, sbtComponents)

  def sbtProjects(projects: Project*): Seq[SBTDistributionComponent] =
    macro sbtProjectsImpl

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
