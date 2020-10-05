package src.main.scala.licenses

case class DependencySummary(
  dependencies: Seq[(DependencyInformation, Seq[Attachment])]
)
