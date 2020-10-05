package src.main.scala.licenses.review

import java.nio.file.Path

import src.main.scala.licenses.DependencySummary

case class Review(settingsRoot: Path, dependencySummary: DependencySummary) {
  def run(): DependencySummary = {
    // TODO change type to add metadata
    DependencySummary(dependencySummary.dependencies.map {
      case (information, attachments) =>
        (information, Attachments.processAttachments(attachments))
    })
  }
}
