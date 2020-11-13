package org.enso.cli.internal

/** Allows to display a progress bar in a terminal.
  */
class ProgressBar {

  /** Begins drawing the progressbar.
    */
  def start(): Unit = {
    drawProgressBar(0, "")
  }

  /** Updates the progressbar with a percentage.
    */
  def updateProgress(percentage: Float): Unit = {
    drawProgressBar(
      (percentage / 100.0f * totalStates).floor.toInt,
      s"${percentage.toInt}%"
    )
  }

  /** Clears the progressbar.
    */
  def hide(): Unit = {
    print("\r" + " " * (paddingLength + 2) + "\r")
  }

  /** Displays a next step of animation indicating progress of a task with an
    * unknown total.
    */
  def showUnknownProgress(): Unit = {
    state += 1
    val pos    = state % progressWidth
    val prefix = " " * pos
    val suffix = " " * (progressWidth - pos - 1)
    val bar    = s"  [$prefix?$suffix]\r"
    print(bar)
  }

  private var state               = 0
  private var longestComment: Int = 0
  def paddingLength: Int          = progressWidth + 4 + longestComment

  private val progressWidth  = 20
  private val progressStates = Seq(".", "#")
  private val totalStates    = progressWidth * progressStates.length

  private def drawProgressBar(state: Int, comment: String): Unit = {
    longestComment = Seq(longestComment, comment.length).max

    val stateClamped = Seq(Seq(state, 0).max, totalStates).min
    val full =
      progressStates.last * (stateClamped / progressStates.length)
    val partial = {
      val idx = stateClamped % progressStates.length
      if (idx > 0) {
        progressStates(idx - 1)
      } else ""
    }

    val bar     = full + partial
    val rest    = " " * (progressWidth - bar.length)
    val line    = s"[$bar$rest] $comment"
    val padding = " " * (paddingLength - line.length)
    print("  " + line + padding + "\r")
  }
}
