package org.enso.cli

import org.enso.cli.internal.NeedlemanWunsch

object Spelling {

  /** Selects closest matches to a potentially mistyped name.
    *
    * @param typo the unknown name that the user typed
    * @param possibleWords a sequence of words that were expected
    * @return a sequence of `possibleWords` that are most similar to `typo`
    */
  def selectClosestMatches(
    typo: String,
    possibleWords: Seq[String]
  ): Seq[String] =
    selectClosestMatchesWithMetadata(typo, possibleWords.map((_, ()))).map(_._1)

  /** Selects closest matches to a potentially mistyped name, returning not only
    * the matches but any metadata that was passed with them. May be useful for
    * including additional text in the match suggestions.
    *
    * @param typo the unknown name that the user typed
    * @param possibleMatches a sequence of tuples (word, metadata)
    * @tparam A type of metadata
    * @return a sequence of tuples from `possibleMatches` whose first elements
    *         were most similar to `typo`
    */
  def selectClosestMatchesWithMetadata[A](
    typo: String,
    possibleMatches: Seq[(String, A)]
  ): Seq[(String, A)] =
    if (
      possibleMatches.isEmpty ||
      typo.length > 2 * possibleMatches.map(_._1.length).max
    ) Seq()
    else {
      val withScores =
        possibleMatches.map(word =>
          (NeedlemanWunsch.score(typo, word._1), word)
        )
      val top = withScores
        .filter(_._1 < maxScoreThreshold)
        .sortBy(_._1)
        .take(topSuggestionsLimit)
      top.map(_._2)
    }

  /** A threshold for the Needleman-Wunsch similarity score. Any matches above
    * that threshold are not considered.
    */
  private val maxScoreThreshold: Double = 10.0

  /** The limit of how many suggestions should be displayed at most.
    */
  private val topSuggestionsLimit = 3
}
