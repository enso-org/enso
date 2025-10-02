import * as Y from 'yjs'

/**
 * Defines a range on text using relative positions that can be transformed back to
 * absolute positions. (https://docs.yjs.dev/api/relative-positions)
 */
export class YRange {
  readonly yanchor: Y.RelativePosition
  readonly yhead: Y.RelativePosition

  /** Create a new YRange. */
  constructor(yanchor: Y.RelativePosition, yhead: Y.RelativePosition) {
    this.yanchor = yanchor
    this.yhead = yhead
  }

  /** Convert this YRange to a plain JSON object. */
  toJSON() {
    return {
      yanchor: Y.relativePositionToJSON(this.yanchor),
      yhead: Y.relativePositionToJSON(this.yhead),
    }
  }

  /** Convert a plain JSON object to a YRange. */
  static fromJSON(json: { yanchor: unknown; yhead: unknown }) {
    return new YRange(
      Y.createRelativePositionFromJSON(json.yanchor),
      Y.createRelativePositionFromJSON(json.yhead),
    )
  }
}
