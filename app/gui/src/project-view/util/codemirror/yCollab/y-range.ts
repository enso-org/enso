import * as Y from 'yjs'

/**
 * Defines a range on text using relative positions that can be transformed back to
 * absolute positions. (https://docs.yjs.dev/api/relative-positions)
 */
export class YRange {
  /** TODO: Add docs */
  constructor(
    readonly yanchor: Y.RelativePosition,
    readonly yhead: Y.RelativePosition,
  ) {
    this.yanchor = yanchor
    this.yhead = yhead
  }

  /** TODO: Add docs */
  toJSON() {
    return {
      yanchor: Y.relativePositionToJSON(this.yanchor),
      yhead: Y.relativePositionToJSON(this.yhead),
    }
  }

  /** TODO: Add docs */
  static fromJSON(json: { yanchor: unknown; yhead: unknown }) {
    return new YRange(
      Y.createRelativePositionFromJSON(json.yanchor),
      Y.createRelativePositionFromJSON(json.yhead),
    )
  }
}
