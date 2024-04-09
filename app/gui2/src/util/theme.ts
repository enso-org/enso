import originalTheme from '@/util/theme.json'

const theme: Theme = originalTheme
export default theme

export interface Theme {
  /** Configuration for node rendering. */
  node: NodeTheme
  /** Configuration for edge rendering. */
  edge: EdgeTheme
}

/** Configuration for node rendering. */
export interface NodeTheme {
  /** The default height of a node. */
  height: number
  /** The maximum corner radius of a node. If the node is shorter than `2 * corner_radius`,
   * the corner radius will be half of the node's height instead. */
  corner_radius: number
  /** The vertical gap between nodes in automatic layout. */
  vertical_gap: number
  /** The horizontal gap between nodes in automatic layout. */
  horizontal_gap: number
}

/** Configuration for edge rendering. */
export interface EdgeTheme {
  /** Minimum height above the target the edge must approach it from. */
  min_approach_height: number
  /** The preferred arc radius for corners, when an edge changes direction. */
  radius: number
  /** Configuration for edges that change direction once. */
  one_corner: EdgeOneCornerTheme
  /** Configuration for edges with change direction three times. */
  three_corner: EdgeThreeCornerTheme
}

/** Configuration for edges that change direction once. */
export interface EdgeOneCornerTheme {
  /** The y-allocation for the radius will be the full available height minus this value. */
  radius_y_adjustment: number
  /** The base x-allocation for the radius. */
  radius_x_base: number
  /** Proportion (0-1) of extra x-distance allocated to the radius. */
  radius_x_factor: number
  /** Distance for the line to continue under the node, to ensure that there isn't a gap. */
  source_node_overlap: number
  /** Minimum arc radius at which we offset the source end to exit normal to the node's curve. */
  minimum_tangent_exit_radius: number
}

/** Configuration for edges with change direction three times. */
export interface EdgeThreeCornerTheme {
  /** The maximum arc radius. */
  radius_max: number
  backward_edge_arrow_threshold: number
  /** The maximum radius reduction (from [`RADIUS_BASE`]) to allow when choosing whether to use
   *  the three-corner layout that doesn't use a backward corner. */
  max_squeeze: number
}
