import type { Pattern } from '@/util/ast/match'
import type { Vec2 } from '@/util/data/vec2'

interface AllNodeCreationOptions {
  /** Where to place the node; if a value is specified, it will be used exactly.
   *  If unspecified, a location will be chosen based on the source node. */
  position?: Vec2 | undefined
  /** If false, the Component Browser will be opened to edit the node.
   *  If true, the node will be created without further interaction. */
  commit: boolean
  /** The content of the node. If unspecified, it will be determined based on the source node. */
  content?: Pattern | undefined
}

// Opening the CB with specified content has not been needed (yet).
type SupportedNodeContentConfigurations =
  | { commit: false; content: undefined }
  | { commit: true; content: Pattern }
export type NodeCreationOptions = AllNodeCreationOptions & SupportedNodeContentConfigurations
