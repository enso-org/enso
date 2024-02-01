/** @file A node in the drive's item tree. */
import * as backendModule from '#/services/Backend'

// =====================
// === AssetTreeNode ===
// =====================

/** An {@link AssetTreeNode}, but excluding its methods. */
export interface AssetTreeNodeData
  extends Pick<
    AssetTreeNode,
    'children' | 'depth' | 'directoryId' | 'directoryKey' | 'item' | 'key'
  > {}

/** A node in the drive's item tree. */
export default class AssetTreeNode {
  /** Create a {@link AssetTreeNode}. */
  constructor(
    /** The id of the asset (or the placeholder id for new assets). This must never change. */
    public readonly key: backendModule.AssetId,
    /** The actual asset. This MAY change if this is initially a placeholder item, but rows MAY
     * keep updated values within the row itself as well. */
    public item: backendModule.AnyAsset,
    /** The id of the asset's parent directory (or the placeholder id for new assets).
     * This must never change. */
    public readonly directoryKey: backendModule.AssetId,
    /** The actual id of the asset's parent directory (or the placeholder id for new assets). */
    public readonly directoryId: backendModule.DirectoryId,
    /** This is `null` if the asset is not a directory asset, OR if it is a collapsed directory
     * asset. */
    public readonly children: AssetTreeNode[] | null,
    public readonly depth: number
  ) {}

  /** Get an {@link AssetTreeNode.key} from an {@link AssetTreeNode}. Useful for React,
   * becausse references of static functions do not change. */
  static getKey(this: void, node: AssetTreeNode) {
    return node.key
  }

  /** Return a positive number if `a > b`, a negative number if `a < b`, and zero if `a === b`.
   * Uses {@link backendModule.compareAssets} internally. */
  static compare(this: void, a: AssetTreeNode, b: AssetTreeNode) {
    return backendModule.compareAssets(a.item, b.item)
  }

  /** Creates an {@link AssetTreeNode} from a {@link backendModule.AnyAsset}. */
  static fromAsset(
    this: void,
    asset: backendModule.AnyAsset,
    directoryKey: backendModule.AssetId,
    directoryId: backendModule.DirectoryId,
    depth: number,
    getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null
  ): AssetTreeNode {
    getKey ??= oldAsset => oldAsset.id
    return new AssetTreeNode(getKey(asset), asset, directoryKey, directoryId, null, depth)
  }

  /** Create a new {@link AssetTreeNode} with the specified properties updated. */
  with(update: Partial<AssetTreeNodeData>) {
    return new AssetTreeNode(
      update.key ?? this.key,
      update.item ?? this.item,
      update.directoryKey ?? this.directoryKey,
      update.directoryId ?? this.directoryId,
      // `null` MUST be special-cases in the following line.
      // eslint-disable-next-line eqeqeq
      update.children === null ? update.children : update.children ?? this.children,
      update.depth ?? this.depth
    )
  }

  /** Return a new {@link AssetTreeNode} array if any children would be changed by the transformation
   * function, otherwise return the original {@link AssetTreeNode} array. */
  map(transform: (node: AssetTreeNode) => AssetTreeNode) {
    const children = this.children ?? []
    let result: AssetTreeNode = transform(this)
    for (let i = 0; i < children.length; i += 1) {
      const node = children[i]
      if (node == null) {
        break
      }
      const newNode = node.map(transform)
      if (newNode !== node) {
        if (result === this) {
          result = this.with({ children: [...children] })
        }
        // This is SAFE, as `result` is always created with a non-`null` children.
        // (See the line above.)
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        result.children![i] = newNode
      }
    }
    return result
  }

  /** Return a new {@link AssetTreeNode} array if any children would be changed by the transformation
   * function, otherwise return the original {@link AssetTreeNode} array. The predicate is applied to
   * a parent node before it is applied to its children. The root node is never removed. */
  filter(predicate: (node: AssetTreeNode) => boolean) {
    const children = this.children ?? []
    let result: AssetTreeNode | null = null
    for (let i = 0; i < children.length; i += 1) {
      const node = children[i]
      if (node == null) {
        break
      }
      if (!predicate(node)) {
        result ??= this.with({ children: i === 0 ? null : children.slice(0, i) })
      } else {
        let newNode = node
        if (node.children != null) {
          newNode = node.filter(predicate)
          if (newNode !== node) {
            result ??= this.with({ children: children.slice(0, i) })
          }
        }
        if (result) {
          if (!result.children) {
            result = result.with({ children: [newNode] })
          } else {
            result.children.push(newNode)
          }
        }
      }
    }
    return result?.children?.length === 0 ? result.with({ children: null }) : result ?? this
  }

  /** Returns all items in the tree, flattened into an array using pre-order traversal. */
  preorderTraversal(
    preprocess: ((tree: AssetTreeNode[]) => AssetTreeNode[]) | null = null
  ): AssetTreeNode[] {
    return (preprocess?.(this.children ?? []) ?? this.children ?? []).flatMap(node =>
      node.children == null ? [node] : [node, ...node.preorderTraversal(preprocess)]
    )
  }
}
