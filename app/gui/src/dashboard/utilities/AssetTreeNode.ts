/** @file A node in the drive's item tree. */
import type * as assetEvent from '#/events/assetEvent'

import * as backendModule from '#/services/Backend'

// =====================
// === AssetTreeNode ===
// =====================

/** An {@link AssetTreeNode}, but excluding its methods. */
export type AssetTreeNodeData = Pick<
  AssetTreeNode,
  | 'children'
  | 'createdAt'
  | 'depth'
  | 'directoryId'
  | 'directoryKey'
  | 'initialAssetEvents'
  | 'item'
  | 'key'
  | 'path'
>

/** All possible variants of {@link AssetTreeNode}s. */
// The `Item extends Item` is required to trigger distributive conditional types:
// https://www.typescriptlang.org/docs/handbook/2/conditional-types.html#distributive-conditional-types
export type AnyAssetTreeNode<Item extends backendModule.AnyAsset = backendModule.AnyAsset> =
  Item extends Item ? AssetTreeNode<Item> : never

/** A node in the drive's item tree. */
export default class AssetTreeNode<Item extends backendModule.AnyAsset = backendModule.AnyAsset> {
  readonly type: Item['type']
  /** Create a {@link AssetTreeNode}. */
  constructor(
    /**
     * The actual asset. This MAY change if this is initially a placeholder item, but rows MAY
     * keep updated values within the row itself as well.
     */
    public item: Item,
    /**
     * The id of the asset's parent directory (or the placeholder id for new assets).
     * This must never change.
     */
    public readonly directoryKey: backendModule.DirectoryId,
    /** The actual id of the asset's parent directory (or the placeholder id for new assets). */
    public readonly directoryId: backendModule.DirectoryId,
    /**
     * This is `null` if the asset is not a directory asset, OR a directory asset whose contents
     * have not yet been fetched.
     */
    public readonly children: AnyAssetTreeNode[] | null,
    public readonly depth: number,
    public readonly path: string,
    public readonly initialAssetEvents: readonly assetEvent.AssetEvent[] | null,
    /**
     * The internal (to the frontend) id of the asset (or the placeholder id for new assets).
     * This must never change, otherwise the component's state is lost when receiving the real id
     * from the backend.
     */
    public readonly key: Item['id'] = item.id,
    public readonly createdAt = new Date(),
  ) {
    this.type = item.type
  }

  /**
   * Return a positive number if `a > b`, a negative number if `a < b`, and zero if `a === b`.
   * Uses {@link backendModule.compareAssets} internally.
   */
  static compare(this: void, a: AssetTreeNode, b: AssetTreeNode) {
    return backendModule.compareAssets(a.item, b.item)
  }

  /** Creates an {@link AssetTreeNode} from a {@link backendModule.AnyAsset}. */
  static fromAsset<Asset extends backendModule.AnyAsset>(
    this: void,
    asset: Asset,
    directoryKey: backendModule.DirectoryId,
    directoryId: backendModule.DirectoryId,
    depth: number,
    path: string,
    initialAssetEvents: readonly assetEvent.AssetEvent[] | null = null,
    key: Asset['id'] = asset.id,
  ): AnyAssetTreeNode {
    return new AssetTreeNode(
      asset,
      directoryKey,
      directoryId,
      null,
      depth,
      path,
      initialAssetEvents,
      key,
    ).asUnion()
  }

  /** Return `this`, coerced into an {@link AnyAssetTreeNode}. */
  asUnion() {
    // This is SAFE, as an `AssetTreeNode` cannot change types, and `AnyAssetTreeNode` is an
    // exhaustive list of variants.
    // eslint-disable-next-line no-restricted-syntax
    return this as AnyAssetTreeNode
  }

  /** Whether this node contains a specific type of asset. */
  isType<Type extends backendModule.AssetType>(
    type: Type,
  ): this is AssetTreeNode<backendModule.AnyAsset<Type>> {
    return backendModule.assetIsType(type)(this.item)
  }

  /** Create a new {@link AssetTreeNode} with the specified properties updated. */
  with(update: Partial<AssetTreeNodeData>): AnyAssetTreeNode {
    return new AssetTreeNode(
      update.item ?? this.item,
      update.directoryKey ?? this.directoryKey,
      update.directoryId ?? this.directoryId,
      // `null` MUST be special-cases in the following line.
      // eslint-disable-next-line eqeqeq
      update.children === null ? update.children : update.children ?? this.children,
      update.depth ?? this.depth,
      update.path ?? this.path,
      update.initialAssetEvents ?? this.initialAssetEvents,
      update.key ?? this.key,
      update.createdAt ?? this.createdAt,
    ).asUnion()
  }

  /**
   * Return a new {@link AssetTreeNode} array if any children would be changed by the transformation
   * function, otherwise return the original {@link AssetTreeNode} array.
   */
  map(transform: (node: AnyAssetTreeNode) => AnyAssetTreeNode): AnyAssetTreeNode {
    const children = this.children ?? []

    let result: AnyAssetTreeNode = transform(this.asUnion())
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

  /**
   * Return a new {@link AssetTreeNode} array if any children would be changed by the transformation
   * function, otherwise return the original {@link AssetTreeNode} array. The predicate is applied to
   * a parent node before it is applied to its children. The root node is never removed.
   */
  filter(predicate: (node: AnyAssetTreeNode) => boolean): AnyAssetTreeNode {
    const children = this.children ?? []
    let result: AnyAssetTreeNode | null = null
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
    return result?.children?.length === 0 ?
        result.with({ children: null })
      : result ?? this.asUnion()
  }

  /** Returns all items in the tree, flattened into an array using pre-order traversal. */
  preorderTraversal(
    preprocess: ((tree: AnyAssetTreeNode[]) => AnyAssetTreeNode[]) | null = null,
  ): AnyAssetTreeNode[] {
    const children = this.children ?? []
    return (preprocess?.(children) ?? children).flatMap((node) =>
      node.children == null ? [node] : [node, ...node.preorderTraversal(preprocess)],
    )
  }

  /** Check whether a pending rename is valid. */
  isNewTitleValid(newTitle: string, siblings?: readonly AssetTreeNode[] | null) {
    siblings ??= []
    return (
      newTitle !== '' &&
      newTitle !== this.item.title &&
      siblings.every((sibling) => {
        const isSelf = sibling.key === this.key
        const hasSameType = sibling.item.type === this.item.type
        const hasSameTitle = sibling.item.title === newTitle
        return !(!isSelf && hasSameType && hasSameTitle)
      })
    )
  }
}
