/** @file A node in the drive's item tree. */
import * as backendModule from '#/services/Backend'

import * as array from '#/utilities/array'

// =====================
// === AssetTreeNode ===
// =====================

/** An {@link AssetTreeNode}, but excluding its methods. */
export interface AssetTreeNodeData
  extends Pick<
    AssetTreeNode,
    'children' | 'depth' | 'directory' | 'directoryKey' | 'item' | 'key'
  > {}

/** All possible variants of {@link AssetTreeNode}s. */
// The `Item extends Item` is required to trigger distributive conditional types:
// https://www.typescriptlang.org/docs/handbook/2/conditional-types.html#distributive-conditional-types
export type AnyAssetTreeNode<
  Item extends backendModule.AnySmartAsset = backendModule.AnySmartAsset,
> = Item extends Item ? AssetTreeNode<Item> : never

/** A node in the drive's item tree. */
export default class AssetTreeNode<
  Item extends backendModule.AnySmartAsset = backendModule.AnySmartAsset,
> {
  readonly type: Item['type']
  /** Create a {@link AssetTreeNode}. */
  constructor(
    /** The actual asset. This MAY change if this is initially a placeholder item, but rows MAY
     * keep updated values within the row itself as well. */
    public item: Item,
    /** The id of the asset's parent directory (or the placeholder id for new assets).
     * This must never change. */
    public readonly directoryKey: backendModule.DirectoryId,
    /** The actual id of the asset's parent directory (or the placeholder id for new assets). */
    public readonly directory: backendModule.SmartDirectory,
    /** This is `null` if the asset is not a directory asset, OR if it is a collapsed directory
     * asset. */
    public readonly children: AnyAssetTreeNode[] | null,
    public readonly depth: number,
    /** The internal (to the frontend) id of the asset (or the placeholder id for new assets).
     * This must never change, otherwise the component's state is lost when receiving the real id
     * from the backend. */
    public readonly key: Item['value']['id'] = item.value.id
  ) {
    this.type = item.type
  }

  /** Get an {@link AssetTreeNode.key} from an {@link AssetTreeNode}. Useful for React,
   * becausse references of static functions do not change. */
  static getKey(this: void, node: AssetTreeNode) {
    return node.key
  }

  /** Return a positive number if `a > b`, a negative number if `a < b`, and zero if `a === b`.
   * Uses {@link backendModule.compareAssets} internally. */
  static compare(this: void, a: AssetTreeNode, b: AssetTreeNode) {
    return backendModule.compareAssets(a.item.value, b.item.value)
  }

  /** Creates an {@link AssetTreeNode} from a {@link backendModule.AnySmartAsset}. */
  static fromSmartAsset(
    this: void,
    smartAsset: backendModule.AnySmartAsset,
    directoryKey: backendModule.DirectoryId,
    directory: backendModule.SmartDirectory,
    depth: number,
    key: backendModule.AssetId = smartAsset.value.id
  ): AnyAssetTreeNode {
    return new AssetTreeNode(smartAsset, directoryKey, directory, null, depth, key).asUnion()
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
    type: Type
  ): this is AssetTreeNode<backendModule.AnySmartAsset<Type>> {
    return backendModule.smartAssetIsType(type)(this.item)
  }

  /** Create a new {@link AssetTreeNode} with the specified properties updated. */
  with(update: Partial<AssetTreeNodeData>): AnyAssetTreeNode {
    return new AssetTreeNode(
      update.item ?? this.item,
      update.directoryKey ?? this.directoryKey,
      update.directory ?? this.directory,
      // `null` MUST be special-cases in the following line.
      // eslint-disable-next-line eqeqeq
      update.children === null ? update.children : update.children ?? this.children,
      update.depth ?? this.depth,
      update.key ?? this.key
    ).asUnion()
  }

  /** Return a new {@link AssetTreeNode} array if any children would be changed by the transformation
   * function, otherwise return the original {@link AssetTreeNode} array. */
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

  /** Return a new {@link AssetTreeNode} array if any children would be changed by the transformation
   * function, otherwise return the original {@link AssetTreeNode} array. The predicate is applied to
   * a parent node before it is applied to its children. The root node is never removed. */
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
    return result?.children?.length === 0
      ? result.with({ children: null })
      : result ?? this.asUnion()
  }

  /** Returns all items in the tree, flattened into an array using pre-order traversal. */
  preorderTraversal(
    preprocess: ((tree: readonly AnyAssetTreeNode[]) => readonly AnyAssetTreeNode[]) | null = null
  ): readonly AnyAssetTreeNode[] {
    return (preprocess?.(this.children ?? []) ?? this.children ?? []).flatMap(node =>
      node.children == null ? [node] : [node, ...node.preorderTraversal(preprocess)]
    )
  }

  /** Return self, with new children added into its list of children.
   * The children MAY be of different asset types. */
  withChildrenInserted(
    children: readonly backendModule.AnySmartAsset[],
    directoryKey: backendModule.DirectoryId,
    directory: backendModule.SmartDirectory,
    getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null
  ) {
    const depth = this.depth + 1
    const nodes = (this.children ?? []).filter(
      node => node.item.value.type !== backendModule.AssetType.specialEmpty
    )
    const byType: Record<backendModule.AssetType, backendModule.AnySmartAsset[]> = {
      [backendModule.AssetType.directory]: [],
      [backendModule.AssetType.project]: [],
      [backendModule.AssetType.file]: [],
      [backendModule.AssetType.dataLink]: [],
      [backendModule.AssetType.secret]: [],
      [backendModule.AssetType.specialLoading]: [],
      [backendModule.AssetType.specialEmpty]: [],
    }
    for (const child of children) {
      byType[child.value.type].push(child)
    }
    let newNodes = nodes
    for (const childrenOfSpecificType of Object.values(byType)) {
      const firstChild = childrenOfSpecificType[0]
      if (firstChild) {
        const typeOrder = backendModule.ASSET_TYPE_ORDER[firstChild.value.type]
        const nodesToInsert = childrenOfSpecificType.map(asset =>
          AssetTreeNode.fromSmartAsset(asset, directoryKey, directory, depth, getKey?.(asset.value))
        )
        newNodes = array.splicedBefore(
          newNodes,
          nodesToInsert,
          child => backendModule.ASSET_TYPE_ORDER[child.item.value.type] >= typeOrder
        )
      }
    }
    return newNodes === nodes ? this : this.with({ children: newNodes })
  }

  /** Return self, with new children added into its list of children.
   * All children MUST have the same asset type. */
  withHomogeneousChildrenInserted(
    children: readonly backendModule.AnySmartAsset[],
    directoryKey: backendModule.DirectoryId,
    directory: backendModule.SmartDirectory
  ): AssetTreeNode {
    const depth = this.depth + 1
    const typeOrder =
      children[0] != null ? backendModule.ASSET_TYPE_ORDER[children[0].value.type] : 0
    const nodes = (this.children ?? []).filter(
      node => node.item.value.type !== backendModule.AssetType.specialEmpty
    )
    const nodesToInsert = children.map(asset =>
      AssetTreeNode.fromSmartAsset(asset, directoryKey, directory, depth)
    )
    const newNodes = array.splicedBefore(
      nodes,
      nodesToInsert,
      innerItem => backendModule.ASSET_TYPE_ORDER[innerItem.item.value.type] >= typeOrder
    )
    return this.with({ children: newNodes })
  }

  /** Return self, with new children added into the children list of the given directory.
   * The children MAY be of different asset types. */
  withDescendantsInserted(
    assets: readonly backendModule.AnySmartAsset[],
    parentKey: backendModule.DirectoryId,
    parent: backendModule.SmartDirectory,
    getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null
  ) {
    return this.map(item =>
      item.key !== parentKey ? item : item.withChildrenInserted(assets, parentKey, parent, getKey)
    )
  }

  /** Return self, with new children added into the children list of the given directory.
   * All children MUST have the same asset type. */
  withHomogeneousDescendantsInserted(
    assets: readonly backendModule.AnySmartAsset[],
    parentKey: backendModule.DirectoryId,
    parent: backendModule.SmartDirectory
  ) {
    return this.map(item =>
      item.key !== parentKey
        ? item
        : item.withHomogeneousChildrenInserted(assets, parentKey, parent)
    )
  }
}
