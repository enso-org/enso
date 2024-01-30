/** @file A node in the drive's item tree. */
import * as React from 'react'

import * as backendModule from '#/services/backend'

// =====================
// === AssetTreeNode ===
// =====================

/** An {@link AssetTreeNode}, but excluding its methods. */
export interface AssetTreeNodeData
  extends Pick<
    AssetTreeNode,
    'children' | 'depth' | 'directory' | 'directoryKey' | 'item' | 'key'
  > {}

/** A node in the drive's item tree. */
export class AssetTreeNode {
  /** Create a {@link AssetTreeNode}. */
  constructor(
    /** The id of the asset (or the placeholder id for new assets). This must never change. */
    public readonly key: backendModule.AssetId,
    /** The actual asset. This MAY change if this is initially a placeholder item, but rows MAY
     * keep updated values within the row itself as well. */
    public item: backendModule.AnySmartAsset,
    /** The id of the asset's parent directory (or the placeholder id for new assets).
     * This must never change. */
    public readonly directoryKey: backendModule.AssetId,
    /** The actual id of the asset's parent directory (or the placeholder id for new assets). */
    public readonly directory: backendModule.SmartDirectory,
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
    return backendModule.compareAssets(a.item.value, b.item.value)
  }

  /** Creates an {@link AssetTreeNode} from a {@link backendModule.AnySmartAsset}. */
  static fromSmartAsset(
    this: void,
    smartAsset: backendModule.AnySmartAsset,
    directoryKey: backendModule.AssetId,
    directory: backendModule.SmartDirectory,
    depth: number,
    getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null
  ): AssetTreeNode {
    getKey ??= oldAsset => oldAsset.id
    return new AssetTreeNode(
      getKey(smartAsset.value),
      smartAsset,
      directoryKey,
      directory,
      null,
      depth
    )
  }

  /** Create a new {@link AssetTreeNode} with the specified properties updated. */
  with(update: Partial<AssetTreeNodeData>) {
    return new AssetTreeNode(
      update.key ?? this.key,
      update.item ?? this.item,
      update.directoryKey ?? this.directoryKey,
      update.directory ?? this.directory,
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

// ===================
// === useSetAsset ===
// ===================

/** Converts a React set state action for an {@link AssetTreeNode} to a set state action for any
 * subset of {@link backendModule.AnyAsset}. This is unsafe when `T` does not match the type of the
 * item contained in the `AssetTreeNode`, so this MUST be guarded by checking that the item is of
 * the correct type. A value of type `T` must be provided as the first parameter to ensure that this
 * has been done. */
export function useSetAsset<T extends backendModule.AnyAsset>(
  _value: T,
  setNode: React.Dispatch<React.SetStateAction<AssetTreeNode>>
) {
  return React.useCallback(
    (valueOrUpdater: React.SetStateAction<T>) => {
      setNode(oldNode => {
        const value =
          typeof valueOrUpdater === 'function'
            ? // This is SAFE, because it is a mistake for an item to change type.
              // eslint-disable-next-line no-restricted-syntax
              valueOrUpdater(oldNode.item.value as T)
            : valueOrUpdater
        // This is SAFE, because it is a mistake for an item to change type.
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, no-restricted-syntax, @typescript-eslint/no-explicit-any
        return oldNode.with({ item: oldNode.item.withValue(value as any) })
      })
    },
    [/* should never change */ setNode]
  )
}
