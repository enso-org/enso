/** @file A node in the drive's item tree. */
import * as React from 'react'

import * as backendModule from './backend'

// =====================
// === AssetTreeNode ===
// =====================

/** A node in the drive's item tree. */
export interface AssetTreeNode {
    /** The id of the asset (or the placeholder id for new assets). This must never change. */
    key: backendModule.AssetId
    /** The actual asset. This MAY change if this is initially a placeholder item, but rows MAY
     * keep updated values within the row itself as well. */
    item: backendModule.AnyAsset
    /** The id of the asset's parent directory (or the placeholder id for new assets).
     * This must never change. */
    directoryKey: backendModule.AssetId | null
    /** The actual id of the asset's parent directory (or the placeholder id for new assets). */
    directoryId: backendModule.DirectoryId | null
    /** This is `null` if the asset is not a directory asset, OR if it is a collapsed directory
     * asset. */
    children: AssetTreeNode[] | null
    depth: number
}

/** Get an {@link AssetTreeNode.key} from an {@link AssetTreeNode}. Useful for React, references
 * of global functions do not change. */
export function getAssetTreeNodeKey(node: AssetTreeNode) {
    return node.key
}

/** Return a positive number if `a > b`, a negative number if `a < b`, and zero if `a === b`.
 * Uses {@link backendModule.compareAssets} internally. */
export function compareAssetTreeNodes(a: AssetTreeNode, b: AssetTreeNode) {
    return backendModule.compareAssets(a.item, b.item)
}

/** Return a new {@link AssetTreeNode} array if any children would be changed by the transformation
 * function, otherwise return the original {@link AssetTreeNode} array. */
export function assetTreeMap(
    tree: AssetTreeNode[],
    transform: (node: AssetTreeNode) => AssetTreeNode
) {
    let result: AssetTreeNode[] | null = null
    for (let i = 0; i < tree.length; i += 1) {
        const node = tree[i]
        if (node == null) {
            break
        }
        const intermediateNode = transform(node)
        let newNode: AssetTreeNode = intermediateNode
        if (intermediateNode.children != null) {
            const newChildren = assetTreeMap(intermediateNode.children, transform)
            if (newChildren !== intermediateNode.children) {
                newNode = { ...intermediateNode, children: newChildren }
            }
        }
        if (newNode !== node) {
            result ??= Array.from(tree)
            result[i] = newNode
        }
    }
    return result ?? tree
}

/** Return a new {@link AssetTreeNode} array if any children would be changed by the transformation
 * function, otherwise return the original {@link AssetTreeNode} array. The predicate is applied to
 * a parent node before it is applied to its children. */
export function assetTreeFilter(
    tree: AssetTreeNode[],
    predicate: (node: AssetTreeNode) => boolean
) {
    let result: AssetTreeNode[] | null = null
    for (let i = 0; i < tree.length; i += 1) {
        const node = tree[i]
        if (node == null) {
            break
        }
        if (!predicate(node)) {
            result = tree.slice(0, i)
        } else {
            if (node.children != null) {
                const newChildren = assetTreeFilter(node.children, predicate)
                if (newChildren !== node.children) {
                    result ??= tree.slice(0, i)
                    const newNode = {
                        ...node,
                        children: newChildren.length === 0 ? null : newChildren,
                    }
                    result.push(newNode)
                } else if (result != null) {
                    result.push(node)
                }
            } else if (result != null) {
                result.push(node)
            }
        }
    }
    return result ?? tree
}

/** Returns all items in the tree, flattened into an array using pre-order traversal. */
export function assetTreePreorderTraversal(
    tree: AssetTreeNode[],
    preprocess?: ((tree: AssetTreeNode[]) => AssetTreeNode[]) | null
): AssetTreeNode[] {
    return (preprocess?.(tree) ?? tree).flatMap(node => {
        if (node.children != null) {
            return [node, ...assetTreePreorderTraversal(node.children, preprocess ?? null)]
        } else {
            return [node]
        }
    })
}

/** Creates an {@link AssetTreeNode} from a {@link backendModule.AnyAsset}. */
export function assetTreeNodeFromAsset(
    asset: backendModule.AnyAsset,
    directoryKey: backendModule.AssetId | null,
    directoryId: backendModule.DirectoryId | null,
    depth: number
): AssetTreeNode {
    return {
        key: asset.id,
        item: asset,
        directoryKey,
        directoryId,
        children: null,
        depth,
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
                const item =
                    typeof valueOrUpdater === 'function'
                        ? // This is SAFE, because it is a mistake for an item to change type.
                          // eslint-disable-next-line no-restricted-syntax
                          valueOrUpdater(oldNode.item as T)
                        : valueOrUpdater
                return { ...oldNode, item }
            })
        },
        [/* should never change */ setNode]
    )
}
