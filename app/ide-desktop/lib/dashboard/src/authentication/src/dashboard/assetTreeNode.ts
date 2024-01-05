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
    root: AssetTreeNode,
    transform: (node: AssetTreeNode) => AssetTreeNode
) {
    const children = root.children ?? []
    let result: AssetTreeNode = transform(root)
    for (let i = 0; i < children.length; i += 1) {
        const node = children[i]
        if (node == null) {
            break
        }
        const intermediateNode = transform(node)
        let newNode: AssetTreeNode = intermediateNode
        if (intermediateNode.children != null) {
            newNode = assetTreeMap(intermediateNode, transform)
        }
        if (newNode !== node) {
            if (result === root) {
                result = { ...root }
                result.children = [...children]
            }
            result.children ??= []
            result.children[i] = newNode
        }
    }
    return result
}

/** Return a new {@link AssetTreeNode} array if any children would be changed by the transformation
 * function, otherwise return the original {@link AssetTreeNode} array. The predicate is applied to
 * a parent node before it is applied to its children. The root node is never removed. */
export function assetTreeFilter(root: AssetTreeNode, predicate: (node: AssetTreeNode) => boolean) {
    const children = root.children ?? []
    let result: AssetTreeNode | null = null
    for (let i = 0; i < children.length; i += 1) {
        const node = children[i]
        if (node == null) {
            break
        }
        if (!predicate(node)) {
            if (!result) {
                result = { ...root }
                result.children = i === 0 ? null : children.slice(0, i)
            }
        } else {
            let newNode = node
            if (node.children != null) {
                newNode = assetTreeFilter(node, predicate)
                if (newNode !== node) {
                    if (!result) {
                        result = { ...root }
                        result.children = children.slice(0, i)
                    }
                }
            }
            if (result != null) {
                result.children ??= []
                result.children.push(newNode)
            }
        }
    }
    if (result !== root && result?.children?.length === 0) {
        result.children = null
    }
    return result ?? root
}

/** Returns all items in the tree, flattened into an array using pre-order traversal. */
export function assetTreePreorderTraversal(
    root: AssetTreeNode,
    preprocess?: ((tree: AssetTreeNode[]) => AssetTreeNode[]) | null
): AssetTreeNode[] {
    return (preprocess?.(root.children ?? []) ?? root.children ?? []).flatMap(node =>
        node.children == null
            ? [node]
            : [node, ...assetTreePreorderTraversal(node, preprocess ?? null)]
    )
}

/** Creates an {@link AssetTreeNode} from a {@link backendModule.AnyAsset}. */
export function assetTreeNodeFromAsset(
    asset: backendModule.AnyAsset,
    directoryKey: backendModule.AssetId | null,
    directoryId: backendModule.DirectoryId | null,
    depth: number,
    getKey: ((asset: backendModule.AnyAsset) => backendModule.AssetId) | null = null
): AssetTreeNode {
    getKey ??= oldAsset => oldAsset.id
    return {
        key: getKey(asset),
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
