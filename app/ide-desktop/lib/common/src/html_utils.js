// ==================
// === HTML Utils ===
// ==================

/// Remove the given node if it exists.
export function remove_node(node) {
    if (node) {
        node.parentNode.removeChild(node)
    }
}

/// Creates a new top-level div which occupy full size of its parent's space.
export function new_top_level_div() {
    let node = document.createElement('div')
    node.style.width = '100%'
    node.style.height = '100%'
    document.body.appendChild(node)
    return node
}

/// Log subsequent messages in a group.
export async function log_group_collapsed(msg, f) {
    console.groupCollapsed(msg)
    let out
    try {
        out = await f()
    } catch (error) {
        console.groupEnd()
        throw error
    }
    console.groupEnd()
    return out
}
