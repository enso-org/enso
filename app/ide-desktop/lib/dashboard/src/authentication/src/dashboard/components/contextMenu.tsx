/** @file A right-click menu. */

// ===============
// === Context ===
// ===============

export interface ContextMenuContext {
    // FIXME[sb]: This should somehow pass down the innermost `ContextMenu`.
}

// =================
// === Component ===
// =================

export interface ContextMenuProps {
    items: JSX.Element[]
}

function ContextMenu() {
    return <div></div>
}

export default ContextMenu
