/** @file DOM utilities. */

// ==================
// === HTML Utils ===
// ==================

/** Creates a new top-level div which occupies the full size of its parent. */
export function newTopLevelDiv(): HTMLDivElement {
  const node = document.createElement('div')
  node.style.width = '100%'
  node.style.height = '100%'
  document.body.appendChild(node)
  return node
}

/** Disable the context menu for the whole browser window. */
export function disableContextMenu() {
  document.body.addEventListener('contextmenu', (event) => event.preventDefault())
}
