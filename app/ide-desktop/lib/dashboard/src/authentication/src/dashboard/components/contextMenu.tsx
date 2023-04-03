/** @file A context menu. */

import * as react from 'react'

export interface ContextMenuProps {
    event: react.MouseEvent
}

// This component MUST NOT use `useState` because it is not rendered directly.
function ContextMenu(props: react.PropsWithChildren<ContextMenuProps>) {
    const { children, event: event } = props
    return (
        <div
            style={{ left: event.pageX, top: event.pageY }}
            className="absolute bg-white rounded-lg shadow-soft flex flex-col flex-nowrap"
        >
            {children}
        </div>
    )
}

export default ContextMenu
