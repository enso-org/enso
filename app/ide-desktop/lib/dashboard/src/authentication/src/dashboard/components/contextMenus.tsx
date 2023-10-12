/** @file A context menu. */
import * as React from 'react'

import Modal from './modal'

// ===================
// === ContextMenu ===
// ===================

/** Props for a {@link ContextMenus}. */
export interface ContextMenusProps extends React.PropsWithChildren {
    hidden?: boolean
    key: string
    event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
}

/** A context menu that opens at the current mouse position. */
export default function ContextMenus(props: ContextMenusProps) {
    const { hidden = false, children, event } = props
    const contextMenuRef = React.useRef<HTMLDivElement>(null)
    const [left, setLeft] = React.useState(event.pageX)
    const [top, setTop] = React.useState(event.pageY)

    React.useLayoutEffect(() => {
        if (contextMenuRef.current != null) {
            setTop(Math.min(top, window.innerHeight - contextMenuRef.current.clientHeight))
            const boundingBox = contextMenuRef.current.getBoundingClientRect()
            setLeft(event.pageX - boundingBox.width / 2)
        }
    }, [children, top, event.pageX])

    return hidden ? (
        <>{children}</>
    ) : (
        <Modal
            className="absolute h-full w-full overflow-hidden bg-dim"
            onContextMenu={innerEvent => {
                innerEvent.preventDefault()
            }}
        >
            <div
                data-testid="context-menus"
                ref={contextMenuRef}
                style={{ left, top }}
                className="pointer-events-none sticky flex w-min items-start gap-0.5"
                onClick={clickEvent => {
                    clickEvent.stopPropagation()
                }}
            >
                {children}
            </div>
        </Modal>
    )
}
