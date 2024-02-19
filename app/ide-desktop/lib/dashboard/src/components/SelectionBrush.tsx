/** @file A selection brush to indicate the area being selected by the mouse drag action. */
import * as React from 'react'

import * as reactDom from 'react-dom'

import * as animationHooks from '#/hooks/animationHooks'

import type * as geometry from '#/utilities/geometry'

// ======================
// === SelectionBrush ===
// ======================

/** Props for a {@link SelectionBrush}. */
export interface SelectionBrushProps {
  readonly onChange: (rectangle: geometry.DetailedRectangle | null, event: MouseEvent) => void
}

/** A selection brush to indicate the area being selected by the mouse drag action. */
export default function SelectionBrush(props: SelectionBrushProps) {
  const { onChange } = props
  const isMouseDown = React.useRef(false)
  const didMoveWhileDragging = React.useRef(false)
  const lastMouseEvent = React.useRef<MouseEvent | null>(null)
  const [anchor, setAnchor] = React.useState<geometry.Coordinate2D | null>(null)
  // This will be `null` if `anchor` is `null`.
  const [position, setPosition] = React.useState<geometry.Coordinate2D | null>(null)
  const [lastSetAnchor, setLastSetAnchor] = React.useState<geometry.Coordinate2D | null>(null)
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  const anchorAnimFactor = animationHooks.useApproach(anchor != null ? 1 : 0, 60)
  const hidden =
    anchor == null ||
    position == null ||
    (anchor.left === position.left && anchor.top === position.top)

  React.useEffect(() => {
    if (anchor != null) {
      anchorAnimFactor.skip()
    }
  }, [anchorAnimFactor, anchor])

  React.useEffect(() => {
    const onMouseDown = (event: MouseEvent) => {
      isMouseDown.current = true
      didMoveWhileDragging.current = false
      lastMouseEvent.current = event
      const newAnchor = { left: event.pageX, top: event.pageY }
      setAnchor(newAnchor)
      setLastSetAnchor(newAnchor)
      setPosition(newAnchor)
    }
    const onMouseUp = () => {
      // This is required, otherwise the values are changed before the `onClick` handler is
      // executed.
      window.setTimeout(() => {
        isMouseDown.current = false
        didMoveWhileDragging.current = false
      })
      setAnchor(null)
    }
    const onMouseMove = (event: MouseEvent) => {
      if (isMouseDown.current) {
        didMoveWhileDragging.current = true
        lastMouseEvent.current = event
        setPosition({ left: event.pageX, top: event.pageY })
      }
    }
    const onClick = (event: MouseEvent) => {
      if (isMouseDown.current && didMoveWhileDragging.current) {
        event.stopImmediatePropagation()
      }
    }
    document.addEventListener('mousedown', onMouseDown)
    document.addEventListener('mouseup', onMouseUp)
    document.addEventListener('dragstart', onMouseUp, { capture: true })
    document.addEventListener('mousemove', onMouseMove)
    document.addEventListener('click', onClick)
    return () => {
      document.removeEventListener('mousedown', onMouseDown)
      document.removeEventListener('mouseup', onMouseUp)
      document.removeEventListener('dragstart', onMouseUp, { capture: true })
      document.removeEventListener('mousemove', onMouseMove)
      document.removeEventListener('click', onClick)
    }
  }, [])

  const rectangle = React.useMemo(() => {
    if (position != null && lastSetAnchor != null) {
      const start: geometry.Coordinate2D = {
        left:
          position.left * (1 - anchorAnimFactor.value) +
          lastSetAnchor.left * anchorAnimFactor.value,
        top:
          position.top * (1 - anchorAnimFactor.value) + lastSetAnchor.top * anchorAnimFactor.value,
      }
      return {
        left: Math.min(position.left, start.left),
        top: Math.min(position.top, start.top),
        right: Math.max(position.left, start.left),
        bottom: Math.max(position.top, start.top),
        width: Math.abs(position.left - start.left),
        height: Math.abs(position.top - start.top),
        signedWidth: position.left - start.left,
        signedHeight: position.top - start.top,
      }
    } else {
      return null
    }
  }, [anchorAnimFactor.value, lastSetAnchor, position])

  const selectionRectangle = React.useMemo(() => (hidden ? null : rectangle), [hidden, rectangle])

  React.useEffect(() => {
    if (lastMouseEvent.current != null) {
      onChange(selectionRectangle, lastMouseEvent.current)
    }
    // `onChange` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [selectionRectangle])

  const brushStyle =
    rectangle == null
      ? {}
      : {
          left: `${rectangle.left}px`,
          top: `${rectangle.top}px`,
          width: `${rectangle.width}px`,
          height: `${rectangle.height}px`,
        }

  return reactDom.createPortal(
    <div
      className={`fixed bg-selection-brush pointer-events-none box-content rounded-lg border-transparent z-1 transition-border-margin duration-100 ${
        hidden ? 'border-0 m-0' : 'border-6 -m-1.5'
      }`}
      style={brushStyle}
    />,
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    document.getElementById('enso-dashboard')!
  )
}
