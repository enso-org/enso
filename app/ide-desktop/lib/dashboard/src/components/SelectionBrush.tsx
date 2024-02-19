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
  readonly onChange: (rectangle: geometry.Rectangle | null) => void
}

/** A selection brush to indicate the area being selected by the mouse drag action. */
export default function SelectionBrush(props: SelectionBrushProps) {
  const { onChange } = props
  const isMouseDown = React.useRef(false)
  const [anchor, setAnchor] = React.useState<geometry.Coordinate2D | null>(null)
  // This will be `null` if `anchor` is `null`.
  const [position, setPosition] = React.useState<geometry.Coordinate2D | null>(null)
  const [lastSetAnchor, setLastSetAnchor] = React.useState<geometry.Coordinate2D | null>(null)
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  const anchorAnimFactor = animationHooks.useApproach(anchor != null ? 1 : 0, 60)
  const hidden = anchor == null

  React.useEffect(() => {
    if (anchor != null) {
      anchorAnimFactor.skip()
    }
  }, [anchorAnimFactor, anchor])

  React.useEffect(() => {
    const onMouseDown = (event: MouseEvent) => {
      isMouseDown.current = true
      const newAnchor = { left: event.pageX, top: event.pageY }
      setAnchor(newAnchor)
      setLastSetAnchor(newAnchor)
      setPosition(newAnchor)
    }
    const onMouseUp = () => {
      isMouseDown.current = false
      setAnchor(null)
    }
    const onMouseMove = (event: MouseEvent) => {
      if (isMouseDown.current) {
        setPosition({ left: event.pageX, top: event.pageY })
      }
    }
    document.addEventListener('mousedown', onMouseDown)
    document.addEventListener('mouseup', onMouseUp)
    document.addEventListener('dragstart', onMouseUp, { capture: true })
    document.addEventListener('mousemove', onMouseMove)
    return () => {
      document.removeEventListener('mousedown', onMouseDown)
      document.removeEventListener('mouseup', onMouseUp)
      document.removeEventListener('dragstart', onMouseUp, { capture: true })
      document.removeEventListener('mousemove', onMouseMove)
    }
  }, [])

  const rectangle = React.useMemo(() => {
    if (position != null && lastSetAnchor != null) {
      const end: geometry.Coordinate2D = {
        left:
          position.left * (1 - anchorAnimFactor.value) +
          lastSetAnchor.left * anchorAnimFactor.value,
        top:
          position.top * (1 - anchorAnimFactor.value) + lastSetAnchor.top * anchorAnimFactor.value,
      }
      return {
        left: Math.min(position.left, end.left),
        top: Math.min(position.top, end.top),
        width: Math.abs(position.left - end.left),
        height: Math.abs(position.top - end.top),
      }
      // onChange(rectangle)
    } else {
      return null
    }
  }, [anchorAnimFactor.value, lastSetAnchor, position])

  React.useEffect(() => {
    onChange(rectangle)
    // `onChange` is a callback, not a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [rectangle])

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
