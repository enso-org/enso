/** @file A selection brush to indicate the area being selected by the mouse drag action. */
import * as React from 'react'

import * as animationHooks from '#/hooks/animationHooks'

import * as modalProvider from '#/providers/ModalProvider'

import Portal from '#/components/Portal'

import * as eventModule from '#/utilities/event'
import type * as geometry from '#/utilities/geometry'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

/**
 * Controls the speed of animation of the {@link SelectionBrush} when the
 * mouse is released and the selection brush collapses back to zero size.
 */
const ANIMATION_TIME_HORIZON = 60

// ======================
// === SelectionBrush ===
// ======================

/** Props for a {@link SelectionBrush}. */
export interface SelectionBrushProps {
  readonly targetRef: React.RefObject<HTMLElement>
  readonly margin?: number
  readonly onDrag: (rectangle: geometry.DetailedRectangle, event: MouseEvent) => void
  readonly onDragEnd: (event: MouseEvent) => void
  readonly onDragCancel: () => void
}

/** A selection brush to indicate the area being selected by the mouse drag action. */
export default function SelectionBrush(props: SelectionBrushProps) {
  const { targetRef, margin = 0, onDrag, onDragEnd, onDragCancel } = props
  const { modalRef } = modalProvider.useModalRef()
  const isMouseDownRef = React.useRef(false)
  const didMoveWhileDraggingRef = React.useRef(false)
  const onDragRef = React.useRef(onDrag)
  onDragRef.current = onDrag
  const onDragEndRef = React.useRef(onDragEnd)
  onDragEndRef.current = onDragEnd
  const onDragCancelRef = React.useRef(onDragCancel)
  onDragCancelRef.current = onDragCancel
  const lastMouseEvent = React.useRef<MouseEvent | null>(null)
  const parentBounds = React.useRef<DOMRect | null>(null)
  const anchorRef = React.useRef<geometry.Coordinate2D | null>(null)
  const [anchor, setAnchor] = React.useState<geometry.Coordinate2D | null>(null)
  const [position, setPosition] = React.useState<geometry.Coordinate2D | null>(null)
  const [lastSetAnchor, setLastSetAnchor] = React.useState<geometry.Coordinate2D | null>(null)
  const anchorAnimFactor = animationHooks.useApproach(
    anchor != null ? 1 : 0,
    ANIMATION_TIME_HORIZON,
  )
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
    const isEventInBounds = (event: MouseEvent, parent?: HTMLElement | null) => {
      if (parent == null) {
        return true
      } else {
        parentBounds.current = parent.getBoundingClientRect()
        return eventModule.isElementInBounds(event, parentBounds.current, margin)
      }
    }
    const unsetAnchor = () => {
      if (anchorRef.current != null) {
        anchorRef.current = null
        setAnchor(null)
      }
    }
    const onMouseDown = (event: MouseEvent) => {
      if (
        modalRef.current == null &&
        !eventModule.isElementTextInput(event.target) &&
        !(event.target instanceof HTMLButtonElement) &&
        !(event.target instanceof HTMLAnchorElement) &&
        isEventInBounds(event, targetRef.current)
      ) {
        isMouseDownRef.current = true
        didMoveWhileDraggingRef.current = false
        lastMouseEvent.current = event
        const newAnchor = { left: event.pageX, top: event.pageY }
        anchorRef.current = newAnchor
        setAnchor(newAnchor)
        setLastSetAnchor(newAnchor)
        setPosition(newAnchor)
      }
    }
    const onMouseUp = (event: MouseEvent) => {
      if (didMoveWhileDraggingRef.current) {
        onDragEndRef.current(event)
      }
      // The `setTimeout` is required, otherwise the values are changed before the `onClick` handler
      // is executed.
      window.setTimeout(() => {
        isMouseDownRef.current = false
        didMoveWhileDraggingRef.current = false
      })
      unsetAnchor()
    }
    const onMouseMove = (event: MouseEvent) => {
      if (!(event.buttons & 1)) {
        isMouseDownRef.current = false
      }
      if (isMouseDownRef.current) {
        // Left click is being held.
        didMoveWhileDraggingRef.current = true
        lastMouseEvent.current = event
        const positionLeft =
          parentBounds.current == null ?
            event.pageX
          : Math.max(
              parentBounds.current.left - margin,
              Math.min(parentBounds.current.right + margin, event.pageX),
            )
        const positionTop =
          parentBounds.current == null ?
            event.pageY
          : Math.max(
              parentBounds.current.top - margin,
              Math.min(parentBounds.current.bottom + margin, event.pageY),
            )
        setPosition({ left: positionLeft, top: positionTop })
      }
    }
    const onClick = (event: MouseEvent) => {
      if (isMouseDownRef.current && didMoveWhileDraggingRef.current) {
        event.stopImmediatePropagation()
      }
    }
    const onDragStart = () => {
      if (isMouseDownRef.current) {
        isMouseDownRef.current = false
        onDragCancelRef.current()
        unsetAnchor()
      }
    }

    document.addEventListener('mousedown', onMouseDown)
    document.addEventListener('mouseup', onMouseUp)
    document.addEventListener('dragstart', onDragStart, { capture: true })
    document.addEventListener('mousemove', onMouseMove)
    document.addEventListener('click', onClick, { capture: true })
    return () => {
      document.removeEventListener('mousedown', onMouseDown)
      document.removeEventListener('mouseup', onMouseUp)
      document.removeEventListener('dragstart', onDragStart, { capture: true })
      document.removeEventListener('mousemove', onMouseMove)
      document.removeEventListener('click', onClick, { capture: true })
    }
  }, [margin, targetRef, modalRef])

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
    if (selectionRectangle != null && lastMouseEvent.current != null) {
      onDragRef.current(selectionRectangle, lastMouseEvent.current)
    }
  }, [selectionRectangle])

  const brushStyle =
    rectangle == null ?
      {}
    : {
        left: `${rectangle.left}px`,
        top: `${rectangle.top}px`,
        width: `${rectangle.width}px`,
        height: `${rectangle.height}px`,
      }
  return (
    <Portal>
      <div
        className={tailwindMerge.twMerge(
          'pointer-events-none fixed z-1 box-content rounded-selection-brush border-transparent bg-selection-brush transition-border-margin',
          hidden ? 'm border-0' : '-m-selection-brush-border border-selection-brush',
        )}
        style={brushStyle}
      />
    </Portal>
  )
}
