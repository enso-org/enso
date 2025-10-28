/** @file Modal for dragging an asset. */
import { Badge } from '#/components/Badge'
import { DIALOG_BACKGROUND } from '#/components/Dialog'
import Portal from '#/components/Portal'
import { Underlay } from '#/components/Underlay'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { unsetModal } from '#/providers/ModalProvider'
import {
  Children,
  startTransition,
  useEffect,
  useRef,
  type DragEvent,
  type PropsWithChildren,
} from 'react'

/** The default offset (up and to the right) of the drag element. */
const DEFAULT_OFFSET_PX = 16

/** Props for a {@link DragModal}. */
export interface DragModalProps
  extends Readonly<PropsWithChildren>,
    Readonly<JSX.IntrinsicElements['div']> {
  readonly hideBadge?: boolean
  readonly event: DragEvent
  readonly onDragEnd: () => void
  readonly offsetPx?: number
  readonly offsetXPx?: number
  readonly offsetYPx?: number
}

/** A modal for dragging an asset. */
export default function DragModal(props: DragModalProps) {
  const {
    hideBadge = false,
    event,
    offsetPx,
    offsetXPx = DEFAULT_OFFSET_PX,
    offsetYPx = DEFAULT_OFFSET_PX,
    children,
    style,
    className,
    onDragEnd: onDragEndRaw,
    ...passthrough
  } = props
  const containerRef = useRef<HTMLDivElement>(null)
  const onDragEndOuter = useEventCallback(onDragEndRaw)

  const onDrag = useEventCallback((dragEvent: MouseEvent) => {
    if (!containerRef.current) return
    if (dragEvent.pageX !== 0 || dragEvent.pageY !== 0) {
      containerRef.current.style.left = `${dragEvent.pageX - (offsetPx ?? offsetXPx)}px`
      containerRef.current.style.top = `${dragEvent.pageY - (offsetPx ?? offsetYPx)}px`
    }
  })

  useEffect(() => {
    const onDragEnd = () => {
      startTransition(() => {
        onDragEndOuter()
        unsetModal()
      })
    }

    // Update position (non-FF)
    document.addEventListener('drag', onDrag, { capture: true })
    // Update position (FF)
    document.addEventListener('dragover', onDrag, { capture: true })

    document.addEventListener('dragend', onDragEnd, { capture: true })

    return () => {
      document.removeEventListener('drag', onDrag, { capture: true })
      document.removeEventListener('dragover', onDrag, { capture: true })
      document.removeEventListener('dragend', onDragEnd, { capture: true })
    }
  }, [onDragEndOuter, onDrag])

  return (
    <Portal>
      <div className="pointer-events-none absolute size-full overflow-hidden shadow-md">
        <div
          ref={containerRef}
          {...passthrough}
          style={{
            left: event.pageX - (offsetPx ?? offsetXPx),
            top: event.pageY - (offsetPx ?? offsetYPx),
            ...style,
          }}
          className={DIALOG_BACKGROUND({
            className: ['relative w-48 translate-x-3 translate-y-3', className],
          })}
        >
          <div className="absolute w-full">
            {Children.toArray(children)
              .slice(0, 3)
              .reverse()
              .map((child, index, array) => (
                <div
                  key={index}
                  className="absolute w-full rounded-4xl border-[0.5px] border-primary/10 bg-invert shadow-sm"
                  style={{ left: array.length - index * 3, top: array.length - index * 4 }}
                >
                  {child}
                </div>
              ))}
          </div>

          {!hideBadge && (
            <Underlay className="absolute -right-1 -top-3 rounded-full">
              <Badge color="primary">{Children.toArray(children).length}</Badge>
            </Underlay>
          )}
        </div>
      </div>
    </Portal>
  )
}
