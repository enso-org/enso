/** @file An element that prevents navigation outside of itself. */
import { FocusScope } from '#/components/aria'
import { withFocusScope } from '#/components/styled/withFocusScope'
import { useNavigator2D } from '#/providers/Navigator2DProvider'
import { IS_DEV_MODE } from 'enso-common/src/detect'
import { useMemo, useRef, type JSX, type KeyboardEventHandler, type RefCallback } from 'react'

/** Props passed to the inner handler of a {@link FocusRoot}. */
export interface FocusRootInnerProps {
  readonly ref: RefCallback<HTMLElement | SVGElement | null>
  readonly onKeyDown?: KeyboardEventHandler<HTMLElement>
}

/** Props for a {@link FocusRoot} */
export interface FocusRootProps {
  readonly active?: boolean
  readonly children: (props: FocusRootInnerProps) => JSX.Element
}

/** An element that prevents navigation outside of itself. */
function FocusRootInternal(props: FocusRootProps) {
  const { active = true, children } = props
  const navigator2D = useNavigator2D()
  const cleanupRef = useRef(() => {})

  const cachedChildren = useMemo(
    () =>
      children({
        ref: (element) => {
          cleanupRef.current()
          if (active && element != null) {
            cleanupRef.current = navigator2D.pushFocusRoot(element)
          } else {
            cleanupRef.current = () => {}
          }
          if (element != null && IS_DEV_MODE) {
            if (active) {
              element.dataset.focusRoot = ''
            } else {
              delete element.dataset.focusRoot
            }
          }
        },
        ...(active && {
          onKeyDown: (event) => {
            navigator2D.onKeyDown(event)
          },
        }),
      }),
    [active, children, navigator2D],
  )

  return !active ? cachedChildren : (
      <FocusScope contain restoreFocus autoFocus>
        {cachedChildren}
      </FocusScope>
    )
}

/** An area that can be focused within. */
// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
export const FocusRoot = withFocusScope(FocusRootInternal)
