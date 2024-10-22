/** @file An element that prevents navigation outside of itself. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import * as aria from '#/components/aria'
import * as withFocusScope from '#/components/styled/withFocusScope'

// =================
// === FocusRoot ===
// =================

/** Props passed to the inner handler of a {@link FocusRoot}. */
export interface FocusRootInnerProps {
  readonly ref: React.RefCallback<HTMLElement | SVGElement | null>
  readonly onKeyDown?: React.KeyboardEventHandler<HTMLElement>
}

/** Props for a {@link FocusRoot} */
export interface FocusRootProps {
  readonly active?: boolean
  readonly children: (props: FocusRootInnerProps) => React.JSX.Element
}

/** An element that prevents navigation outside of itself. */
function FocusRoot(props: FocusRootProps) {
  const { active = true, children } = props
  const navigator2D = navigator2DProvider.useNavigator2D()
  const cleanupRef = React.useRef(() => {})

  let isRealRun = !detect.IS_DEV_MODE
  React.useEffect(() => {
    return () => {
      if (isRealRun) {
        cleanupRef.current()
      }
      // This is INTENTIONAL. The first time this hook runs, when in Strict Mode, is *after* the ref
      // has already been set. This makes the focus root immediately unset itself,
      // which is incorrect behavior.
      // eslint-disable-next-line react-hooks/exhaustive-deps
      isRealRun = true
    }
  }, [])

  const cachedChildren = React.useMemo(
    () =>
      children({
        ref: (element) => {
          cleanupRef.current()
          if (active && element != null) {
            cleanupRef.current = navigator2D.pushFocusRoot(element)
          } else {
            cleanupRef.current = () => {}
          }
          if (element != null && detect.IS_DEV_MODE) {
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
      <aria.FocusScope contain restoreFocus autoFocus>
        {cachedChildren}
      </aria.FocusScope>
    )
}

/** An area that can be focused within. */
export default withFocusScope.withFocusScope(FocusRoot)
