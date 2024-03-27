/** @file An area that contains focusable children. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import AreaFocusProvider from '#/providers/AreaFocusProvider'
import type * as focusDirectionProvider from '#/providers/FocusDirectionProvider'
import FocusDirectionProvider from '#/providers/FocusDirectionProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import * as aria from '#/components/aria'
import * as withFocusScope from '#/components/styled/withFocusScope'

// =================
// === FocusArea ===
// =================

/** Props returned by {@link aria.useFocusWithin}. */
export interface FocusWithinProps {
  readonly onFocus: NonNullable<aria.DOMAttributes<Element>['onFocus']>
  readonly onBlur: NonNullable<aria.DOMAttributes<Element>['onBlur']>
}

/** Props for a {@link FocusArea} */
export interface FocusAreaProps {
  readonly active?: boolean
  readonly direction: focusDirectionProvider.FocusDirection
  readonly children: (
    ref: React.RefCallback<HTMLElement | SVGElement | null>,
    props: FocusWithinProps
  ) => JSX.Element
}

/** An area that can be focused within. */
function FocusArea(props: FocusAreaProps) {
  const { active = true, direction, children } = props
  const [areaFocus, setAreaFocus] = React.useState(false)
  const { focusWithinProps } = aria.useFocusWithin({ onFocusWithinChange: setAreaFocus })
  const focusManager = aria.useFocusManager()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const rootRef = React.useRef<HTMLElement | SVGElement | null>(null)
  const cleanupRef = React.useRef(() => {})

  let isRealRun = !detect.IS_DEV_MODE
  React.useEffect(() => {
    return () => {
      if (isRealRun) {
        cleanupRef.current()
      }
      // This is INTENTIONAL. It may not be causing problems now, but is a defensive measure
      // to make the implementation of this function consistent with the implementation of
      // `FocusRoot`.
      // eslint-disable-next-line react-hooks/exhaustive-deps
      isRealRun = true
    }
  }, [])

  const cachedChildren = React.useMemo(
    () =>
      children(element => {
        rootRef.current = element
        cleanupRef.current()
        if (active && element != null && focusManager != null) {
          const focusFirst = focusManager.focusFirst.bind(null, {
            accept: other => other.classList.contains('focus-child'),
          })
          const focusLast = focusManager.focusLast.bind(null, {
            accept: other => other.classList.contains('focus-child'),
          })
          const focusCurrent = () =>
            focusManager.focusFirst({
              accept: other => other.classList.contains('focus-default'),
            }) ?? focusFirst()
          cleanupRef.current = navigator2D.register(element, {
            focusPrimaryChild: focusCurrent,
            focusWhenPressed:
              direction === 'horizontal'
                ? { right: focusFirst, left: focusLast }
                : { down: focusFirst, up: focusLast },
          })
        } else {
          cleanupRef.current = () => {}
        }
        if (element != null && detect.IS_DEV_MODE) {
          if (active) {
            element.dataset.focusArea = ''
          } else {
            delete element.dataset.focusArea
          }
        }
        // This is REQUIRED, otherwise `useFocusWithin` does not work with components from
        // `react-aria-components`.
        // eslint-disable-next-line no-restricted-syntax
      }, focusWithinProps as FocusWithinProps),
    [active, direction, children, focusManager, focusWithinProps, navigator2D]
  )

  return (
    <FocusDirectionProvider direction={direction}>
      <AreaFocusProvider areaFocus={areaFocus}>{cachedChildren}</AreaFocusProvider>
    </FocusDirectionProvider>
  )
}

/** An area that can be focused within. */
export default withFocusScope.withFocusScope(FocusArea)
