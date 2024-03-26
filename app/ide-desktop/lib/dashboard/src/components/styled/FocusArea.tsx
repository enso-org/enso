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
  const unregisterNavigatorRef = React.useRef(() => {})

  React.useEffect(() => {
    if (!active || rootRef.current == null || focusManager == null) {
      return
    } else {
      unregisterNavigatorRef.current = navigator2D.register(rootRef.current, {
        focusPrimaryChild: focusManager.focusFirst.bind(null),
      })
      return () => {
        unregisterNavigatorRef.current()
      }
    }
  }, [active, focusManager, navigator2D])

  const cachedChildren = React.useMemo(
    () =>
      children(element => {
        rootRef.current = element
        unregisterNavigatorRef.current()
        if (element != null && focusManager != null) {
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
          unregisterNavigatorRef.current = navigator2D.register(element, {
            focusPrimaryChild: focusCurrent,
            focusWhenPressed:
              direction === 'horizontal'
                ? { right: focusFirst, left: focusLast }
                : { down: focusFirst, up: focusLast },
          })
        } else {
          unregisterNavigatorRef.current = () => {}
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
