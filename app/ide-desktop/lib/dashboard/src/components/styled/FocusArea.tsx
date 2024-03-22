/** @file An area that contains focusable children. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import AreaFocusProvider from '#/providers/AreaFocusProvider'
import type * as focusDirectionProvider from '#/providers/FocusDirectionProvider'
import FocusDirectionProvider from '#/providers/FocusDirectionProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import * as aria from '#/components/aria'

// =================
// === FocusArea ===
// =================

/** Remove undefined from all values of an object. */
// eslint-disable-next-line no-restricted-syntax
interface FocusWithinProps {
  readonly onFocus: NonNullable<aria.DOMAttributes<Element>['onFocus']>
  readonly onBlur: NonNullable<aria.DOMAttributes<Element>['onBlur']>
}

/** Props for a {@link FocusAreaInner} */
export interface FocusAreaInnerProps {
  readonly active?: boolean
  readonly children: (
    ref: React.RefCallback<HTMLElement | SVGElement | null>,
    props: FocusWithinProps
  ) => JSX.Element
}

/** The inner component of {@link FocusArea}. This is required to use the
 * {@link aria.useFocusManager} hook. */
function FocusAreaInner(props: FocusAreaInnerProps) {
  const { active = true, children } = props
  const [areaFocus, setAreaFocus] = React.useState(false)
  const { focusWithinProps } = aria.useFocusWithin({ onFocusWithinChange: setAreaFocus })
  const focusManager = aria.useFocusManager()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const rootRef = React.useRef<HTMLElement | SVGElement | null>(null)

  React.useEffect(() => {
    if (!active || rootRef.current == null || focusManager == null) {
      return
    } else {
      return navigator2D.register(rootRef.current, {
        focusPrimaryChild: focusManager.focusFirst.bind(null),
      })
    }
  }, [active, focusManager, navigator2D])

  const cachedChildren = React.useMemo(
    () =>
      children(ref => {
        rootRef.current = ref
        if (ref != null && detect.IS_DEV_MODE) {
          if (active) {
            ref.dataset.focusArea = ''
          } else {
            delete ref.dataset.focusArea
          }
        }
        // This is REQUIRED, otherwise `useFocusWithin` does not work with
        // eslint-disable-next-line no-restricted-syntax
      }, focusWithinProps as FocusWithinProps),
    [active, children, focusWithinProps]
  )

  return <AreaFocusProvider areaFocus={areaFocus}>{cachedChildren}</AreaFocusProvider>
}

/** Props for a {@link FocusArea} */
export interface FocusAreaProps extends FocusAreaInnerProps {
  readonly direction: focusDirectionProvider.FocusDirection
}

/** An area that can be focused within. */
export default function FocusArea(props: FocusAreaProps) {
  const { direction, ...passthrough } = props

  return (
    <aria.FocusScope>
      <FocusDirectionProvider direction={direction}>
        <FocusAreaInner {...passthrough} />
      </FocusDirectionProvider>
    </aria.FocusScope>
  )
}
