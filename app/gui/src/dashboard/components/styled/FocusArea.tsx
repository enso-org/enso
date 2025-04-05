/** @file An area that contains focusable children. */
import { type JSX, type RefCallback, useMemo, useRef, useState } from 'react'

import { IS_DEV_MODE } from 'enso-common/src/detect'

import AreaFocusProvider from '#/providers/AreaFocusProvider'
import FocusClassesProvider, { useFocusClasses } from '#/providers/FocusClassProvider'
import type { FocusDirection } from '#/providers/FocusDirectionProvider'
import FocusDirectionProvider from '#/providers/FocusDirectionProvider'

import { type DOMAttributes, useFocusWithin } from '#/components/aria'
import { withFocusScope } from '#/components/styled/withFocusScope'
import { useEventCallback } from '#/hooks/eventCallbackHooks'

/** Props returned by {@link useFocusWithin}. */
export interface FocusWithinProps {
  readonly ref: RefCallback<HTMLElement | SVGElement | null>
  readonly onFocus: NonNullable<DOMAttributes<Element>['onFocus']>
  readonly onBlur: NonNullable<DOMAttributes<Element>['onBlur']>
}

/** Props for a {@link FocusArea} */
export interface FocusAreaProps {
  /** Should ONLY be passed in exceptional cases. */
  readonly focusChildClass?: string
  /** Should ONLY be passed in exceptional cases. */
  readonly focusDefaultClass?: string
  readonly active?: boolean
  readonly direction: FocusDirection
  readonly children: (props: FocusWithinProps) => JSX.Element
}

/**
 * An area that can be focused within.
 * This component is mostly useless now, but it's fine -
 * in the most of cases we have proper keyboard navigation without it.
 */
// eslint-disable-next-line react-refresh/only-export-components
function FocusArea(props: FocusAreaProps) {
  const { active = true, direction, children } = props
  const { focusChildClass = 'focus-child' } = props
  const { focusChildClass: outerFocusChildClass } = useFocusClasses()
  const [areaFocus, setAreaFocus] = useState(false)

  const onChangeFocusWithin = useEventCallback((value: boolean) => {
    if (value === areaFocus) return
    setAreaFocus(value)
  })

  const { focusWithinProps } = useFocusWithin({ onFocusWithinChange: onChangeFocusWithin })
  const rootRef = useRef<HTMLElement | SVGElement | null>(null)
  const cleanupRef = useRef(() => {})

  // The following group of functions are for suppressing `react-compiler` lints.
  const cleanup = useEventCallback(() => {
    cleanupRef.current()
  })
  const setRootRef = useEventCallback((value: HTMLElement | SVGElement | null) => {
    rootRef.current = value
  })

  const cachedChildren = useMemo(
    () =>
      // This is REQUIRED, otherwise `useFocusWithin` does not work with components from
      // `react-aria-components`.
      // eslint-disable-next-line no-restricted-syntax
      children({
        ref: (element) => {
          setRootRef(element)
          cleanup()
          if (element != null && IS_DEV_MODE) {
            if (active) {
              element.dataset.focusArea = ''
            } else {
              delete element.dataset.focusArea
            }
          }
        },
        ...focusWithinProps,
      } as FocusWithinProps),
    [active, children, cleanup, focusWithinProps, setRootRef],
  )

  const result = (
    <FocusDirectionProvider direction={direction}>
      <AreaFocusProvider areaFocus={areaFocus}>{cachedChildren}</AreaFocusProvider>
    </FocusDirectionProvider>
  )
  return focusChildClass === outerFocusChildClass ? result : (
      <FocusClassesProvider focusChildClass={focusChildClass}>{result}</FocusClassesProvider>
    )
}

/** An area that can be focused within. */
// eslint-disable-next-line react-refresh/only-export-components
export default withFocusScope(FocusArea)
