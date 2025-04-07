/** @file Barrel re-export of `react-aria` and `react-aria-components`. */
import { createHideableComponent } from '@react-aria/collections'
import type { Mutable } from 'enso-common/src/utilities/data/object'
import type { ForwardedRef } from 'react'
import { useContext } from 'react'
import * as aria from 'react-aria'
import { useFocusRing } from 'react-aria'
import {
  UNSTABLE_CollectionRendererContext as CollectionRendererContext,
  UNSTABLE_DefaultCollectionRenderer as DefaultCollectionRenderer,
  TabListStateContext,
  TabsContext,
  type TabPanelProps,
} from 'react-aria-components'
import invariant from 'tiny-invariant'

// eslint-disable-next-line react-refresh/only-export-components
export * from '@react-aria/interactions'
export { ClearPressResponder } from '@react-aria/interactions'
export type * from '@react-types/shared'
// eslint-disable-next-line react-refresh/only-export-components
export * from 'react-aria'
// @ts-expect-error The conflicting exports are props types ONLY
// eslint-disable-next-line react-refresh/only-export-components
export * from 'react-aria-components'
// Resolve ambigouous star exports (`react-aria` and `react-aria-components`)
export { I18nProvider, RouterProvider } from 'react-aria-components'
export {
  // eslint-disable-next-line react-refresh/only-export-components
  useTooltipTriggerState,
  type OverlayTriggerState,
  type TooltipTriggerState,
} from 'react-stately'

/**
 * Merges multiple props objects together.
 * Event handlers are chained, classNames are combined, and ids are deduplicated -
 * different ids will trigger a side-effect and re-render components hooked up with `useId`.
 * For all other props, the last prop object overrides all previous ones.
 *
 * The constraint is defaulted to `never` to make an explicit constraint mandatory.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function mergeProps<Constraint extends object = never>() {
  return <const T extends readonly (Partial<Constraint> | null | undefined)[]>(
    ...args: T & { [K in keyof T]: Pick<T[K], keyof Constraint & keyof T[K]> }
    // This is SAFE, as `args` is an intersection of `T` and another type.
    // eslint-disable-next-line no-restricted-syntax
  ) => aria.mergeProps<Mutable<T>>(...(args as T))
}

/**
 * A TabPanel provides the content for a tab.
 *
 * This component is a modified version of the `TabPanel` component from the
 * `react-aria-components` library. We use simplified solution that better
 * fits our needs and doesn't cause performance issues.
 * Original component causes layout recalculations on each render.
 * We removed the `useTabPanel` hook: https://github.com/adobe/react-spectrum/blob/main/packages/react-aria-components/src/Tabs.tsx#L293
 * !!! Make sure to keep this component in sync with the upstream component.
 */
// eslint-disable-next-line no-restricted-syntax
export const TabPanel = createHideableComponent(function TabPanel(
  props: TabPanelProps,
  ref: ForwardedRef<HTMLDivElement>,
) {
  const { shouldForceMount = false, children, className, style, ...rest } = props

  const state = useContext(TabListStateContext)

  invariant(state != null, 'TabPanel component cannot be used outside of a Tabs component')

  const { focusProps, isFocused, isFocusVisible } = useFocusRing()

  const isSelected = state.selectedKey === props.id

  if (!isSelected && !shouldForceMount) {
    return null
  }

  const domProps =
    isSelected ? mergeProps<React.HTMLAttributes<HTMLDivElement>>()(rest, focusProps) : {}

  const renderProps = {
    defaultClassName: '',
    defaultChildren: null,
    defaultStyle: {},
    isFocused,
    isFocusVisible,
    isInert: !isSelected,
    state,
  }

  return (
    <div
      {...domProps}
      className={typeof className === 'function' ? className(renderProps) : className}
      style={typeof style === 'function' ? style(renderProps) : style}
      ref={ref}
      data-inert={!isSelected ? 'true' : undefined}
    >
      <TabsContext.Provider value={null}>
        <TabListStateContext.Provider value={null}>
          <CollectionRendererContext.Provider value={DefaultCollectionRenderer}>
            {typeof children === 'function' ? children(renderProps) : children}
          </CollectionRendererContext.Provider>
        </TabListStateContext.Provider>
      </TabsContext.Provider>
    </div>
  )
})
