/** @file Barrel re-export of `react-aria` and `react-aria-components`. */
import { createHideableComponent } from '@react-aria/collections'
import type { ForwardedRef } from 'react'
import { useContext } from 'react'
import { useFocusRing } from 'react-aria'
import {
  UNSTABLE_CollectionRendererContext as CollectionRendererContext,
  UNSTABLE_DefaultCollectionRenderer as DefaultCollectionRenderer,
  TabListStateContext,
  TabsContext,
  type TabPanelProps,
} from 'react-aria-components'
import invariant from 'tiny-invariant'
import { mergeProps } from './aria'

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
