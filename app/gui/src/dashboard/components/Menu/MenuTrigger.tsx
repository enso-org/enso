/**
 * @file
 * A trigger for a menu that handles opening and closing the menu.
 */
import * as React from 'react'

import * as aria from 'react-aria-components'

/** Props for {@link MenuTrigger} */
export interface MenuTriggerProps extends Omit<aria.MenuTriggerProps, 'children'> {
  readonly children: [React.ReactElement, React.ReactElement]
}

/**
 * A trigger for a menu that handles opening and closing the menu.
 */
export function MenuTrigger(props: MenuTriggerProps) {
  const { children, ...triggerProps } = props

  const [trigger, menu] = children

  return (
    <aria.MenuTrigger {...triggerProps}>
      {trigger}
      {menu}
    </aria.MenuTrigger>
  )
}
