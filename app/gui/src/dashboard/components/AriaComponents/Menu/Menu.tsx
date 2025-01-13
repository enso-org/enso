/**
 * @file
 * A menu displays a list of actions or options that a user can choose.
 */
import * as React from 'react'

import { createHideableComponent } from '@react-aria/collections'
import * as aria from 'react-aria-components'

import { tv, type VariantProps } from '#/utilities/tailwindVariants'

import { memo, useId } from 'react'
import { AnimatedBackground } from '../../AnimatedBackground'
import { Popover } from '../Dialog'
import { Separator, SEPARATOR_STYLES, type SeparatorProps } from '../Separator'
import { Text } from '../Text'
import type { Placement } from '../types'
import { MenuItem } from './MenuItem'
import { MenuTrigger } from './MenuTrigger'

export const MENU_STYLES = tv({
  base: 'flex flex-col [clip-path:inset(0_0_0_0_round_.75rem)] overflow-x-hidden',
  slots: {
    popover: 'min-w-[200px] w-auto max-w-[300px]',
  },
})

export const MENU_SECTION_STYLES = tv({
  base: 'flex flex-col',
  slots: {
    header: 'px-3.5 py-0.5',
  },
})

export const MENU_SEPARATOR_STYLES = tv({
  extend: SEPARATOR_STYLES,
  base: 'my-1.5 mx-2',
})

/** Props for {@link Menu} */
export interface MenuProps<T extends object>
  extends aria.MenuProps<T>,
    VariantProps<typeof MENU_STYLES> {
  readonly variant?: 'dark' | 'light'
  readonly className?: string
  readonly placement?: Placement
}

/** Props for {@link MenuSection} */
export interface MenuSectionProps<T extends object>
  extends aria.SectionProps<T>,
    VariantProps<typeof MENU_SECTION_STYLES> {
  readonly title: string
  readonly className?: string
}

/**
 * A menu displays a list of actions or options that a user can choose.
 */
// `createHideableComponent` wrapper is a workaround for a bug in react-aria-components,
//  when you can't display a menu inside a tabs component.
// see: https://github.com/adobe/react-spectrum/issues/6885
// eslint-disable-next-line no-restricted-syntax
export const Menu = createHideableComponent(function Menu<T extends object>(props: MenuProps<T>) {
  const {
    variant,
    className,
    children,
    placement = 'bottom start',
    variants = MENU_STYLES,
    ...menuProps
  } = props

  const styles = variants()

  return (
    <Popover
      variant={variant}
      placement={placement}
      className={styles.popover()}
      size="xxsmall"
      rounded="xxxlarge"
    >
      {() => (
        <AnimatedBackground>
          <aria.Menu<T> className={styles.base({ className })} {...menuProps}>
            {children}
          </aria.Menu>
        </AnimatedBackground>
      )}
    </Popover>
  )
}) as (<T extends object>(props: MenuProps<T>) => React.ReactElement) & {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Item: typeof MenuItem
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Section: typeof MenuSection
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Trigger: typeof MenuTrigger
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Separator: typeof MenuSeparator
  // eslint-disable-next-line @typescript-eslint/naming-convention
  SubmenuTrigger: typeof aria.SubmenuTrigger
}

/**
 * A section within a menu.
 */
export function MenuSection<T extends object>(props: MenuSectionProps<T>) {
  const { className, title, variants = MENU_SECTION_STYLES, ...sectionProps } = props

  const styles = variants()
  const id = useId()

  return (
    <>
      <aria.Header id={id} className={styles.header()}>
        <Text className="block" variant="body-sm" weight="bold" color="muted" textSelection="none">
          {title}
        </Text>
      </aria.Header>

      <aria.MenuSection
        aria-label={title}
        className={styles.base({ className })}
        {...sectionProps}
      />
    </>
  )
}

/** Props for {@link MenuSeparator} */
export interface MenuSeparatorProps
  extends SeparatorProps,
    VariantProps<typeof MENU_SEPARATOR_STYLES> {}

/**
 * A separator in a menu.
 */
// eslint-disable-next-line no-restricted-syntax
export const MenuSeparator = memo(function MenuSeparator(props: MenuSeparatorProps) {
  const { variants = MENU_SEPARATOR_STYLES, ...rest } = props

  return <Separator size="thin" variants={variants} {...rest} />
})

Menu.Item = MenuItem
Menu.Section = MenuSection
Menu.Trigger = MenuTrigger
Menu.Separator = MenuSeparator
Menu.SubmenuTrigger = aria.SubmenuTrigger
