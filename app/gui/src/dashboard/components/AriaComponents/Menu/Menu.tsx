/**
 * @file
 * A menu displays a list of actions or options that a user can choose.
 */
import * as React from 'react'

import { createHideableComponent, createLeafComponent } from '@react-aria/collections'
import * as aria from 'react-aria-components'

import { tv, type VariantProps } from '#/utilities/tailwindVariants'

import { twJoin } from '#/utilities/tailwindMerge'
import { memo } from 'react'
import { AnimatedBackground } from '../../AnimatedBackground'
import { Popover } from '../Dialog'
import { Separator, SEPARATOR_STYLES, type SeparatorProps } from '../Separator'
import { Text } from '../Text'
import type { TestIdProps } from '../types'
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
    VariantProps<typeof MENU_STYLES>,
    TestIdProps {
  readonly variant?: 'dark' | 'light'
  readonly className?: string
}

/** Props for {@link MenuSection} */
export type MenuSectionProps<T extends object> = BaseMenuSectionProps &
  TestIdProps &
  VariantProps<typeof MENU_SECTION_STYLES> &
  (MenuSectionDynamicProps<T> | MenuSectionStaticProps)

/**
 * Base props for a menu section.
 */
interface BaseMenuSectionProps {
  readonly title: string
  readonly className?: string
  readonly id?: aria.Key
}

/**
 * Props for a dynamic menu section.
 */
interface MenuSectionDynamicProps<T extends object> {
  readonly items: Iterable<T>
  readonly children: (item: T) => React.ReactNode
}

/**
 * Props for a static menu section.
 */
interface MenuSectionStaticProps {
  readonly items?: never
  readonly children: React.ReactNode
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
    variants = MENU_STYLES,
    testId = 'menu',
    ...menuProps
  } = props

  const styles = variants()

  return (
    <Popover variant={variant} className={styles.popover()} size="xxsmall" rounded="xxxlarge">
      {() => (
        <AnimatedBackground>
          <aria.Menu<T> data-testid={testId} className={styles.base({ className })} {...menuProps}>
            {children}
          </aria.Menu>
        </AnimatedBackground>
      )}
    </Popover>
  )
}) as (<T extends object>(props: MenuProps<T>) => React.ReactElement) & {
  /* eslint-disable @typescript-eslint/naming-convention */
  Item: typeof MenuItem
  Section: typeof MenuSection
  SectionHeader: typeof MenuSectionHeader
  Trigger: typeof MenuTrigger
  Separator: typeof MenuSeparator
  SubmenuTrigger: typeof aria.SubmenuTrigger
  /* eslint-enable @typescript-eslint/naming-convention */
}

/**
 * A section within a menu.
 */
function MenuSection<T extends object>(props: MenuSectionProps<T>) {
  const {
    className,
    title,
    items,
    children,
    variants = MENU_SECTION_STYLES,
    testId = 'menu-section',
    ...sectionProps
  } = props

  const styles = variants()

  return (
    <aria.MenuSection
      data-testid={testId}
      aria-label={title}
      className={styles.base({ className })}
      {...sectionProps}
    >
      <MenuSectionHeader title={title} variants={variants} />

      {items ?
        <aria.Collection items={items}>{children}</aria.Collection>
      : children}
    </aria.MenuSection>
  )
}

/** Props for {@link MenuSectionHeader} */
export interface MenuSectionHeaderProps
  extends VariantProps<typeof MENU_SECTION_STYLES>,
    TestIdProps {
  readonly title: string
  readonly className?: string
}

/**
 * A header for a menu section.
 */
// eslint-disable-next-line no-restricted-syntax
export const MenuSectionHeader = createLeafComponent(
  'header',
  function MenuSectionHeader(props: MenuSectionHeaderProps) {
    const {
      className,
      title,
      variants = MENU_SECTION_STYLES,
      testId = 'menu-section-header',
    } = props

    const styles = variants()

    return (
      <Text
        elementType="header"
        testId={testId}
        variant="body-sm"
        weight="bold"
        color="muted"
        textSelection="none"
        className={styles.header({ className: twJoin(className, 'block') })}
      >
        {title}
      </Text>
    )
  },
)

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
Menu.SectionHeader = MenuSectionHeader
Menu.Trigger = MenuTrigger
Menu.Separator = MenuSeparator
Menu.SubmenuTrigger = aria.SubmenuTrigger
