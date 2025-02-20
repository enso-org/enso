/**
 * @file
 * An item within a menu that represents a single action or option.
 */
import ArrowRight from '#/assets/expand_arrow_right.svg'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { memo, type ReactElement, type ReactNode } from 'react'
import type { MenuItemProps as AriaMenuItemProps, MenuItemRenderProps } from 'react-aria-components'
import { MenuItem as AriaMenuItem, Keyboard } from 'react-aria-components'
import { AnimatedBackground } from '../../AnimatedBackground'
import { Icon } from '../../Icon'
import SvgMask from '../../SvgMask'
import { Check } from '../Check'
import { Text, TEXT_STYLE } from '../Text'
import type { IconProp, TestIdProps } from '../types'

export const MENU_ITEM_STYLES = tv({
  base: 'group flex w-full cursor-default gap-3 rounded-3xl px-[14px] py-1 outline-none transition-colors duration-75 text-left',
  variants: {
    isDisabled: {
      true: 'cursor-not-allowed',
      false: '',
    },
    isPressed: {
      true: 'bg-primary/5',
    },
  },
  slots: {
    checkContainer: 'block',
    icon: 'flex-none h-4 w-4',
    submenuIndicator: 'flex-none h-4 w-4 self-center text-primary',
    shortcut: 'self-center text-primary mt-[1px]',
    title: 'block w-full flex-1',
    description: 'block w-full flex-1',
    hover: 'bg-primary/5 w-full rounded-3xl',
    customContent: TEXT_STYLE({
      className: 'flex flex-1 min-w-0 w-full text-primary',
    }),
  },
  compoundSlots: [
    {
      slots: ['checkContainer', 'icon'],
      className: 'mt-[3.5px] text-primary',
    },
  ],
  defaultVariants: { isDisabled: false, isSelected: false },
})

/** Props for {@link MenuItem} */
export type MenuItemProps<T extends object, IconType extends string> = MenuItemBaseProps<IconType> &
  Omit<AriaMenuItemProps<T>, 'children'> &
  TestIdProps &
  VariantProps<typeof MENU_ITEM_STYLES> &
  (MenuItemCustomContentProps | MenuItemDefaultContentProps)

/**
 * Base props for the menu item.
 */
export interface MenuItemBaseProps<IconType extends string> {
  /** Icon to display before the menu item text. Can be a string (path to SVG), ReactElement, or a render function */
  readonly icon?: IconProp<IconType, MenuItemRenderProps>
  /** Keyboard shortcut text to display */
  readonly shortcut?: string
  /** Additional class name */
  readonly className?: string
}

/**
 * Menu item with arbitrary content.
 */
export interface MenuItemDefaultContentProps {
  /** Title of the menu item */
  readonly children: string | ((props: MenuItemRenderProps) => string) | undefined
  /** Description of the menu item */
  readonly description?: string | ((props: MenuItemRenderProps) => string) | undefined
}

/**
 * Menu item with arbitrary content.
 */
export interface MenuItemCustomContentProps {
  /** Content of the menu item */
  readonly children?: ReactElement | ((props: MenuItemRenderProps) => ReactElement)
  readonly description?: never
}

/**
 * An item within a menu that represents a single action or option.
 */
export const MenuItem = memo(function MenuItem<T extends object, IconType extends string>(
  props: MenuItemProps<T, IconType>,
) {
  const {
    icon,
    shortcut,
    className,
    variants = MENU_ITEM_STYLES,
    testId = 'menu-item',
    ...itemProps
  } = props

  return (
    <AriaMenuItem data-testid={testId} {...itemProps}>
      {(renderProps) => {
        const { isHovered, isDisabled, isPressed, isFocusVisible } = renderProps
        const classes = variants({ isDisabled, className, isPressed })

        let content: ReactNode

        if (typeof props.children !== 'undefined') {
          content =
            typeof props.children === 'function' ? props.children(renderProps) : props.children

          if (typeof content === 'string') {
            content = (
              <MenuItemContent title={content} description={props.description} {...renderProps} />
            )
          } else {
            content = <div className={classes.customContent()}>{content}</div>
          }
        } else {
          content = (
            <MenuItemContent
              title={props.children}
              description={props.description}
              {...renderProps}
            />
          )
        }

        return (
          <AnimatedBackground.Item
            isSelected={isHovered || isFocusVisible}
            className={classes.base()}
            animationClassName={classes.hover()}
          >
            <div className="flex w-full gap-2">
              <SelectionIndicator className={classes.checkContainer()} {...renderProps} />

              <MenuItemIcon icon={icon} {...renderProps} className={classes.icon()} />

              {content}

              <ShortcutText shortcut={shortcut} className={classes.shortcut()} />

              <SubmenuIndicator
                hasSubmenu={renderProps.hasSubmenu}
                className={classes.submenuIndicator()}
              />
            </div>
          </AnimatedBackground.Item>
        )
      }}
    </AriaMenuItem>
  )
})

/** Props for {@link MenuItemIcon} */
interface MenuItemIconProps<IconType extends string> extends MenuItemRenderProps {
  readonly icon: MenuItemProps<object, IconType>['icon']
  readonly className?: string
}

/** Renders the icon for the menu item */
// eslint-disable-next-line no-restricted-syntax
const MenuItemIcon = memo(function MenuItemIcon<IconType extends string>(
  props: MenuItemIconProps<IconType>,
) {
  const { icon, className, ...renderProps } = props

  return (
    <Icon color="current" renderProps={renderProps} className={className}>
      {icon}
    </Icon>
  )
})

/** Renders the selection indicator for the menu item */
// eslint-disable-next-line no-restricted-syntax
const SelectionIndicator = memo(function SelectionIndicator(
  props: MenuItemRenderProps & { className?: string },
) {
  const { selectionMode, isSelected, className, isPressed, hasSubmenu } = props

  if (selectionMode === 'none' || hasSubmenu) return null

  return (
    <span className={className}>
      <Check isSelected={isSelected} size="medium" isPressed={isPressed} />
    </span>
  )
})

/** Renders the shortcut text for the menu item */
// eslint-disable-next-line no-restricted-syntax
const ShortcutText = memo(function ShortcutText(props: {
  shortcut?: string | undefined
  className?: string
}) {
  const { shortcut, className } = props

  if (shortcut == null) return null

  return (
    <Keyboard className={className}>
      <Text variant="body" nowrap truncate="1" textSelection="none" transform="uppercase">
        {shortcut}
      </Text>
    </Keyboard>
  )
})

/** Renders the submenu indicator */
// eslint-disable-next-line no-restricted-syntax
const SubmenuIndicator = memo(function SubmenuIndicator(props: {
  hasSubmenu: boolean
  className?: string
}) {
  const { hasSubmenu, className } = props

  if (!hasSubmenu) return null

  return <SvgMask src={ArrowRight} className={className} />
})

/**
 * Props for {@link MenuItemContent}
 */
interface MenuItemContentProps extends MenuItemRenderProps {
  readonly title: string | ((props: MenuItemRenderProps) => string) | undefined
  readonly description?: string | ((props: MenuItemRenderProps) => string) | undefined
}

/**
 * Renders the content of the menu item.
 */
// eslint-disable-next-line no-restricted-syntax
const MenuItemContent = memo(function MenuItemContent(props: MenuItemContentProps) {
  const { title, description, ...renderProps } = props

  const titleContent = typeof title === 'function' ? title(renderProps) : title
  const descriptionContent =
    typeof description === 'function' ? description(renderProps) : description

  if (descriptionContent == null)
    return (
      <Text
        className="block w-full"
        variant="body"
        nowrap
        truncate="1"
        textSelection="none"
        tooltipPlacement="right"
      >
        {titleContent}
      </Text>
    )

  return (
    <div className="-mt-[1px] flex w-full min-w-0 flex-1 flex-col">
      <Text
        className="block w-full"
        variant="body"
        nowrap
        truncate="1"
        textSelection="none"
        tooltipPlacement="right"
      >
        {titleContent}
      </Text>

      <Text
        className="-mt-[4px] block w-full"
        variant="caption"
        nowrap
        truncate="1"
        textSelection="none"
        disableLineHeightCompensation
        tooltipPlacement="right"
      >
        {descriptionContent}
      </Text>
    </div>
  )
})
