/**
 * @file Breadcrumbs component implementation.
 */

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '#/providers/TextProvider'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import {
  createContext,
  isValidElement,
  useContext,
  useRef,
  type CSSProperties,
  type Key,
  type PropsWithChildren,
  type ReactNode,
} from 'react'
import { useBreadcrumbItem, type AriaBreadcrumbItemProps } from 'react-aria'
import type * as aria from 'react-aria-components'
import invariant from 'tiny-invariant'
import { Button, Menu, Text, type Addon, type IconProp, type TestIdProps } from '../AriaComponents'
import { Icon as IconComponent, renderIcon } from '../Icon'

export const BREADCRUMB_ITEM_STYLES = tv({
  base: 'flex items-center gap-2',
  slots: {
    link: 'max-w-48',
    more: 'aspect-square',
    container: 'flex items-center gap-2',
    icon: '-mb-0.5',
  },
  variants: {
    isCurrent: {
      true: { link: 'flex justify-center px-2 h-8' },
    },
  },
  defaultVariants: {
    isCurrent: false,
  },
})

/**
 *
 */
export interface BreadcrumbItemRenderProps {
  readonly isCurrent: boolean
  readonly isDisabled: boolean
}

/**
 * Props for {@link BreadcrumbItem}
 */
export interface BreadcrumbItemProps<IconType extends string>
  extends Omit<AriaBreadcrumbItemProps, 'id'>,
    Omit<aria.LinkProps, 'children' | 'className' | 'style'>,
    TestIdProps,
    VariantProps<typeof BREADCRUMB_ITEM_STYLES> {
  readonly id?: Key
  /** An optional suffix element to render after the breadcrumb content */
  readonly addonStart?: Addon<BreadcrumbItemRenderProps>
  readonly addonEnd?: Addon<BreadcrumbItemRenderProps>
  readonly icon?: IconProp<IconType, BreadcrumbItemRenderProps>
  readonly isCurrent?: boolean
  readonly isDisabled?: boolean
  readonly className?: string | ((renderProps: BreadcrumbItemRenderProps) => string)
  readonly style?: CSSProperties | ((renderProps: BreadcrumbItemRenderProps) => CSSProperties)
  readonly children: ReactNode | ((renderProps: BreadcrumbItemRenderProps) => ReactNode)
}

/**
 * Context props for {@link BreadcrumbItemProvider}
 */
export interface BreadcrumbItemContextType {
  readonly isCurrent: boolean
  /**
   * Workaround to have optimized `onAction` callback using `useEventCallback` hook.
   * And be able to check if `onAction` prop was specified and id is not.
   */
  readonly onActionSpecified: boolean
  readonly onAction: (key: Key) => void
}

/**
 * Context for the breadcrumb item.
 */
export const BreadcrumbItemContext = createContext<BreadcrumbItemContextType>({
  isCurrent: false,
  onActionSpecified: false,
  onAction: () => {},
})

/**
 * Provider for the breadcrumb item context.
 */
export function BreadcrumbItemProvider(props: PropsWithChildren<BreadcrumbItemContextType>) {
  const { children, isCurrent, onAction, onActionSpecified } = props

  return (
    <BreadcrumbItemContext.Provider value={{ isCurrent, onAction, onActionSpecified }}>
      {children}
    </BreadcrumbItemContext.Provider>
  )
}

/**
 * A single breadcrumb item.
 */
export function BreadcrumbItem<IconType extends string>(props: BreadcrumbItemProps<IconType>) {
  const {
    children,
    variants = BREADCRUMB_ITEM_STYLES,
    className,
    style = {},
    isDisabled = false,
    addonStart,
    addonEnd,
    icon,
    href,
    hrefLang,
    target,
    download,
    rel,
    ping,
    referrerPolicy,
  } = props
  const { id, ...breadcrumbItemProps } = props

  const { isCurrent, onAction, onActionSpecified } = useContext(BreadcrumbItemContext)

  const renderProps = { isCurrent, isDisabled } satisfies BreadcrumbItemRenderProps

  const ref = useRef(null)
  const { itemProps } = useBreadcrumbItem({ elementType: 'div', ...breadcrumbItemProps }, ref)

  const onPress = useEventCallback(() => {
    if (id == null) {
      return
    }

    onAction(id)
  })

  const iconComponent = (() => {
    if (typeof icon === 'function') {
      return icon(renderProps)
    }
    return icon
  })()

  const shouldFail = onActionSpecified && id == null

  invariant(
    !shouldFail,
    'When onAction is specified on `<Breadcrumbs />` component, the `id` prop must be specified on `<BreadcrumbItem />` component.',
  )

  const linkProps =
    isCurrent ?
      {}
      // This is safe because we're passing link props transparently
      // eslint-disable-next-line no-restricted-syntax
    : ({
        href,
        hrefLang,
        target,
        download,
        rel,
        ping,
        referrerPolicy,
      } as Pick<
        aria.LinkProps,
        'download' | 'href' | 'hrefLang' | 'ping' | 'referrerPolicy' | 'rel' | 'target'
      >)

  const styles = variants({ isCurrent })

  const container =
    isCurrent ?
      <Text
        className={styles.link()}
        nowrap
        truncate="1"
        data-current
        aria-current="page"
        textSelection="none"
        elementType="a"
      >
        <span className={styles.container()}>
          <IconComponent className={styles.icon()} size="medium" renderProps={renderProps}>
            {icon}
          </IconComponent>
          {typeof children === 'function' ? children(renderProps) : children}
        </span>
      </Text>
    : <Button {...linkProps} onPress={onPress} icon={iconComponent}>
        <Text className={styles.link()} nowrap truncate="1" disableLineHeightCompensation>
          {typeof children === 'function' ? children(renderProps) : children}
        </Text>
      </Button>

  return (
    <li
      className={styles.base({
        className: typeof className === 'function' ? className(renderProps) : className,
      })}
      style={typeof style === 'function' ? style(renderProps) : style}
      {...(id != null ? { id: id.toString() } : {})}
    >
      <div className={styles.container()} {...itemProps}>
        <Button.GroupJoin verticalAlign="center" buttonVariants={{ variant: 'icon', isDisabled }}>
          {typeof addonStart === 'function' ? addonStart(renderProps) : addonStart}

          {container}

          {typeof addonEnd === 'function' ? addonEnd(renderProps) : addonEnd}
        </Button.GroupJoin>
      </div>
    </li>
  )
}

/**
 * Props for {@link BreadcrumbCollapsedItem}
 */
interface BreadcrumbCollapsedItemProps<T extends object> {
  readonly id?: aria.Key | undefined
  /** The items to render */
  readonly items: T[]
  /** The children to render */
  readonly children: (item: T) => React.ReactNode
  readonly triggerLabel?: string
  /** The callback to call when an item is selected */
  readonly onAction?: (key: Key) => void
}

/**
 * A collapsed breadcrumb item. Displays a menu with the items, that don't fit in the breadcrumbs list.
 * @internal
 */
export function BreadcrumbCollapsedItem<T extends object>(props: BreadcrumbCollapsedItemProps<T>) {
  const { getText } = useText()

  const { items, children, triggerLabel = getText('more') } = props

  const { onAction } = useContext(BreadcrumbItemContext)

  return (
    <Menu.Trigger>
      <Button aria-label={triggerLabel} className="aspect-square">
        {/* eslint-disable-next-line no-restricted-syntax */}
        <span aria-hidden="true">...</span>
      </Button>

      <Menu items={items} onAction={onAction}>
        {(menuItem) => {
          const breadcrumb = children(menuItem)

          if (isValidElement(breadcrumb) && breadcrumb.type === BreadcrumbItem) {
            const {
              testId,
              id,
              children: breadcrumbChildren,
              href,
              download,
              target,
              hrefLang,
              isCurrent = false,
              isDisabled = false,
              'aria-describedby': ariaDescribedby,
              rel,
              icon,
              // eslint-disable-next-line no-restricted-syntax
            } = breadcrumb.props as BreadcrumbItemProps<string>

            if (breadcrumbChildren == null) {
              return null
            }

            // eslint-disable-next-line no-restricted-syntax
            const linkProps = {
              href,
              download,
              target,
              hrefLang,
              rel,
            } as Pick<aria.LinkProps, 'download' | 'href' | 'hrefLang' | 'rel' | 'target'>

            return (
              <Menu.Item
                testId={testId}
                // This is safe, because we're passing the id transparently to the Menu.Item
                // eslint-disable-next-line no-restricted-syntax
                id={id as aria.Key}
                aria-describedby={ariaDescribedby}
                {...linkProps}
                icon={(() => {
                  if (typeof icon === 'function') {
                    return icon({ isCurrent, isDisabled })
                  }

                  return icon
                })()}
              >
                <>
                  {typeof breadcrumbChildren === 'function' ?
                    breadcrumbChildren({ isCurrent, isDisabled })
                  : breadcrumbChildren}
                </>
              </Menu.Item>
            )
          }

          return null
        }}
      </Menu>
    </Menu.Trigger>
  )
}
