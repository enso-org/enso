/**
 * @file Breadcrumbs component implementation.
 */

import { useText } from '#/providers/TextProvider'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { createLeafComponent } from '@react-aria/collections'
import { isValidElement } from 'react'
import * as aria from 'react-aria-components'
import { Button, Menu, Text, type Addon, type IconProp, type TestIdProps } from '../AriaComponents'
import { Icon as IconComponent, renderIcon } from '../Icon'

export const BREADCRUMB_ITEM_STYLES = tv({
  base: 'flex items-center gap-2',
  slots: {
    link: 'max-w-48 block',
    more: 'aspect-square',
    container: 'flex items-center gap-2',
  },
  variants: {
    isCurrent: {
      true: { link: 'px-2' },
    },
  },
  defaultVariants: {
    isCurrent: false,
  },
})

/**
 * Props for {@link BreadcrumbItem}
 */
export interface BreadcrumbItemProps<IconType extends string>
  extends Omit<aria.BreadcrumbProps, 'id'>,
    Omit<aria.LinkProps, 'children' | 'className' | 'style'>,
    TestIdProps,
    VariantProps<typeof BREADCRUMB_ITEM_STYLES> {
  /** A unique id for the breadcrumb, which will be passed to `onAction` when the breadcrumb is pressed. */
  readonly id?: aria.Key | undefined
  /** An optional suffix element to render after the breadcrumb content */
  readonly addonStart?: Addon<aria.BreadcrumbRenderProps>
  readonly addonEnd?: Addon<aria.BreadcrumbRenderProps>
  readonly icon?: IconProp<IconType, aria.BreadcrumbRenderProps>
  readonly isCurrent?: boolean
  readonly isDisabled?: boolean
}

/**
 * A single breadcrumb item.
 */
export function BreadcrumbItem<IconType extends string>(props: BreadcrumbItemProps<IconType>) {
  const {
    children,
    id,
    variants = BREADCRUMB_ITEM_STYLES,
    className,
    style = {},
    addonStart,
    addonEnd,
    icon,
    isDisabled = false,
    isCurrent = false,
    href,
    hrefLang,
    target,
    download,
    rel,
    ping,
    referrerPolicy,
    ...itemProps
  } = props

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

  const styles = variants({
    isCurrent,
  })

  return (
    <aria.Breadcrumb
      className={(renderProps) =>
        styles.base({
          className: typeof className === 'function' ? className(renderProps) : className,
        })
      }
      style={style}
      {...(id != null ? { id } : {})}
    >
      {(renderProps) => {
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
                <IconComponent size="medium" renderProps={renderProps}>
                  {icon}
                </IconComponent>
                {typeof children === 'function' ? children(renderProps) : children}
              </span>
            </Text>
          : <Button {...linkProps} {...itemProps} icon={renderIcon(icon, renderProps)}>
              <Text className={styles.link()} nowrap truncate="1" disableLineHeightCompensation>
                {typeof children === 'function' ? children(renderProps) : children}
              </Text>
            </Button>

        return (
          <div className={styles.container()}>
            <Button.GroupJoin
              verticalAlign="center"
              buttonVariants={{ variant: 'icon', isDisabled }}
            >
              {typeof addonStart === 'function' ? addonStart(renderProps) : addonStart}

              {container}

              {typeof addonEnd === 'function' ? addonEnd(renderProps) : addonEnd}
            </Button.GroupJoin>
          </div>
        )
      }}
    </aria.Breadcrumb>
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
}

/**
 * A collapsed breadcrumb item. Displays a menu with the items, that don't fit in the breadcrumbs list.
 * @internal
 */
// eslint-disable-next-line no-restricted-syntax
export const BreadcrumbCollapsedItem = createLeafComponent(
  'BreadcrumbCollapsedItem',
  function BreadcrumbCollapsedItem<T extends object>(props: BreadcrumbCollapsedItemProps<T>) {
    const { getText } = useText()

    const { items, children, triggerLabel = getText('more') } = props

    return (
      <Menu.Trigger>
        <Button aria-label={triggerLabel} className="aspect-square">
          {/* eslint-disable-next-line no-restricted-syntax */}
          <span aria-hidden="true">...</span>
        </Button>

        <Menu items={items}>
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
                  icon={() => {
                    if (typeof icon === 'function') {
                      return icon({ isCurrent, isDisabled })
                    }

                    return icon
                  }}
                >
                  <>
                    {typeof breadcrumbChildren === 'function' ?
                      breadcrumbChildren({
                        isCurrent,
                        isDisabled,
                        defaultChildren: <></>,
                      })
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
  },
) as <T extends object>(props: BreadcrumbCollapsedItemProps<T>) => React.ReactNode
