/** @file A single breadcrumb item. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { noop } from '#/utilities/functions'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { useMutation } from '@tanstack/react-query'
import {
  createContext,
  useContext,
  useRef,
  type CSSProperties,
  type Key,
  type PropsWithChildren,
  type ReactNode,
} from 'react'
import {
  useBreadcrumbItem,
  useDrop,
  type AriaBreadcrumbItemProps,
  type DropEvent,
  type PressEvent,
} from 'react-aria'
import type * as aria from 'react-aria-components'
import invariant from 'tiny-invariant'
import { Button, Text, type Addon, type IconProp, type TestIdProps } from '../AriaComponents'
import { Icon as IconComponent } from '../Icon'

export const BREADCRUMB_ITEM_STYLES = tv({
  base: 'flex items-center gap-2 bg-transparent transition-colors',
  slots: {
    link: 'block max-w-48 min-w-4 w-auto',
    more: 'aspect-square',
    container: 'flex items-center gap-2',
    icon: '-mb-0.5',
  },
  variants: {
    isCurrent: {
      true: { link: 'flex justify-center px-2 h-8' },
    },
    isDropTarget: {
      true: { base: 'bg-primary/10 rounded-4xl cursor-copy' },
    },
  },
  defaultVariants: {
    isCurrent: false,
    isDropTarget: false,
  },
})

/** Render props for {@link BreadcrumbItem}. */
export interface BreadcrumbItemRenderProps {
  readonly isCurrent: boolean
  readonly isDisabled: boolean
}

/** Props for {@link BreadcrumbItem}. */
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
  readonly isLoading?: boolean
  readonly isDroppable?: boolean
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
  readonly onAction: (key: Key) => Promise<void> | void
  /**
   * Workaround to have optimized `onDrop` callback using `useEventCallback` hook.
   * And be able to check if `onDrop` prop was specified and id is not.
   */
  readonly onDropSpecified: boolean
  readonly onDrop: (key: Key, e: DropEvent) => Promise<void> | void
}

/**
 * Context for the breadcrumb item.
 */
export const BreadcrumbItemContext = createContext<BreadcrumbItemContextType>({
  isCurrent: false,
  onActionSpecified: false,
  onAction: noop,
  onDropSpecified: false,
  onDrop: noop,
})

/** Provider for the breadcrumb item context. */
export function BreadcrumbItemProvider(props: PropsWithChildren<BreadcrumbItemContextType>) {
  return (
    <BreadcrumbItemContext.Provider value={props}>{props.children}</BreadcrumbItemContext.Provider>
  )
}

/** A single breadcrumb item. */
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
    onPress: onPressRaw,
    isDroppable = true,
  } = props
  const { id, ...breadcrumbItemProps } = props

  const { isCurrent, onAction, onActionSpecified, onDrop, onDropSpecified } =
    useContext(BreadcrumbItemContext)

  const renderProps = { isCurrent, isDisabled } satisfies BreadcrumbItemRenderProps

  const ref = useRef(null)
  const { itemProps } = useBreadcrumbItem({ elementType: 'div', ...breadcrumbItemProps }, ref)

  const dropMutation = useMutation({
    mutationFn: async (params: { id: Key | null | undefined; e: DropEvent }) => {
      if (params.id == null) {
        return
      }

      return onDrop(params.id, params.e)
    },
  })

  // `dropProps` is type-safe, ESLint is being silly.
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const { dropProps, isDropTarget } = useDrop({
    isDisabled: !onDropSpecified || isDisabled || !isDroppable,
    ref,
    onDrop: (e) => {
      dropMutation.mutate({ id, e })
    },
  })

  const onPress = useEventCallback(async (event: PressEvent) => {
    if (id == null) {
      return
    }

    await Promise.all([onAction(id), onPressRaw?.(event) ?? Promise.resolve()])
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
    : ({ href, hrefLang, target, download, rel, ping, referrerPolicy } as Pick<
        aria.LinkProps,
        'download' | 'href' | 'hrefLang' | 'ping' | 'referrerPolicy' | 'rel' | 'target'
      >)

  const styles = variants({ isCurrent, isDropTarget })

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
    : <Button
        {...linkProps}
        loading={dropMutation.isPending}
        loaderPosition="icon"
        onPress={onPress}
        icon={iconComponent}
      >
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
      {...dropProps}
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
