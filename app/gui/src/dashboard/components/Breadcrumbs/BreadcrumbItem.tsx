/** @file A single breadcrumb item. */
import { mergeProps } from '#/components/aria'
import { Button } from '#/components/Button'
import { IconDisplay } from '#/components/IconDisplay'
import { Text } from '#/components/Text'
import type { Addon, IconProp, TestIdProps } from '#/components/types'
import type { TooltipElementType } from '#/components/VisualTooltip'
import { useDragDelayAction, type DragDelayCallback } from '#/hooks/dragDelayHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { noop } from '#/utilities/functions'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import {
  createContext,
  useContext,
  useRef,
  useState,
  type CSSProperties,
  type HTMLAttributes,
  type Key,
  type PropsWithChildren,
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

// eslint-disable-next-line react-refresh/only-export-components
export const BREADCRUMB_ITEM_STYLES = tv({
  base: 'flex items-center gap-2 bg-transparent transition-colors rounded-4xl drop-target-after',
  slots: {
    link: 'block max-w-48 min-w-4 w-auto',
    more: 'aspect-square',
    container: 'flex items-center gap-2',
    iconDisplay: 'h-8',
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
  readonly children:
    | TooltipElementType
    | ((renderProps: BreadcrumbItemRenderProps) => TooltipElementType)
  readonly isLoading?: boolean
  readonly isDroppable?: boolean
  readonly onDragDelay?: DragDelayCallback<HTMLElement> | undefined
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
// eslint-disable-next-line react-refresh/only-export-components
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
    onDragDelay,
  } = props
  const { id, ...breadcrumbItemProps } = props

  const [isTransitioning, setIsTransitioning] = useState(false)

  const { isCurrent, onAction, onActionSpecified, onDrop, onDropSpecified } =
    useContext(BreadcrumbItemContext)

  const renderProps = { isCurrent, isDisabled } satisfies BreadcrumbItemRenderProps

  const ref = useRef(null)
  const { itemProps } = useBreadcrumbItem({ elementType: 'div', ...breadcrumbItemProps }, ref)

  const useDropResult: {
    readonly dropProps: HTMLAttributes<HTMLElement>
    readonly isDropTarget: boolean
  } = useDrop({
    isDisabled: !onDropSpecified || isDisabled || !isDroppable,
    ref,
    onDrop: (e) => {
      if (id == null) {
        return
      }

      const res = onDrop(id, e)

      if (res instanceof Promise) {
        setIsTransitioning(true)
        void res.finally(() => {
          setIsTransitioning(false)
        })
      }
    },
  })
  const { dropProps, isDropTarget } = useDropResult

  const dragDelayProps = useDragDelayAction(onDragDelay)

  const onPress = useEventCallback(async (event: PressEvent) => {
    if (id == null) {
      return
    }

    await Promise.all([onAction(id), onPressRaw?.(event) ?? Promise.resolve()])
  })

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

  const styles = variants({ isCurrent })

  const renderedIcon = typeof icon === 'function' ? icon(renderProps) : icon
  const renderedChildren = typeof children === 'function' ? children(renderProps) : children

  const container =
    isCurrent ?
      <IconDisplay
        data-current
        aria-current="page"
        textSelection="none"
        elementType="a"
        icon={renderedIcon}
        className={styles.iconDisplay()}
      >
        {renderedChildren}
      </IconDisplay>
    : <Button
        {...linkProps}
        isLoading={isTransitioning}
        loaderPosition="icon"
        onPress={onPress}
        icon={renderedIcon}
      >
        <Text className={styles.link()} nowrap truncate="1" disableLineHeightCompensation>
          {renderedChildren}
        </Text>
      </Button>

  return (
    <li
      className={styles.base({
        className: typeof className === 'function' ? className(renderProps) : className,
      })}
      style={typeof style === 'function' ? style(renderProps) : style}
      data-drop-target={isDropTarget}
      {...(id != null ? { id: id.toString() } : {})}
      {...mergeProps<HTMLAttributes<HTMLElement>>()(dropProps, dragDelayProps)}
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
