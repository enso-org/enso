/** @file A styled button. */
import {
  memo,
  useLayoutEffect,
  useRef,
  useState,
  type ForwardedRef,
  type ReactElement,
  type ReactNode,
} from 'react'

import * as aria from '#/components/aria'
import { StatelessSpinner } from '#/components/StatelessSpinner'
import SvgMask from '#/components/SvgMask'

import { useVisualTooltip } from '#/components/AriaComponents/Text'
import { Tooltip, TooltipTrigger } from '#/components/AriaComponents/Tooltip'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { forwardRef } from '#/utilities/react'
import { ButtonGroup, ButtonGroupJoin } from './ButtonGroup'
import { useJoinedButtonPrivateContext, useMergedButtonStyles } from './shared'
import type { ButtonProps } from './types'
import { BUTTON_STYLES } from './variants'

const ICON_LOADER_DELAY = 150

/** A button allows a user to perform an action, with mouse, touch, and keyboard interactions. */
// Manually casting types to make TS infer the final type correctly (e.g. RenderProps in icon)
// eslint-disable-next-line no-restricted-syntax
export const Button = memo(
  forwardRef(function Button(props: ButtonProps, ref: ForwardedRef<HTMLButtonElement>) {
    props = useMergedButtonStyles(props)
    const {
      className,
      contentClassName,
      children,
      variant,
      icon,
      loading,
      isActive,
      showIconOnHover,
      iconPosition,
      size,
      fullWidth,
      rounded,
      tooltip,
      tooltipPlacement,
      testId,
      loaderPosition = 'full',
      extraClickZone: extraClickZoneProp,
      onPress = () => {},
      variants = BUTTON_STYLES,
      addonStart,
      addonEnd,
      hideLoader = false,
      ...ariaProps
    } = props

    const { position, isJoined } = useJoinedButtonPrivateContext()

    const [implicitlyLoading, setImplicitlyLoading] = useState(false)

    const contentRef = useRef<HTMLSpanElement>(null)
    const loaderRef = useRef<HTMLSpanElement>(null)

    const isLink = ariaProps.href != null

    const Tag = isLink ? aria.Link : aria.Button

    const goodDefaults = {
      ...(isLink ? { rel: 'noopener noreferrer' } : { type: 'button' as const }),
      'data-testid': testId,
    }

    const isIconOnly = (children == null || children === '' || children === false) && icon != null

    const shouldShowTooltip = (() => {
      if (tooltip === false) {
        return false
      } else if (isIconOnly) {
        return true
      } else {
        return tooltip != null
      }
    })()

    const tooltipElement = shouldShowTooltip ? (tooltip ?? ariaProps['aria-label']) : null

    const isLoading = (() => {
      if (typeof loading === 'boolean') {
        return loading
      }

      return implicitlyLoading
    })()

    const isDisabled = props.isDisabled ?? isLoading
    const shouldUseVisualTooltip = shouldShowTooltip && isDisabled
    const extraClickZone = extraClickZoneProp ?? variant === 'icon'

    useLayoutEffect(() => {
      const delay = ICON_LOADER_DELAY

      if (isLoading) {
        const loaderAnimation = loaderRef.current?.animate(
          [{ opacity: 0 }, { opacity: 0, offset: 1 }, { opacity: 1 }],
          { duration: delay, easing: 'linear', delay: 0, fill: 'forwards' },
        )
        const contentAnimation =
          loaderPosition !== 'full' ? null : (
            contentRef.current?.animate([{ opacity: 1 }, { opacity: 0 }], {
              duration: 0,
              easing: 'linear',
              delay,
              fill: 'forwards',
            })
          )

        return () => {
          loaderAnimation?.cancel()
          contentAnimation?.cancel()
        }
      } else {
        return () => {}
      }
    }, [isLoading, loaderPosition])

    const handlePress = useEventCallback((event: aria.PressEvent): void => {
      if (!isDisabled) {
        const result = onPress?.(event)

        if (result instanceof Promise) {
          setImplicitlyLoading(true)

          void result.finally(() => {
            setImplicitlyLoading(false)
          })
        }
      }
    })

    const styles = variants({
      isDisabled,
      isActive,
      loading: isLoading,
      fullWidth,
      size,
      rounded,
      variant,
      iconPosition,
      showIconOnHover,
      extraClickZone,
      iconOnly: isIconOnly,
      isJoined,
      position,
    })

    const { tooltip: visualTooltip, targetProps } = useVisualTooltip({
      targetRef: contentRef,
      children: tooltipElement,
      isDisabled: !shouldUseVisualTooltip,
      ...(tooltipPlacement && { overlayPositionProps: { placement: tooltipPlacement } }),
    })

    const shouldDisplayBorder = isJoined && (position === 'first' || position === 'middle')

    const button = (
      <Tag
        // @ts-expect-error ts errors are expected here because we are merging props with different types
        ref={ref}
        // @ts-expect-error ts errors are expected here because we are merging props with different types
        {...aria.mergeProps<aria.ButtonProps>()(goodDefaults, ariaProps, {
          isPending: isLoading,
          isDisabled,
          // we use onPressEnd instead of onPress because for some reason react-aria doesn't trigger
          // onPress on EXTRA_CLICK_ZONE, but onPress{start,end} are triggered
          onPressEnd: (e) => {
            if (!isDisabled) {
              handlePress(e)
            }
          },
          className: aria.composeRenderProps(className, (classNames, states) =>
            styles.base({ className: classNames, ...states }),
          ),
        })}
      >
        {(render: aria.ButtonRenderProps | aria.LinkRenderProps) => {
          const shouldShowOverlayLoader = () => {
            if (hideLoader) {
              return false
            }

            return isLoading && loaderPosition === 'full'
          }

          return (
            <>
              <span className={styles.wrapper()}>
                <span
                  ref={contentRef}
                  className={styles.content({ className: contentClassName })}
                  {...targetProps}
                >
                  <ButtonContent
                    isIconOnly={isIconOnly}
                    loaderPosition={loaderPosition}
                    hideLoader={hideLoader}
                    /* @ts-expect-error any here is safe because we transparently pass it to the children, and ts infer the type outside correctly */
                    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                    isLoading={render.isPending}
                    /* @ts-expect-error any here is safe because we transparently pass it to the children, and ts infer the type outside correctly */
                    icon={typeof icon === 'function' ? icon(render) : icon}
                    styles={styles}
                    /* @ts-expect-error any here is safe because we transparently pass it to the children, and ts infer the type outside correctly */
                    addonStart={typeof addonStart === 'function' ? addonStart(render) : addonStart}
                    /* @ts-expect-error any here is safe because we transparently pass it to the children, and ts infer the type outside correctly */
                    addonEnd={typeof addonEnd === 'function' ? addonEnd(render) : addonEnd}
                  >
                    {/* @ts-expect-error any here is safe because we transparently pass it to the children, and ts infer the type outside correctly */}
                    {typeof children === 'function' ? children(render) : children}
                  </ButtonContent>
                </span>

                {shouldShowOverlayLoader() && (
                  <span ref={loaderRef} className={styles.loader()}>
                    <StatelessSpinner state="loading-medium" size={16} />
                  </span>
                )}

                {shouldShowTooltip && visualTooltip}
              </span>

              {shouldDisplayBorder && <div className={styles.joinSeparator()} />}
            </>
          )
        }}
      </Tag>
    )

    if (tooltipElement == null) {
      return button
    }

    return (
      <TooltipTrigger delay={0} closeDelay={0}>
        {button}

        <Tooltip {...(tooltipPlacement != null ? { placement: tooltipPlacement } : {})}>
          {tooltipElement}
        </Tooltip>
      </TooltipTrigger>
    )
  }),
) as unknown as ((props: ButtonProps & { ref?: ForwardedRef<HTMLButtonElement> }) => ReactNode) & {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Group: typeof ButtonGroup
  // eslint-disable-next-line @typescript-eslint/naming-convention
  GroupJoin: typeof ButtonGroupJoin
}

Button.Group = ButtonGroup
Button.GroupJoin = ButtonGroupJoin

/**
 * Props for {@link ButtonContent}.
 */
interface ButtonContentProps {
  readonly hideLoader: boolean
  readonly isIconOnly: boolean
  readonly isLoading: boolean
  readonly loaderPosition: 'full' | 'icon'
  readonly icon: ReactElement | string | null | undefined
  readonly styles: ReturnType<typeof BUTTON_STYLES>
  readonly children: ReactNode
  readonly addonStart?: ReactElement | string | false | null | undefined
  readonly addonEnd?: ReactElement | string | false | null | undefined
}

/**
 * Checks if an addon is present.
 */
function hasAddon(addon: ButtonContentProps['addonEnd']): boolean {
  return addon != null && addon !== false && addon !== ''
}

/**
 * Renders the content of a button.
 */
// eslint-disable-next-line no-restricted-syntax
const ButtonContent = memo(function ButtonContent(props: ButtonContentProps) {
  const {
    isIconOnly,
    isLoading,
    loaderPosition,
    icon,
    styles,
    children,
    addonStart,
    addonEnd,
    hideLoader,
  } = props

  // Icon only button
  if (isIconOnly) {
    return (
      <span className={styles.extraClickZone()}>
        {hasAddon(addonStart) && <div className={styles.addonStart()}>{addonStart}</div>}
        <Icon
          isLoading={isLoading}
          loaderPosition={loaderPosition}
          icon={icon}
          styles={styles}
          hideLoader={hideLoader}
        />
        {hasAddon(addonEnd) && <div className={styles.addonEnd()}>{addonEnd}</div>}
      </span>
    )
  }

  // Default button
  return (
    <>
      {hasAddon(addonStart) && <div className={styles.addonStart()}>{addonStart}</div>}
      <Icon
        isLoading={isLoading}
        loaderPosition={loaderPosition}
        icon={icon}
        styles={styles}
        hideLoader={hideLoader}
      />
      <span className={styles.text()}>{children}</span>
      {hasAddon(addonEnd) && <div className={styles.addonEnd()}>{addonEnd}</div>}
    </>
  )
})

/**
 * Props for {@link Icon}.
 */
interface IconProps {
  readonly isLoading: boolean
  readonly loaderPosition: 'full' | 'icon'
  readonly icon: ReactElement | string | null | undefined
  readonly styles: ReturnType<typeof BUTTON_STYLES>
  readonly hideLoader: boolean
}

/**
 * Renders an icon for a button.
 */
const Icon = memo(function Icon(props: IconProps) {
  const { isLoading, loaderPosition, icon, styles, hideLoader } = props

  const [loaderIsVisible, setLoaderIsVisible] = useState(false)

  useLayoutEffect(() => {
    if (isLoading && loaderPosition === 'icon') {
      const timeout = setTimeout(() => {
        setLoaderIsVisible(true)
      }, ICON_LOADER_DELAY)

      return () => {
        clearTimeout(timeout)
      }
    } else {
      setLoaderIsVisible(false)
    }
  }, [isLoading, loaderPosition])

  const shouldShowLoader = (() => {
    if (hideLoader) {
      return false
    }

    return isLoading && loaderPosition === 'icon' && loaderIsVisible
  })()

  if (icon == null && !shouldShowLoader) {
    return null
  }

  const actualIcon = (() => {
    return typeof icon === 'string' ?
        <SvgMask src={icon} className={styles.icon()} />
      : <span className={styles.icon()}>{icon}</span>
  })()

  if (shouldShowLoader) {
    return (
      <div className={styles.icon()}>
        <StatelessSpinner state="loading-medium" size={16} />
      </div>
    )
  }

  return actualIcon
})
