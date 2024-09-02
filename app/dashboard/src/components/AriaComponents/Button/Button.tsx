/** @file A styled button. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import StatelessSpinner, * as spinnerModule from '#/components/StatelessSpinner'
import SvgMask from '#/components/SvgMask'

import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { forwardRef } from '#/utilities/react'
import { TEXT_STYLE } from '../Text'

// ==============
// === Button ===
// ==============

/** Props for a {@link Button}. */
export type ButtonProps =
  | (BaseButtonProps<aria.ButtonRenderProps> & Omit<aria.ButtonProps, 'onPress'> & PropsWithoutHref)
  | (BaseButtonProps<aria.LinkRenderProps> & Omit<aria.LinkProps, 'onPress'> & PropsWithHref)

/**
 * Props for a button with an href.
 */
interface PropsWithHref {
  readonly href: string
}

/**
 * Props for a button without an href.
 */
interface PropsWithoutHref {
  readonly href?: never
}

/**
 * Base props for a button.
 */
export interface BaseButtonProps<Render>
  extends Omit<VariantProps<typeof BUTTON_STYLES>, 'iconOnly'> {
  /** Falls back to `aria-label`. Pass `false` to explicitly disable the tooltip. */
  readonly tooltip?: React.ReactElement | string | false | null
  readonly tooltipPlacement?: aria.Placement
  /**
   * The icon to display in the button
   */
  readonly icon?:
    | React.ReactElement
    | string
    | ((render: Render) => React.ReactElement | string | null)
    | null
  /**
   * When `true`, icon will be shown only when hovered.
   */
  readonly showIconOnHover?: boolean
  /**
   * Handler that is called when the press is released over the target.
   * If the handler returns a promise, the button will be in a loading state until the promise resolves.
   */
  readonly onPress?: ((event: aria.PressEvent) => Promise<void> | void) | null | undefined
  readonly contentClassName?: string
  readonly testId?: string
  readonly isDisabled?: boolean
  readonly formnovalidate?: boolean
  /** Defaults to `full`. When `full`, the entire button will be replaced with the loader.
   * When `icon`, only the icon will be replaced with the loader. */
  readonly loaderPosition?: 'full' | 'icon'
  readonly styles?: typeof BUTTON_STYLES
}

export const BUTTON_STYLES = tv({
  base: [
    'group',
    // we need to set the height to max-content to prevent the button from growing in flex containers
    'h-[max-content]',
    // basic outline
    'outline-offset-[1px] outline-transparent',
    // buttons always have borders
    // so keep them in mind when setting paddings
    'border-0.5 border-transparent',
    // button reset styles
    'whitespace-nowrap cursor-pointer select-none appearance-none',
    // Align the content by the center
    'text-center items-center justify-center',
    // animations
    'transition-[opacity,outline-offset,background,border-color] duration-150 ease-in-out',
  ],
  variants: {
    isDisabled: {
      true: 'disabled:opacity-50 disabled:cursor-not-allowed aria-disabled:opacity-50 aria-disabled:cursor-not-allowed',
    },
    isFocused: {
      true: 'focus:outline-none focus-visible:outline-2 focus-visible:outline-black focus-visible:outline-offset-[-2px]',
    },
    isActive: {
      none: '',
      false:
        'disabled:opacity-30 [&.disabled]:opacity-30 disabled:cursor-not-allowed [&.disabled]:cursor-not-allowed opacity-50 hover:opacity-75',
      true: 'opacity-100 disabled:opacity-100 [&.disabled]:opacity-100 hover:opacity-100 disabled:cursor-default [&.disabled]:cursor-default',
    },
    loading: { true: { base: 'cursor-wait' } },
    fullWidth: { true: 'w-full' },
    size: {
      custom: { base: '', extraClickZone: '', icon: 'h-full' },
      hero: { base: 'px-8 py-4 text-lg font-bold', content: 'gap-[0.75em]' },
      large: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'semibold',
          className: 'flex px-[11px] py-[5.5px]',
        }),
        content: 'gap-2',
        icon: 'mb-[-0.1cap] h-4 w-4',
        extraClickZone: 'after:inset-[-6px]',
      },
      medium: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'semibold',
          className: 'flex px-[9px] py-[3.5px]',
        }),
        icon: 'mb-[-0.1cap] h-4 w-4',
        content: 'gap-2',
        extraClickZone: 'after:inset-[-8px]',
      },
      small: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'medium',
          className: 'flex px-[7px] py-[1.5px]',
        }),
        icon: 'mb-[-0.1cap] h-3.5 w-3.5',
        content: 'gap-1',
        extraClickZone: 'after:inset-[-10px]',
      },
      xsmall: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'medium',
          disableLineHeightCompensation: true,
          className: 'flex px-[5px] pt-[0.5px] pb-[2.5px]',
        }),
        icon: 'mb-[-0.2cap] h-3 w-3',
        content: 'gap-1',
        extraClickZone: 'after:inset-[-12px]',
      },
      xxsmall: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          className: 'flex px-[3px] pt-[0.5px] pb-[2.5px] leading-[16px]',
          // we need to disable line height compensation for this size
          // because otherwise the text will be too high in the button
          disableLineHeightCompensation: true,
        }),
        content: 'gap-0.5',
        icon: 'mb-[-0.1cap]',
        extraClickZone: 'after:inset-[-12px]',
      },
    },
    iconOnly: {
      true: {
        base: TEXT_STYLE({
          disableLineHeightCompensation: true,
          className: 'border-0 outline-offset-[5px]',
        }),
        icon: 'mb-[unset]',
      },
    },
    rounded: {
      full: 'rounded-full',
      large: 'rounded-lg',
      medium: 'rounded-md',
      none: 'rounded-none',
      small: 'rounded-sm',
      xlarge: 'rounded-xl',
      xxlarge: 'rounded-2xl',
      xxxlarge: 'rounded-3xl',
    },
    variant: {
      custom: '',
      link: {
        base: 'inline-block px-0 py-0 rounded-sm text-primary/50 underline hover:text-primary border-0',
        content: 'gap-1.5',
        icon: 'h-[1.25cap] w-[1.25cap] mt-[0.25cap]',
      },
      primary: 'bg-primary text-white hover:bg-primary/70',
      accent: 'bg-accent text-white hover:bg-accent-dark',
      delete:
        'bg-danger/80 hover:bg-danger text-white focus-visible:outline-danger focus-visible:bg-danger',
      icon: {
        base: 'text-primary opacity-80 hover:opacity-100 focus-visible:opacity-100',
        wrapper: 'w-full h-full',
        content: 'w-full h-full',
        extraClickZone: 'w-full h-full',
      },
      ghost:
        'text-primary hover:text-primary/80 hover:bg-white focus-visible:text-primary/80 focus-visible:bg-white',
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'ghost-fading':
        'text-primary opacity-80 hover:opacity-100 hover:bg-white focus-visible:bg-white',
      submit: 'bg-invite text-white opacity-80 hover:opacity-100',
      outline: 'border-primary/20 text-primary hover:border-primary hover:bg-primary/5',
    },
    iconPosition: {
      start: { content: '' },
      end: { content: 'flex-row-reverse' },
    },
    showIconOnHover: {
      true: { icon: 'opacity-0 group-hover:opacity-100 group-focus-visible:opacity-100' },
    },
    extraClickZone: {
      true: {
        extraClickZone:
          'flex relative after:absolute after:cursor-pointer group-disabled:after:cursor-not-allowed',
      },
      false: {
        extraClickZone: 'after:inset-0',
      },
      xxsmall: {
        extraClickZone: 'after:inset-[-2px]',
      },
      xsmall: {
        extraClickZone: 'after:inset-[-4px]',
      },
      small: {
        extraClickZone: 'after:inset-[-6px]',
      },
      medium: {
        extraClickZone: 'after:inset-[-8px]',
      },
      large: {
        extraClickZone: 'after:inset-[-10px]',
      },
      custom: {
        extraClickZone: 'after:inset-[calc(var(--extra-click-zone-offset, 0) * -1)]',
      },
    },
  },
  slots: {
    extraClickZone:
      'flex relative after:absolute after:cursor-pointer group-disabled:after:cursor-not-allowed',
    wrapper: 'relative block',
    loader: 'absolute inset-0 flex items-center justify-center',
    content: 'flex items-center gap-[0.5em]',
    text: 'inline-flex items-center justify-center gap-1 w-full',
    icon: 'h-[1.906cap] w-[1.906cap] flex-none aspect-square flex items-center justify-center',
  },
  defaultVariants: {
    isActive: 'none',
    loading: false,
    fullWidth: false,
    size: 'medium',
    rounded: 'full',
    variant: 'primary',
    iconPosition: 'start',
    showIconOnHover: false,
  },
  compoundVariants: [
    { isFocused: true, iconOnly: true, class: 'focus-visible:outline-offset-[3px]' },
    { size: 'custom', iconOnly: true, class: { icon: 'w-full h-full' } },
    { size: 'xxsmall', iconOnly: true, class: { base: 'p-0 rounded-full', icon: 'w-2.5 h-2.5' } },
    { size: 'xsmall', iconOnly: true, class: { base: 'p-0 rounded-full', icon: 'w-3 h-3' } },
    { size: 'small', iconOnly: true, class: { base: 'p-0 rounded-full', icon: 'w-3.5 h-3.5' } },
    { size: 'medium', iconOnly: true, class: { base: 'p-0 rounded-full', icon: 'w-4 h-4' } },
    { size: 'large', iconOnly: true, class: { base: 'p-0 rounded-full', icon: 'w-4.5 h-4.5' } },
    { size: 'hero', iconOnly: true, class: { base: 'p-0 rounded-full', icon: 'w-12 h-12' } },

    { variant: 'link', isFocused: true, class: 'focus-visible:outline-offset-1' },
    { variant: 'link', size: 'xxsmall', class: 'font-medium' },
    { variant: 'link', size: 'xsmall', class: 'font-medium' },
    { variant: 'link', size: 'small', class: 'font-medium' },
    { variant: 'link', size: 'medium', class: 'font-medium' },
    { variant: 'link', size: 'large', class: 'font-medium' },
    { variant: 'link', size: 'hero', class: 'font-medium' },
  ],
})

/** A button allows a user to perform an action, with mouse, touch, and keyboard interactions. */
export const Button = forwardRef(function Button(
  props: ButtonProps,
  ref: React.ForwardedRef<HTMLButtonElement>,
) {
  const {
    className,
    contentClassName,
    children,
    variant,
    icon,
    loading = false,
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
    ...ariaProps
  } = props
  const focusChildProps = focusHooks.useFocusChild()

  const [implicitlyLoading, setImplicitlyLoading] = React.useState(false)
  const contentRef = React.useRef<HTMLSpanElement>(null)
  const loaderRef = React.useRef<HTMLSpanElement>(null)

  const isLink = ariaProps.href != null

  const Tag = isLink ? aria.Link : aria.Button

  const goodDefaults = {
    ...(isLink ? { rel: 'noopener noreferrer' } : { type: 'button' as const }),
    'data-testid': testId ?? (isLink ? 'link' : 'button'),
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
  const tooltipElement = shouldShowTooltip ? tooltip ?? ariaProps['aria-label'] : null

  const isLoading = loading || implicitlyLoading
  const isDisabled = props.isDisabled ?? isLoading

  React.useLayoutEffect(() => {
    const delay = 350

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

  const handlePress = (event: aria.PressEvent): void => {
    if (!isDisabled) {
      const result = onPress?.(event)

      if (result instanceof Promise) {
        setImplicitlyLoading(true)
        void result.finally(() => {
          setImplicitlyLoading(false)
        })
      }
    }
  }

  const {
    base,
    content,
    wrapper,
    loader,
    extraClickZone,
    icon: iconClasses,
    text: textClasses,
  } = variants({
    isDisabled,
    isActive,
    loading: isLoading,
    fullWidth,
    size,
    rounded,
    variant,
    iconPosition,
    showIconOnHover,
    extraClickZone: extraClickZoneProp,
    iconOnly: isIconOnly,
  })

  const childrenFactory = (
    render: aria.ButtonRenderProps | aria.LinkRenderProps,
  ): React.ReactNode => {
    const iconComponent = (() => {
      if (icon == null) {
        return null
      } else if (isLoading && loaderPosition === 'icon') {
        return (
          <span className={iconClasses()}>
            <StatelessSpinner state={spinnerModule.SpinnerState.loadingMedium} size={16} />
          </span>
        )
      } else {
        /* @ts-expect-error any here is safe because we transparently pass it to the children, and ts infer the type outside correctly */
        const actualIcon = typeof icon === 'function' ? icon(render) : icon

        if (typeof actualIcon === 'string') {
          return <SvgMask src={actualIcon} className={iconClasses()} />
        } else {
          return <span className={iconClasses()}>{actualIcon}</span>
        }
      }
    })()
    // Icon only button
    if (isIconOnly) {
      return <span className={extraClickZone()}>{iconComponent}</span>
    } else {
      // Default button
      return (
        <>
          {iconComponent}
          <span className={textClasses()}>
            {/* @ts-expect-error any here is safe because we transparently pass it to the children, and ts infer the type outside correctly */}
            {typeof children === 'function' ? children(render) : children}
          </span>
        </>
      )
    }
  }

  const button = (
    <Tag
      // @ts-expect-error ts errors are expected here because we are merging props with different types
      {...aria.mergeProps<aria.ButtonProps>()(goodDefaults, ariaProps, focusChildProps, {
        ref,
        isDisabled,
        // we use onPressEnd instead of onPress because for some reason react-aria doesn't trigger
        // onPress on EXTRA_CLICK_ZONE, but onPress{start,end} are triggered
        onPressEnd: (e) => {
          if (!isDisabled) {
            handlePress(e)
          }
        },
        className: aria.composeRenderProps(className, (classNames, states) =>
          base({ className: classNames, ...states }),
        ),
      })}
    >
      {/* @ts-expect-error any here is safe because we transparently pass it to the children, and ts infer the type outside correctly */}
      {(render) => (
        <>
          <span className={wrapper()}>
            <span ref={contentRef} className={content({ className: contentClassName })}>
              {/* eslint-disable-next-line @typescript-eslint/no-unsafe-argument */}
              {childrenFactory(render)}
            </span>

            {isLoading && loaderPosition === 'full' && (
              <span ref={loaderRef} className={loader()}>
                <StatelessSpinner state={spinnerModule.SpinnerState.loadingMedium} size={16} />
              </span>
            )}
          </span>
        </>
      )}
    </Tag>
  )

  return tooltipElement == null ? button : (
      <ariaComponents.TooltipTrigger delay={0} closeDelay={0}>
        {button}

        <ariaComponents.Tooltip
          {...(tooltipPlacement != null ? { placement: tooltipPlacement } : {})}
        >
          {tooltipElement}
        </ariaComponents.Tooltip>
      </ariaComponents.TooltipTrigger>
    )
})
