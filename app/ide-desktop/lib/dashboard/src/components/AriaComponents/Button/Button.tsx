/** @file A styled button. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'
import * as twv from 'tailwind-variants'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Spinner, * as spinnerModule from '#/components/Spinner'
import SvgMask from '#/components/SvgMask'

import * as text from '../Text'

// ==============
// === Button ===
// ==============

/** Props for a {@link Button}. */
export type ButtonProps =
  | (BaseButtonProps & Omit<aria.ButtonProps, 'children' | 'onPress' | 'type'> & PropsWithoutHref)
  | (BaseButtonProps & Omit<aria.LinkProps, 'children' | 'onPress' | 'type'> & PropsWithHref)

/**
 * Props for a button with an href.
 */
interface PropsWithHref {
  readonly href?: string
  readonly type?: never
}

/**
 * Props for a button without an href.
 */
interface PropsWithoutHref {
  readonly type?: 'button' | 'reset' | 'submit'
  readonly href?: never
}

/**
 * Base props for a button.
 */
export interface BaseButtonProps extends Omit<twv.VariantProps<typeof BUTTON_STYLES>, 'iconOnly'> {
  /** Falls back to `aria-label`. Pass `false` to explicitly disable the tooltip. */
  readonly tooltip?: React.ReactElement | string | false
  /**
   * The icon to display in the button
   */
  readonly icon?: React.ReactElement | string | null
  /**
   * When `true`, icon will be shown only when hovered.
   */
  readonly showIconOnHover?: boolean
  /**
   * Handler that is called when the press is released over the target.
   * If the handler returns a promise, the button will be in a loading state until the promise resolves.
   */
  readonly onPress?: (event: aria.PressEvent) => Promise<void> | void
  readonly contentClassName?: string
  readonly children?: React.ReactNode
  readonly testId?: string

  readonly formnovalidate?: boolean
}

export const BUTTON_STYLES = twv.tv({
  base: 'group flex h-[max-content] whitespace-nowrap cursor-pointer border border-transparent transition-[opacity,outline-offset,background,border-color] duration-150 ease-in-out select-none text-center items-center justify-center appearance-none',
  variants: {
    isDisabled: { true: 'disabled:opacity-50 disabled:cursor-not-allowed' },
    isFocused: {
      true: 'focus:outline-none focus-visible:outline focus-visible:outline-primary focus-visible:outline-offset-2',
    },
    loading: { true: { base: 'cursor-wait' } },
    fullWidth: { true: 'w-full' },
    size: {
      custom: { base: '', extraClickZone: 'after:inset-[-12px]', icon: 'h-full' },
      hero: { base: 'px-8 py-4 text-lg font-bold', content: 'gap-[0.75em]' },
      large: {
        base: 'px-[11px] py-[5px]',
        content: 'gap-2',
        text: text.TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'bold',
        }),
        extraClickZone: 'after:inset-[-6px]',
      },
      medium: {
        base: 'px-[9px] py-[3px]',
        text: text.TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'bold',
        }),
        content: 'gap-2',
        extraClickZone: 'after:inset-[-8px]',
      },
      small: {
        base: 'px-[7px] py-[1px]',
        content: 'gap-1',
        text: text.TEXT_STYLE({
          variant: 'body',
          color: 'custom',
        }),
        extraClickZone: 'after:inset-[-10px]',
      },
      xsmall: {
        base: 'px-[5px] py-[1px]',
        content: 'gap-1',
        text: text.TEXT_STYLE({
          variant: 'body',
          color: 'custom',
        }),
        extraClickZone: 'after:inset-[-12px]',
      },
      xxsmall: {
        base: 'px-[3px] py-[0px]',
        content: 'gap-0.5',
        text: text.TEXT_STYLE({
          variant: 'body',
          color: 'custom',
        }),
        extraClickZone: 'after:inset-[-12px]',
      },
    },
    iconOnly: { true: { base: '' } },
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
      custom: 'focus-visible:outline-offset-2',
      link: {
        base: 'inline-flex px-0 py-0 rounded-sm text-primary/50 underline hover:text-primary border-none',
        icon: 'h-[1.25cap] mt-[0.25cap]',
      },
      primary: 'bg-primary text-white hover:bg-primary/70',
      tertiary:
        'relative text-white before:absolute before:-inset-px before:rounded-full before:bg-accent before:transition-all hover:before:brightness-90 *:relative',
      cancel: 'bg-white/50 hover:bg-white',
      delete:
        'bg-danger/80 hover:bg-danger text-white focus-visible:outline-danger focus-visible:bg-danger',
      icon: {
        base: 'opacity-80 hover:opacity-100 focus-visible:opacity-100',
        wrapper: 'w-full h-full',
        content: 'w-full h-full',
        extraClickZone: 'w-full h-full',
      },
      ghost:
        'opacity-80 hover:opacity-100 hover:bg-white focus-visible:opacity-100 focus-visible:bg-white',
      submit: 'bg-invite text-white opacity-80 hover:opacity-100 focus-visible:outline-offset-2',
      outline: 'border-primary/40 text-primary hover:border-primary focus-visible:outline-offset-2',
      bar: 'rounded-full border-0.5 border-primary/20 transition-colors hover:bg-primary/10',
    },
    iconPosition: {
      start: { content: '' },
      end: { content: 'flex-row-reverse' },
    },
    showIconOnHover: {
      true: { icon: 'opacity-0 group-hover:opacity-100 group-focus-visible:opacity-100' },
    },
  },
  slots: {
    extraClickZone: 'flex relative after:absolute after:cursor-pointer',
    wrapper: 'relative block',
    loader: 'absolute inset-0 flex items-center justify-center',
    content: 'flex items-center gap-[0.5em]',
    text: 'inline-flex items-center gap-1',
    icon: 'h-[2cap] flex-none aspect-square',
  },
  defaultVariants: {
    loading: false,
    fullWidth: false,
    size: 'xsmall',
    rounded: 'full',
    variant: 'primary',
    iconPosition: 'start',
    showIconOnHover: false,
  },
  compoundVariants: [
    { isFocused: true, iconOnly: true, class: 'focus-visible:outline-offset-3' },
    {
      variant: 'link',
      isFocused: true,
      class: 'focus-visible:outline-offset-1',
    },
    {
      size: 'xxsmall',
      class: { base: 'p-0 rounded-full', icon: 'h-[1.25cap] -mt-[0.1cap]' },
      iconOnly: true,
    },
    {
      size: 'xsmall',
      class: { base: 'p-0 rounded-full', icon: 'h-[1.45cap] -mt-[0.1cap]' },
      iconOnly: true,
    },
    {
      size: 'small',
      class: { base: 'p-0 rounded-full', icon: 'h-[1.65cap] -mt-[0.1cap]' },
      iconOnly: true,
    },
    {
      size: 'medium',
      class: { base: 'p-0 rounded-full', icon: 'h-[2cap] -mt-[0.1cap]' },
      iconOnly: true,
    },
    {
      size: 'large',
      class: { base: 'p-0 rounded-full', icon: 'h-[2.25cap] -mt-[0.1cap]' },
      iconOnly: true,
    },
    {
      size: 'hero',
      class: { base: 'p-0 rounded-full', icon: 'h-[2.5cap] -mt-[0.1cap]' },
      iconOnly: true,
    },
    { variant: 'link', size: 'xxsmall', class: 'font-medium' },
    { variant: 'link', size: 'xsmall', class: 'font-medium' },
    { variant: 'link', size: 'small', class: 'font-medium' },
    { variant: 'link', size: 'medium', class: 'font-medium' },
    { variant: 'link', size: 'large', class: 'font-medium' },
    { variant: 'link', size: 'hero', class: 'font-medium' },
  ],
})

/** A button allows a user to perform an action, with mouse, touch, and keyboard interactions. */
export const Button = React.forwardRef(function Button(
  props: ButtonProps,
  ref: React.ForwardedRef<HTMLButtonElement>
) {
  const {
    className,
    contentClassName,
    children,
    variant,
    icon,
    loading = false,
    isDisabled,
    showIconOnHover,
    iconPosition,
    size,
    fullWidth,
    rounded,
    tooltip,
    testId,
    onPress = () => {},
    ...ariaProps
  } = props
  const focusChildProps = focusHooks.useFocusChild()

  const [implicitlyLoading, setImplicitlyLoading] = React.useState(false)
  const contentRef = React.useRef<HTMLSpanElement>(null)
  const loaderRef = React.useRef<HTMLSpanElement>(null)

  const isLink = ariaProps.href != null

  const Tag = isLink ? aria.Link : aria.Button

  const goodDefaults = {
    ...(isLink ? { rel: 'noopener noreferrer' } : {}),
    ...(isLink ? {} : { type: 'button' as const }),
    'data-testid': testId ?? (isLink ? 'link' : 'button'),
  }
  const isIconOnly = (children == null || children === '' || children === false) && icon != null
  const shouldShowTooltip = isIconOnly && tooltip !== false
  const tooltipElement = shouldShowTooltip ? tooltip ?? ariaProps['aria-label'] : null

  const isLoading = loading || implicitlyLoading

  React.useLayoutEffect(() => {
    const delay = 350

    if (isLoading) {
      const loaderAnimation = loaderRef.current?.animate(
        [{ opacity: 0 }, { opacity: 0, offset: 1 }, { opacity: 1 }],
        { duration: delay, easing: 'linear', delay: 0, fill: 'forwards' }
      )
      const contentAnimation = contentRef.current?.animate([{ opacity: 1 }, { opacity: 0 }], {
        duration: 0,
        easing: 'linear',
        delay,
        fill: 'forwards',
      })

      return () => {
        loaderAnimation?.cancel()
        contentAnimation?.cancel()
      }
    } else {
      return () => {}
    }
  }, [isLoading])

  const handlePress = (event: aria.PressEvent): void => {
    if (!isLoading) {
      const result = onPress(event)

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
  } = BUTTON_STYLES({
    isDisabled,
    loading: isLoading,
    fullWidth,
    size,
    rounded,
    variant,
    iconPosition,
    showIconOnHover,
    iconOnly: isIconOnly,
  })

  const childrenFactory = (): React.ReactNode => {
    const iconComponent = (() => {
      if (icon == null) {
        return null
      } else if (typeof icon === 'string') {
        return <SvgMask src={icon} className={iconClasses()} />
      } else {
        return <span className={iconClasses()}>{icon}</span>
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
          <span className={textClasses()}>{children}</span>
        </>
      )
    }
  }

  const button = (
    <Tag
      {...aria.mergeProps<aria.ButtonProps | aria.LinkProps>()(
        goodDefaults,
        ariaProps,
        focusChildProps,
        {
          // eslint-disable-next-line no-restricted-syntax
          ...{ ref: ref as never },
          isDisabled,
          // we use onPressEnd instead of onPress because for some reason react-aria doesn't trigger
          // onPress on EXTRA_CLICK_ZONE, but onPress{start,end} are triggered
          onPressEnd: handlePress,
          className: aria.composeRenderProps(className, (classNames, states) =>
            base({ className: classNames, ...states })
          ),
        }
      )}
    >
      <span className={wrapper()}>
        <span ref={contentRef} className={tailwindMerge.twMerge(content(), contentClassName)}>
          {childrenFactory()}
        </span>

        {isLoading && (
          <span ref={loaderRef} className={loader()}>
            <Spinner state={spinnerModule.SpinnerState.loadingMedium} size={16} />
          </span>
        )}
      </span>
    </Tag>
  )

  return tooltipElement == null ? (
    button
  ) : (
    <ariaComponents.TooltipTrigger delay={0} closeDelay={0}>
      {button}
      <ariaComponents.Tooltip>{tooltipElement}</ariaComponents.Tooltip>
    </ariaComponents.TooltipTrigger>
  )
})
