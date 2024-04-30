/**
 * @file Text component
 */
import * as React from 'react'

import * as tw from 'tailwind-merge'

import * as aria from '#/components/aria'
import Portal from '#/components/Portal'

import * as mergeRefs from '#/utilities/mergeRefs'

/**
 * Props for the Text component
 */
export interface TextProps extends aria.TextProps {
  readonly variant?: TextVariant
  /**
   * Whether the text uses the monospace font.
   */
  readonly monospace?: boolean
  /**
   * Whether the text overflow is ellipsis
   */
  readonly ellipsis?: boolean
  /**
   * The number of lines to clamp the text to
   * Requires `ellipsis` to be `true`
   * Use `1` for single line truncation
   * Using more than 1 line will transform the text to a flex element
   * @default 1
   */
  readonly lineClamp?: number
  /**
   * Whether the text is not wrapping
   */
  readonly nowrap?: boolean
  /**
   * Whether the text has italic style
   */
  readonly italic?: boolean

  readonly weight?: Weight
  readonly transform?: TextTransform
}

/**
 * Text variants
 */
export type TextVariant =
  | 'body'
  | 'caption'
  | 'custom'
  | 'h1'
  | 'h2'
  | 'h3'
  | 'h4'
  | 'h5'
  | 'h6'
  | 'subtitle'

/**
 * Font weight options
 */
export type Weight = 'bold' | 'custom' | 'extraBold' | 'normal' | 'thin'
/**
 * Text transform options
 */
export type TextTransform = 'capitalize' | 'lowercase' | 'none' | 'uppercase'

const BASIC_CLASSES = 'inline-block text-balance'
const TRUNCATE_CLASSES = 'truncate'
const MULTILINE_TRUNCATE_CLASSES = 'line-clamp-1'

const VARIANT_MAP: Record<TextVariant, string> = {
  custom: '',
  body: 'text-base font-medium leading-[24px]',
  caption: 'text-sm font-medium leading-[18px]',
  h1: 'text-4xl font-bold leading-[42px]',
  h2: 'text-2xl font-semibold leading-[36px]',
  h3: 'text-xl font-semibold leading-[30px]',
  h4: 'text-lg font-medium leading-[26px]',
  h5: 'text-h5 font-medium leading-[22px]',
  h6: 'text-h6 font-medium leading-[20px]',
  subtitle: 'text-subtitle font-medium leading-[16px]',
}

const WEIGHT_MAP: Record<Weight, string> = {
  bold: 'font-bold',
  extraBold: 'font-extrabold',
  normal: 'font-normal',
  thin: 'font-thin',
  custom: '',
}

const TRANSFORM_MAP: Record<TextTransform, string> = {
  none: '',
  capitalize: 'text-capitalize',
  lowercase: 'text-lowercase',
  uppercase: 'text-uppercase',
}

/**
 * Text component
 */
export const Text = React.forwardRef(function Text(
  props: TextProps,
  ref: React.Ref<HTMLSpanElement>
) {
  const {
    className,
    variant = 'body',
    italic = false,
    weight = 'normal',
    nowrap = false,
    ellipsis = false,
    monospace = false,
    transform = 'none',
    lineClamp = 1,
    children,
    ...ariaProps
  } = props

  const textElementRef = React.useRef<HTMLElement>(null)
  const popoverRef = React.useRef<HTMLDivElement>(null)

  const { hoverProps } = aria.useHover({
    onHoverStart: () => {
      if (textElementRef.current && popoverRef.current) {
        const isOverflowing =
          textElementRef.current.scrollWidth > textElementRef.current.clientWidth ||
          textElementRef.current.scrollHeight > textElementRef.current.clientHeight

        if (isOverflowing) {
          popoverRef.current.showPopover()
          updatePosition()
        }
      }
    },
    onHoverEnd: () => popoverRef.current?.hidePopover(),
    isDisabled: !ellipsis,
  })

  const { overlayProps, updatePosition } = aria.useOverlayPosition({
    overlayRef: popoverRef,
    targetRef: textElementRef,
  })

  const id = React.useId()

  const classes = tw.twMerge(
    BASIC_CLASSES,
    TRANSFORM_MAP[transform],
    WEIGHT_MAP[weight],
    VARIANT_MAP[variant],
    italic && 'italic',
    nowrap && 'whitespace-nowrap',
    ellipsis && lineClamp === 1 ? TRUNCATE_CLASSES : '',
    ellipsis && lineClamp > 1 ? MULTILINE_TRUNCATE_CLASSES : '',
    monospace && 'font-mono',
    className
  )

  return (
    <>
      <aria.Text
        ref={mergeRefs.mergeRefs(ref, textElementRef)}
        className={classes}
        {...aria.mergeProps<TextProps>()(
          ariaProps,
          hoverProps,
          ellipsis ? { popovertarget: id, style: { WebkitLineClamp: `${lineClamp}` } } : {}
        )}
      >
        {children}
      </aria.Text>

      {ellipsis && (
        <div
          ref={popoverRef}
          className={tw.twMerge(
            'inset-[unset] m-[unset] h-auto max-h-12 w-auto max-w-64 rounded-md bg-neutral-800 p-2 text-xs shadow-lg',
            'text-white'
          )}
          id={id}
          popover=""
          {...overlayProps}
        >
          {children}
        </div>
      )}
    </>
  )
})
