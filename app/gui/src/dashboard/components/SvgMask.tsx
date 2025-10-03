/** @file File containing SVG icon definitions. */
import type { TestIdProps } from '#/components/types'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import * as React from 'react'

/** Props for a {@link SvgMask}. */
export interface SvgMaskProps extends TestIdProps {
  readonly invert?: boolean
  readonly alt?: string
  /** The URL of the SVG to use as the mask. */
  readonly src: string
  readonly title?: string
  readonly style?: React.CSSProperties
  readonly color?: string | undefined
  readonly className?: string | undefined
}

/**
 * Use an SVG as a mask. This lets the SVG use the text color (`currentColor`).
 * @deprecated Prefer `<Icon />` or `<SvgUse />` instead.
 */
const SvgMask = React.forwardRef(function SvgMask(
  props: SvgMaskProps,
  ref: React.ForwardedRef<HTMLDivElement>,
) {
  const { invert = false, alt = '', src, style, color, className, testId } = props
  const urlSrc = `url(${JSON.stringify(src)})`
  const mask = invert ? `${urlSrc}, linear-gradient(white 0 0)` : urlSrc

  const classes = tailwindMerge.twMerge('inline-block h-4 w-4 flex-none', className)

  const ariaProps =
    // eslint-disable-next-line @typescript-eslint/naming-convention
    alt === '' ? { role: 'presentation', 'aria-hidden': true } : { role: 'img', 'aria-label': alt }

  return (
    <div
      data-testid={testId}
      ref={ref}
      {...ariaProps}
      style={{
        ...(style ?? {}),
        backgroundColor: color ?? 'currentcolor',
        maskImage: mask,
        maskPosition: 'center',
        maskRepeat: 'no-repeat',
        maskSize: 'contain',
        ...(invert ? { maskComposite: 'exclude, exclude' } : {}),
        // The names come from a third-party API and cannot be changed.
        /* eslint-disable @typescript-eslint/naming-convention */
        WebkitMaskImage: mask,
        WebkitMaskPosition: 'center',
        WebkitMaskRepeat: 'no-repeat',
        WebkitMaskSize: 'contain',
        ...(invert ? { WebkitMaskComposite: 'exclude, exclude' } : {}),
        /* eslint-enable @typescript-eslint/naming-convention */
      }}
      className={classes}
    />
  )
})

/**
 * @deprecated Prefer `<Icon />` or `<SvgUse />` instead.
 */
export default React.memo(SvgMask)
