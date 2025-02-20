/** @file File containing SVG icon definitions. */
import * as React from 'react'

import * as tailwindMerge from '#/utilities/tailwindMerge'
import type { TestIdProps } from './AriaComponents'

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
  const { invert = false, alt = '', src, style, color, className, testId = 'svg-mask' } = props
  const urlSrc = `url(${JSON.stringify(src)})`
  const mask = invert ? `${urlSrc}, linear-gradient(white 0 0)` : urlSrc

  const classes = React.useMemo(
    () => tailwindMerge.twMerge('inline-block h-max w-max flex-none', className),
    [className],
  )

  return (
    <div
      data-testid={testId}
      ref={ref}
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
    >
      {/* This is required for this component to have the right size. */}
      <img alt={alt} src={src} className="pointer-events-none opacity-0" draggable={false} />
    </div>
  )
})

/**
 * @deprecated Prefer `<Icon />` or `<SvgUse />` instead.
 */
export default React.memo(SvgMask)
