/** @file File containing SVG icon definitions. */
import * as React from 'react'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

// ===============
// === SvgMask ===
// ===============

/** Props for a {@link SvgMask}. */
export interface SvgMaskProps {
  readonly invert?: boolean
  readonly alt?: string
  /** The URL of the SVG to use as the mask. */
  readonly src: string
  readonly title?: string
  readonly style?: React.CSSProperties
  // Allowing `undefined` is fine here as this prop has a fallback.
  // eslint-disable-next-line no-restricted-syntax
  readonly color?: string | undefined
  // Allowing `undefined` is fine here as this prop is being transparently passed through to the
  // underlying `div`.
  // eslint-disable-next-line no-restricted-syntax
  readonly className?: string | undefined
  readonly onPress?: () => void
}

/** Use an SVG as a mask. This lets the SVG use the text color (`currentColor`). */
export default function SvgMask(props: SvgMaskProps) {
  const { invert = false, alt, src, style, color, className, onPress } = props
  const urlSrc = `url(${JSON.stringify(src)})`
  const mask = invert ? `${urlSrc}, linear-gradient(white 0 0)` : urlSrc
  const ref = React.useRef<HTMLButtonElement>(null)

  const noop = () => {}

  const image = (
    <aria.Button
      ref={ref}
      isDisabled={onPress == null}
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
      className={`inline-block ${onPress != null ? 'cursor-pointer' : ''} ${
        className ?? 'h-max w-max'
      }`}
      onPress={onPress ?? noop}
    >
      {/* This is required for this component to have the right size. */}
      <img alt={alt} src={src} className="transparent" draggable={false} />
    </aria.Button>
  )

  return alt == null && onPress == null ? (
    image
  ) : (
    <aria.TooltipTrigger>
      {image}
      <ariaComponents.Tooltip>{alt}</ariaComponents.Tooltip>
    </aria.TooltipTrigger>
  )
}
