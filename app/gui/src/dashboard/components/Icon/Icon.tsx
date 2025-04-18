/** @file Displays an icon. */
import type {
  IconProp,
  IconPropSvgUse,
  LegacyIconProp,
  TestIdProps,
} from '#/components/AriaComponents/types'
import SvgMask from '#/components/SvgMask'
import type { VariantProps } from '#/utilities/tailwindVariants'
import icons from '@/assets/icons.svg'
import { isIconName, type Icon as PossibleIcon } from '@/util/iconMetadata/iconName'
import { memo } from 'react'
import { ICON_STYLES } from './variants'

/** Props for {@link Icon}. */
export type IconProps<Render = never> = BaseIconProps<Render> &
  (LegacyIconProps<string, Render> | SvgUseIconProps<Render>)

/** Base props for all icon types. */
interface BaseIconProps<Render = never> extends VariantProps<typeof ICON_STYLES>, TestIdProps {
  readonly className?: string | undefined
  readonly renderProps?: Render
  readonly alt?: string | undefined
}

/** @deprecated Prefer defined keys over importing from `#/assets/*.svg`. */
export interface LegacyIconProps<Icon extends string, Render = never>
  extends BaseIconProps<Render> {
  readonly icon: LegacyIconProp<Icon, Render>
}

/** Generic type for icons imported from Figma. */
export interface SvgUseIconProps<Render = never> {
  readonly icon: IconPropSvgUse<Render>
}

/** Displays an icon. */
// eslint-disable-next-line no-restricted-syntax
export const Icon = memo(function Icon<Render = never>(props: IconProps<Render>) {
  const { className, variants = ICON_STYLES, size, testId, renderProps, color, alt } = props

  const styles = variants({ size, className, color })

  return (
    <IconInternal<Render>
      icon={props.icon}
      className={styles}
      testId={testId}
      renderProps={renderProps}
      alt={alt}
    />
  )
}) as <Render = never>(props: IconProps<Render>) => React.JSX.Element

/** Props for {@link IconInternal}. */
interface IconInternalProps<Render = never> extends TestIdProps {
  readonly className?: string | undefined
  readonly icon: IconProp<string, Render>
  readonly renderProps?: Render | undefined
  readonly alt?: string | undefined
}

/**
 * Internal icon component that displays an icon based on different input.
 * @internal
 */
function IconInternal<Render = never>(props: IconInternalProps<Render>) {
  const { className, testId, renderProps, icon, alt = '' } = props

  // eslint-disable-next-line no-restricted-syntax
  const renderedIcon = typeof icon === 'function' ? icon(renderProps as never) : icon

  if (renderedIcon == null || renderedIcon === false) {
    return null
  }

  if (typeof renderedIcon === 'string') {
    if (isIconName(renderedIcon)) {
      return <SvgUse icon={renderedIcon} testId={testId} className={className} alt={alt} />
    }

    return <SvgMask src={renderedIcon} className={className} testId={testId} alt={alt} />
  }

  return (
    <span className={className} data-testid={testId}>
      {renderedIcon}
    </span>
  )
}

/** Props for {@link SvgUse}. */
export interface SvgUseProps extends TestIdProps {
  readonly icon: PossibleIcon
  readonly className?: string | undefined
  readonly alt?: string | undefined
}

/**
 * A component that displays an SVG from the icons bundle file.
 * Please refer to Figma for the list of available icons.
 * Prefer using {@link Icon} instead.
 * @internal
 */
export function SvgUse(props: SvgUseProps) {
  const { icon, testId = 'svg-use', className, alt = '' } = props

  return (
    <svg
      className={className}
      data-testid={testId}
      role={alt.length > 0 ? 'img' : 'presentation'}
      viewBox="0 0 16 16"
      preserveAspectRatio="xMidYMid slice"
      aria-label={alt}
    >
      <use
        href={icon.includes(':') ? icon : `${icons}#${icon}`}
        className="h-full w-full"
        aria-hidden="true"
        data-icon={icon}
      />
    </svg>
  )
}
