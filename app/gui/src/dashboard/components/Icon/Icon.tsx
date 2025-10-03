/**
 * @file
 *
 * Icon component that displays an icon based on different input.
 */
import type {
  AvailableIconReturn,
  IconProp,
  IconPropSvgUse,
  LegacyAvailableIconReturn,
  LegacyIconProp,
  TestIdProps,
} from '#/components/types'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { isIconName, type Icon as PossibleIcon } from '@/util/iconMetadata/iconName'
import { svgUseHref } from '@/util/icons'
import { memo } from 'react'
import SvgMask from '../SvgMask'

/** Props for {@link Icon}. */
export type IconProps<Render = never> = BaseIconProps<Render> &
  (LegacyIconProps<string, Render> | SvgUseIconProps<Render>)

/** Base props for all icon types. */
interface BaseIconProps<Render = never> extends VariantProps<typeof ICON_STYLES>, TestIdProps {
  readonly className?: string | undefined
  readonly renderProps?: Render
  readonly alt?: string | undefined
}

/**
 * @deprecated Prefer defined keys over importing from `#/assets/*.svg
 */
export interface LegacyIconProps<Icon extends string, Render = never>
  extends BaseIconProps<Render> {
  readonly icon: LegacyIconProp<Icon, Render>
}

/** Generic type for icons imported from Figma. */
export interface SvgUseIconProps<Render = never> {
  readonly icon: IconPropSvgUse<Render>
}

// eslint-disable-next-line react-refresh/only-export-components
export const ICON_COLORS = [
  'custom',
  'primary',
  'danger',
  'success',
  'accent',
  'muted',
  'disabled',
  'invert',
  'inherit',
  'current',
] as const satisfies readonly VariantProps<typeof ICON_STYLES>['color'][]

// eslint-disable-next-line react-refresh/only-export-components
export const ICON_STYLES = tv({
  base: 'flex-none aspect-square w-full h-full [&>svg]:stroke-current [&>svg]:w-full [&>svg]:h-full',
  variants: {
    color: {
      custom: '',
      primary: 'text-primary',
      danger: 'text-danger',
      success: 'text-accent-dark',
      accent: 'text-accent-dark',
      muted: 'text-primary/50',
      disabled: 'text-disabled',
      invert: 'text-invert',
      inherit: 'text-inherit',
      current: 'text-current',
    },
    size: {
      xsmall: 'h-2 w-2',
      small: 'h-3 w-3',
      medium: 'h-4 w-4',
      large: 'h-5 w-5',
      xlarge: 'h-6 w-6',
      xxlarge: 'h-7 w-7',
      xxxlarge: 'h-8 w-8',
      xxxxlarge: 'h-9 w-9',
      full: 'h-full w-full',
    },
  },
  defaultVariants: {
    color: 'current',
    size: 'medium',
  },
})

/** Icon component that displays an icon based on different input. */
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

/**
 * Props for {@link SvgUse}.
 */
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
  const { icon, testId, className, alt = '' } = props

  return (
    <svg
      className={className}
      data-testid={testId}
      role={alt.length > 0 ? 'img' : 'presentation'}
      viewBox="0 0 16 16"
      preserveAspectRatio="xMidYMid slice"
      aria-label={alt}
    >
      <use href={svgUseHref(icon)} className="h-full w-full" aria-hidden="true" data-icon={icon} />
    </svg>
  )
}

/**
 * Utility function to render an icon based on the icon type and render props.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function renderIcon<Icon extends string, Render>(
  icon: IconProp<Icon, Render>,
  renderProps: Render,
): AvailableIconReturn | LegacyAvailableIconReturn<Icon> {
  return typeof icon === 'function' ? icon(renderProps) : icon
}
