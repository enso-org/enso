/**
 * @file
 *
 * Icon component that displays an icon based on different input.
 */
import icons from '@/assets/icons.svg'
import { isIconName, type Icon as PossibleIcon } from '@/util/iconMetadata/iconName'

import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { memo } from 'react'
import type {
  AvailableIconReturn,
  IconProp as IconType,
  IconPropSvgUse as IconTypeSvgUse,
  LegacyAvialableIconReturn,
  LegacyIconProp as LegacyIconPropType,
  TestIdProps,
} from '../AriaComponents'
import SvgMask from '../SvgMask'

/**
 * Props for {@link Icon}.
 */
export type IconProps<Render = never> = BaseIconProps<Render> &
  (LegacyIconProps<string, Render> | SvgUseIconProps<Render>)

/**
 * Base props for all icon types.
 */
interface BaseIconProps<Render = never> extends VariantProps<typeof ICON_STYLES>, TestIdProps {
  readonly className?: string | undefined
  readonly renderProps?: Render
}

/**
 * @deprecated Prefer defined keys over importing from `#/assets/*.svg
 */
export interface LegacyIconProps<Icon extends string, Render = never>
  extends BaseIconProps<Render> {
  readonly children: LegacyIconPropType<Icon, Render>
  readonly icon?: never
}

/**
 * Generic type for imported from figma icons
 */
export interface SvgUseIconProps<Render = never> {
  readonly children?: never
  readonly icon: IconTypeSvgUse<Render>
}

export const ICON_STYLES = tv({
  base: 'flex-none aspect-square [&>svg]:stroke-current [&>svg]:w-full [&>svg]:h-full',
  variants: {
    color: {
      custom: '',
      primary: 'text-primary',
      danger: 'text-danger',
      success: 'text-accent-dark',
      accent: 'text-accent-dark',
      muted: 'text-primary/40',
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
    color: 'primary',
    size: 'medium',
  },
})

/**
 * Icon component that displays an icon based on different input.
 */
// eslint-disable-next-line no-restricted-syntax
export const Icon = memo(function Icon<Render = never>(props: IconProps<Render>) {
  const { className, variants = ICON_STYLES, size, testId, renderProps = {}, color } = props

  const styles = variants({
    size,
    className,
    color,
  })

  const getIconJsx = (ic: IconType<string, Render>) => {
    // eslint-disable-next-line no-restricted-syntax
    const renderedIcon = typeof ic === 'function' ? ic(renderProps as Render) : ic

    if (renderedIcon == null || renderedIcon === false) {
      return null
    }

    if (typeof renderedIcon === 'string') {
      if (isIconName(renderedIcon)) {
        return <SvgUse icon={renderedIcon} testId={testId} className={styles} />
      }

      return <SvgMask src={renderedIcon} className={styles} testId={testId} />
    }

    return (
      <span className={styles} data-testid={testId}>
        {renderedIcon}
      </span>
    )
  }

  if ('children' in props) {
    return <>{getIconJsx(props.children)}</>
  }

  return <>{getIconJsx(props.icon)}</>
}) as <Render = never>(props: IconProps<Render>) => React.JSX.Element

/**
 * Props for {@link SvgUse}.
 */
export interface SvgUseProps extends TestIdProps {
  readonly icon: PossibleIcon
  readonly className?: string | undefined
}

/**
 * A component that displays an SVG from the icons bundle file.
 * Please refer to Figma for the list of available icons.
 */
export function SvgUse(props: SvgUseProps) {
  const { icon, testId = 'svg-use', className } = props

  return (
    <svg
      className={className}
      viewBox="0 0 16 16"
      preserveAspectRatio="xMidYMid slice"
      data-testid={testId}
    >
      <use href={icon.includes(':') ? icon : `${icons}#${icon}`} data-icon={icon} />
    </svg>
  )
}

/**
 * Utility function to render an icon based on the icon type and render props.
 */
export function renderIcon<Icon extends string, Render>(
  icon: IconType<Icon, Render>,
  renderProps: Render,
): AvailableIconReturn | LegacyAvialableIconReturn<Icon> {
  return typeof icon === 'function' ? icon(renderProps) : icon
}
