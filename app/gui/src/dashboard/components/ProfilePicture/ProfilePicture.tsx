/** @file Displays a profile picture. */
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { Icon } from '../Icon'

export const PROFILE_PICTURE_STYLES = tv({
  base: 'aspect-square flex-none object-cover',
  variants: {
    size: {
      auto: 'w-auto h-auto',
      full: 'w-full h-full',
      xxsmall: 'w-2 h-2',
      xsmall: 'w-4 h-4',
      small: 'w-6 h-6',
      medium: 'w-8 h-8',
      large: 'w-10 h-10',
      xlarge: 'w-12 h-12',
      xxlarge: 'w-14 h-14',
      xxxlarge: 'w-16 h-16',
    },
    rounded: {
      full: 'rounded-full',
      none: 'rounded-none',
    },
    default: { true: 'opacity-60' },
  },
  defaultVariants: {
    size: 'medium',
    rounded: 'full',
    default: true,
  },
})

/** Props for a {@link ProfilePicture}. */
export interface ProfilePictureProps
  extends Omit<VariantProps<typeof PROFILE_PICTURE_STYLES>, 'default'> {
  /**
   * The valid `src` attribute of the profile picture. If `null`, the default user icon is displayed.
   */
  readonly picture: string | null | undefined
  /**
   * The name of the user, used as the `alt` attribute of the image.
   */
  readonly name: string
}

/**
 * Displays a profile picture.
 */
export function ProfilePicture(props: ProfilePictureProps) {
  const { picture, name, size, rounded, className, variants = PROFILE_PICTURE_STYLES } = props

  const shouldShowDefault = picture == null

  const styles = variants({ size, rounded, className, default: shouldShowDefault })

  if (shouldShowDefault) {
    return <Icon icon="default_user" className={styles} alt={name} />
  }

  return <img src={picture} alt={name} className={styles} />
}
