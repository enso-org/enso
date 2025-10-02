/** @file Displays a profile picture. */
import type { VariantProps } from '#/utilities/tailwindVariants'
import { Icon } from '../Icon'
import { PROFILE_PICTURE_STYLES } from './variants'

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
  /**
   * The class name to apply to the component.
   */
  readonly className?: string | undefined
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
