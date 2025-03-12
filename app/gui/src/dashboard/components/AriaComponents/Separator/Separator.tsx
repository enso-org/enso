/** @file A visual separator. */
import { SEPARATOR_STYLES } from '#/components/AriaComponents/Separator/constants'
import {
  Separator as AriaSeparator,
  type SeparatorProps as AriaSeparatorProps,
} from '#/components/aria'
import type { VariantProps } from '#/utilities/tailwindVariants'

/** The props for {@link Separator} component. */
export interface SeparatorProps extends AriaSeparatorProps, VariantProps<typeof SEPARATOR_STYLES> {
  readonly className?: string | undefined
}

/** A separator component. */
export function Separator(props: SeparatorProps) {
  const {
    orientation = 'horizontal',
    variant,
    variants = SEPARATOR_STYLES,
    className,
    size,
    ...rest
  } = props

  const styles = variants({ orientation, variant, size, className })

  return <AriaSeparator orientation={orientation} className={styles} {...rest} />
}
