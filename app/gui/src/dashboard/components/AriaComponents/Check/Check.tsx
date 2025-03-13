/**
 * @file
 *
 * A checkmark icon
 * Can be used to indicate that an item is selected. Has an indeterminate state.
 */
import { CHECK_STYLES, CHECK_VARIANTS } from '#/components/AriaComponents/Check/constants'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { motion } from 'framer-motion'

/** Props for the {@link Check} component. */
export interface CheckProps extends VariantProps<typeof CHECK_STYLES> {
  readonly className?: string | undefined
  readonly isIndeterminate?: boolean | undefined
}

/**
 * A checkmark icon
 * Can be used to indicate that an item is selected. Has an indeterminate state.
 */
export function Check(props: CheckProps) {
  const {
    isSelected = false,
    isPressed = false,
    isIndeterminate = false,
    variants = CHECK_STYLES,
    className,
    color,
    rounded,
    size,
  } = props

  const styles = variants({ isSelected, className, color, rounded, size, isPressed })

  const animate = () => {
    if (isPressed) {
      return isSelected ? 'pressed-checked' : 'pressed-unchecked'
    }

    if (isSelected) {
      return 'checked'
    }

    return 'unchecked'
  }

  return (
    <motion.svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 16 16"
      className={styles.base()}
      initial={false}
      animate={animate()}
      role="presentation"
      pointerEvents="none"
    >
      <motion.path
        className={styles.path()}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="2"
        stroke="currentColor"
        fill="none"
        d={isIndeterminate ? 'M5 8H11' : 'M4 8.4L6.5 10.9L9.25 8.15L12 5.4'}
        variants={CHECK_VARIANTS}
      />
    </motion.svg>
  )
}
