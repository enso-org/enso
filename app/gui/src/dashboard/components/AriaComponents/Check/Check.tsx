/**
 * @file
 *
 * A checkmark icon
 * Can be used to indicate that an item is selected. Has an indeterminate state.
 */
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { motion, type Variants } from 'framer-motion'

/**
 * Variants for the {@link Check} component.
 */
/* eslint-disable @typescript-eslint/no-magic-numbers */
export const CHECK_VARIANTS: Variants = {
  checked: {
    pathLength: 1,
    opacity: 1,
    transition: { type: 'tween', duration: 0.2, easings: 'circIn' },
  },
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'pressed-checked': {
    pathLength: 0.8,
    opacity: 1,
    transition: { type: 'tween', duration: 0.2, easings: 'circIn' },
  },
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'pressed-unchecked': {
    pathLength: 0.2,
    opacity: 0.5,
    transition: { type: 'tween', duration: 0.2, easings: 'circIn' },
  },
  unchecked: {
    pathLength: 0,
    opacity: 0,
    transition: { type: 'tween', duration: 0.2, easings: 'circOut' },
  },
}
/* eslint-enable @typescript-eslint/no-magic-numbers */

export const CHECK_CLASSES = tv({
  base: ['flex-none aspect-square', 'transition-[outline-offset,border-width] duration-200'],
  variants: {
    isSelected: {
      true: { base: 'border-transparent' },
    },
    isPressed: {
      true: { base: '' },
    },
    // Defined in compoundVariants
    color: {
      custom: { base: '' },
      primary: { base: 'border-primary' },
      accent: { base: 'border-accent' },
      error: { base: 'border-danger' },
    },
    variant: {
      custom: { base: '' },
      outline: { base: 'border-[0.5px]' },
    },
    rounded: {
      custom: { base: '' },
      none: { base: 'rounded-none' },
      full: { base: 'rounded-full' },
      large: { base: 'rounded-lg' },
      medium: { base: 'rounded-md' },
      small: { base: 'rounded-sm' },
      xlarge: { base: 'rounded-xl' },
      xxlarge: { base: 'rounded-2xl' },
      xxxlarge: { base: 'rounded-3xl' },
    },
    size: {
      small: { base: 'w-3 h-3' },
      medium: { base: 'w-4 h-4' },
      large: { base: 'w-5 h-5' },
    },
  },
  slots: { path: '' },
  defaultVariants: {
    size: 'medium',
    rounded: 'medium',
    color: 'primary',
    isPressed: false,
    isSelected: false,
    isIndeterminate: false,
    variant: 'outline',
  },
  compoundVariants: [
    {
      isSelected: true,
      color: 'primary',
      class: { base: 'bg-primary text-white' },
    },
    {
      isSelected: true,
      color: 'accent',
      class: { base: 'bg-accent text-white' },
    },
    {
      isSelected: true,
      color: 'error',
      class: { base: 'bg-danger text-white' },
    },
  ],
})

/**
 * Props for the {@link Check} component.
 */
export interface CheckProps extends VariantProps<typeof CHECK_CLASSES> {
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
    variants = CHECK_CLASSES,
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
