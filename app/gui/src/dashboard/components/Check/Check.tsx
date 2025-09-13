/** @file A checkmark icon. */
import { tv, type VariantProps } from '#/utilities/tailwindVariants'

// eslint-disable-next-line react-refresh/only-export-components
export const CHECK_CLASSES = tv({
  base: ['flex-none aspect-square', 'transition-[outline-offset,border-width] duration-200'],
  variants: {
    isSelected: {
      true: { base: 'border-transparent' },
      false: '',
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

/** Props for a {@link Check}. */
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
    isIndeterminate = false,
    variants = CHECK_CLASSES,
    className,
    color,
    rounded,
    size,
  } = props

  const styles = variants({ isSelected, className, color, rounded, size })

  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 16 16"
      className={styles.base()}
      role="presentation"
      pointerEvents="none"
    >
      <path
        className={styles.path()}
        strokeLinecap="round"
        strokeLinejoin="round"
        strokeWidth="2"
        stroke="currentColor"
        fill="none"
        d={
          isIndeterminate ? 'M5 8H11'
          : isSelected ?
            'M4 8.4L6.5 10.9L9.25 8.15L12 5.4'
          : ''
        }
      />
    </svg>
  )
}
