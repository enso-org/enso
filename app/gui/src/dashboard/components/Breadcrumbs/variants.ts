/** @file Variants for `Breadcrumbs`. */
import { tv } from '#/utilities/tailwindVariants'

export const BREADCRUMBS_STYLES = tv({
  base: 'flex items-center w-full',
  slots: { separator: 'text-primary last:hidden w-2.5 h-2.5 mt-[0.5px]' },
})

export const BREADCRUMB_ITEM_STYLES = tv({
  base: 'flex items-center gap-2 bg-transparent transition-colors rounded-4xl drop-target-after',
  slots: {
    link: 'block max-w-48 min-w-4 w-auto',
    more: 'aspect-square',
    container: 'flex items-center gap-2',
    iconDisplay: 'h-8',
  },
  variants: {
    isCurrent: {
      true: { link: 'flex justify-center px-2 h-8' },
    },
  },
  defaultVariants: {
    isCurrent: false,
  },
})
