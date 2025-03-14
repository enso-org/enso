/** @file Variants for `Radio`. */
import { tv } from '#/utilities/tailwindVariants'

export const RADIO_GROUP_STYLES = tv({
  base: 'flex flex-col gap-0.5 items-start',
  variants: { fullWidth: { true: 'w-full' } },
})
