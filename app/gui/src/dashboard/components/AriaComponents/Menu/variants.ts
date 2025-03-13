/** @file Variants for `MenuItem`. */
import { TEXT_STYLE } from '#/components/AriaComponents/Text'
import { tv } from '#/utilities/tailwindVariants'

export const MENU_ITEM_STYLES = tv({
  base: 'group flex w-full cursor-default gap-3 rounded-3xl px-[14px] py-1 outline-none transition-colors duration-75 text-left',
  variants: {
    isDisabled: { true: 'cursor-not-allowed', false: '' },
    isPressed: { true: 'bg-primary/5' },
  },
  slots: {
    checkContainer: 'block',
    icon: 'flex-none h-4 w-4',
    submenuIndicator: 'flex-none h-4 w-4 self-center text-primary',
    shortcut: 'self-center text-primary mt-[1px]',
    title: 'block w-full flex-1',
    description: 'block w-full flex-1',
    hover: 'bg-primary/5 w-full rounded-3xl',
    customContent: TEXT_STYLE({
      className: 'flex flex-1 min-w-0 w-full text-primary',
    }),
  },
  compoundSlots: [{ slots: ['checkContainer', 'icon'], className: 'mt-[3.5px] text-primary' }],
  defaultVariants: { isDisabled: false, isSelected: false },
})
