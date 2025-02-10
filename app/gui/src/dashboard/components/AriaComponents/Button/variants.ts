/**
 * @file
 *
 * Variants for a button
 */
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { TEXT_STYLE } from '../Text'

/**
 * Variants for a button
 */
export type ButtonVariants = VariantProps<typeof BUTTON_STYLES>

export const BUTTON_STYLES = tv({
  base: [
    'group',
    'relative',
    // basic outline
    'outline-offset-[1px] outline-transparent',
    // buttons always have borders
    // so keep them in mind when setting paddings
    'border-0.5 border-transparent',
    // button reset styles
    'whitespace-nowrap cursor-pointer select-none appearance-none',
    // Align the content by the center
    'text-center items-center justify-center',
    // animations
    'transition-[opacity,outline-offset,background,border-color] duration-150 ease-in-out',
  ],
  variants: {
    isDisabled: {
      true: 'opacity-50 cursor-not-allowed',
    },
    isFocused: {
      true: 'focus:outline-none focus-visible:outline-2 focus-visible:outline-black focus-visible:outline-offset-[-2px]',
    },
    isActive: {
      none: '',
      false:
        'disabled:opacity-30 [&.disabled]:opacity-30 disabled:cursor-not-allowed [&.disabled]:cursor-not-allowed opacity-50 hover:opacity-75',
      true: 'opacity-100 disabled:opacity-100 [&.disabled]:opacity-100 hover:opacity-100 disabled:cursor-default [&.disabled]:cursor-default',
    },
    isPressed: {
      true: '',
    },
    isJoined: {
      // Mostly defined in the compoundVariants
      true: '',
      false: '',
    },
    position: {
      // Mostly defined in the compoundVariants
      first: '',
      last: '',
      middle: '',
    },
    loading: { true: { base: 'cursor-wait' } },
    fullWidth: { true: 'w-full' },
    size: {
      custom: { base: '', extraClickZone: '', icon: 'h-full w-unset min-w-[1.906cap]' },
      hero: {
        base: TEXT_STYLE({
          variant: 'subtitle',
          color: 'custom',
          weight: 'semibold',
          className: 'flex h-16 px-[24px]',
        }),
        text: 'mx-[1.5em]',
      },
      large: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'semibold',
          className: 'flex h-9 px-[11px]',
        }),
        content: 'gap-2',
        icon: '-mb-0.5 h-4 w-4',
        extraClickZone: 'after:inset-[-6px]',
      },
      medium: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'semibold',
          className: 'flex h-8 px-[7px]',
        }),
        icon: '-mb-0.5 h-4 w-4',
        content: 'gap-2',
        extraClickZone: 'after:inset-[-8px]',
      },
      small: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'medium',
          className: 'flex h-7 px-[5px]',
        }),
        icon: '-mb-0.5 h-3.5 w-3.5',
        content: 'gap-1',
        extraClickZone: 'after:inset-[-10px]',
      },
      xsmall: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          weight: 'medium',
          className: 'flex h-6 px-[5px]',
        }),
        icon: '-mb-0.5 h-3 w-3',
        content: 'gap-1',
        extraClickZone: 'after:inset-[-12px]',
      },
      xxsmall: {
        base: TEXT_STYLE({
          variant: 'body',
          color: 'custom',
          className: 'flex h-5 px-[3px] leading-[16px]',
        }),
        content: 'gap-0.5',
        icon: 'mb-[-0.1cap]',
        extraClickZone: 'after:inset-[-12px]',
      },
    },
    iconOnly: {
      // Specified in the compoundVariants
      true: 'aspect-square',
    },
    rounded: {
      full: 'rounded-full',
      large: 'rounded-lg',
      medium: 'rounded-md',
      none: 'rounded-none',
      small: 'rounded-sm',
      xlarge: 'rounded-xl',
      xxlarge: 'rounded-2xl',
      xxxlarge: 'rounded-3xl',
    },
    variant: {
      custom: '',
      link: {
        base: 'inline-block px-0 py-0 rounded-sm text-primary/50 underline hover:text-primary border-0',
        content: 'gap-1.5',
        icon: 'h-[1.25cap] w-[1.25cap] mt-[0.25cap]',
      },
      primary: 'bg-primary text-white hover:bg-primary/70',
      accent: 'bg-accent text-white hover:bg-accent-dark',
      delete:
        'bg-danger/80 hover:bg-danger text-white focus-visible:outline-danger focus-visible:bg-danger',
      icon: {
        base: 'text-primary opacity-80 hover:opacity-100 focus-visible:opacity-100',
        wrapper: 'w-full h-full',
        content: 'w-full h-full',
        extraClickZone: 'w-full h-full',
      },
      ghost:
        'text-primary hover:text-primary/80 hover:bg-white focus-visible:text-primary/80 focus-visible:bg-white',
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'ghost-fading':
        'text-primary opacity-80 hover:opacity-100 hover:bg-white focus-visible:bg-white',
      submit: 'bg-invite text-white opacity-80 hover:opacity-100',
      outline: 'border-primary/20 text-primary hover:border-primary hover:bg-primary/5',
    },
    iconPosition: {
      start: { content: '' },
      end: { content: 'flex-row-reverse' },
    },
    showIconOnHover: {
      true: {
        icon: 'opacity-0 group-hover:opacity-100 group-focus-visible:opacity-100 disabled:opacity-0 aria-disabled:opacity-0 disabled:group-hover:opacity-50 aria-disabled:group-hover:opacity-50',
      },
    },
    extraClickZone: {
      true: {
        extraClickZone:
          'flex relative after:absolute after:cursor-pointer group-disabled:after:cursor-not-allowed',
      },
      false: {
        extraClickZone: 'after:inset-0',
      },
      xxsmall: {
        extraClickZone: 'after:inset-[-2px]',
      },
      xsmall: {
        extraClickZone: 'after:inset-[-4px]',
      },
      small: {
        extraClickZone: 'after:inset-[-6px]',
      },
      medium: {
        extraClickZone: 'after:inset-[-8px]',
      },
      large: {
        extraClickZone: 'after:inset-[-10px]',
      },
      custom: {
        extraClickZone: 'after:inset-[calc(var(--extra-click-zone-offset, 0) * -1)]',
      },
    },
  },
  slots: {
    extraClickZone:
      'flex relative after:absolute after:cursor-pointer group-disabled:after:cursor-not-allowed',
    wrapper: 'relative block',
    loader: 'absolute inset-0 flex items-center justify-center',
    content: 'flex items-center',
    text: 'inline-flex items-center justify-center gap-1 w-full',
    icon: 'h-[1.906cap] w-[1.906cap] flex-none aspect-square flex items-center justify-center',
    addonStart: 'flex items-center justify-center macos:-mb-0.5',
    addonEnd: 'flex items-center justify-center macos:-mb-0.5',
    joinSeparator: 'absolute z-1 -right-[0.5px] h-[80%] w-[0.5px] bg-current rounded-full',
  },
  defaultVariants: {
    isActive: 'none',
    loading: false,
    fullWidth: false,
    size: 'medium',
    rounded: 'full',
    variant: 'primary',
    iconPosition: 'start',
    showIconOnHover: false,
    isDisabled: false,
    extraClickZone: true,
  },
  compoundVariants: [
    { isFocused: true, iconOnly: true, class: 'focus-visible:outline-offset-[3px]' },

    {
      size: 'custom',
      iconOnly: true,
      isJoined: false,
      class: {
        base: TEXT_STYLE({
          disableLineHeightCompensation: true,
          className: 'border-0 outline-offset-[5px] p-0 rounded-full',
        }),
        icon: 'w-full h-full mb-[unset]',
      },
    },
    {
      size: 'xxsmall',
      iconOnly: true,
      isJoined: false,
      class: {
        base: TEXT_STYLE({
          disableLineHeightCompensation: true,
          className: 'border-0 outline-offset-[5px] p-0 rounded-full w-2.5 h-2.5',
        }),
        icon: 'w-[unset] h-[unset] mb-[unset]',
      },
    },
    {
      size: 'xsmall',
      iconOnly: true,
      isJoined: false,
      class: {
        base: TEXT_STYLE({
          disableLineHeightCompensation: true,
          className: 'border-0 outline-offset-[5px] p-0 rounded-full w-3 h-3',
        }),
        icon: 'w-[unset] h-[unset] mb-[unset]',
      },
    },
    {
      size: 'small',
      iconOnly: true,
      isJoined: false,
      class: {
        base: TEXT_STYLE({
          disableLineHeightCompensation: true,
          className: 'border-0 outline-offset-[4px] p-0 rounded-full w-3.5 h-3.5',
        }),
        icon: 'w-[unset] h-[unset] mb-[unset]',
      },
    },
    {
      size: 'medium',
      iconOnly: true,
      isJoined: false,
      class: {
        base: TEXT_STYLE({
          disableLineHeightCompensation: true,
          className: 'border-0 outline-offset-[4px] p-0 rounded-full w-4 h-4',
        }),
        icon: 'w-[unset] h-[unset] mb-[unset]',
      },
    },
    {
      size: 'large',
      iconOnly: true,
      isJoined: false,
      class: {
        base: TEXT_STYLE({
          disableLineHeightCompensation: true,
          className: 'border-0 outline-offset-[4px] p-0 rounded-full w-5 h-5',
        }),
        icon: 'w-[unset] h-[unset] mb-[unset]',
      },
    },
    {
      size: 'hero',
      iconOnly: true,
      isJoined: false,
      class: {
        base: TEXT_STYLE({
          disableLineHeightCompensation: true,
          className: 'border-0 outline-offset-[5px] p-0 rounded-full w-12 h-12',
        }),
        icon: 'w-[unset] h-[unset] mb-[unset]',
      },
    },

    { size: 'xsmall', class: { addonStart: '-ml-[3.5px]', addonEnd: '-mr-[3.5px]' } },
    { size: 'xxsmall', class: { addonStart: '-ml-[2.5px]', addonEnd: '-mr-[2.5px]' } },

    { variant: 'icon', class: { base: 'flex-none' } },
    { variant: 'icon', isDisabled: true, class: { base: 'opacity-50 cursor-not-allowed' } },

    { variant: 'link', isFocused: true, class: 'focus-visible:outline-offset-1' },
    { variant: 'link', size: 'xxsmall', class: 'font-medium' },
    { variant: 'link', size: 'xsmall', class: 'font-medium' },
    { variant: 'link', size: 'small', class: 'font-medium' },
    { variant: 'link', size: 'medium', class: 'font-medium' },
    { variant: 'link', size: 'large', class: 'font-medium' },
    { variant: 'link', size: 'hero', class: 'font-medium' },

    { variant: 'icon', isDisabled: true, class: 'opacity-50' },

    { isJoined: true, position: 'first', class: { base: 'rounded-r-none' } },
    { isJoined: true, position: 'last', class: { base: 'rounded-l-none' } },
    { isJoined: true, position: 'middle', class: { base: 'rounded-none' } },

    { isJoined: true, variant: 'link', class: { joinSeparator: 'hidden' } },

    { isJoined: true, variant: 'icon', class: { extraClickZone: 'items-center' } },

    {
      isJoined: true,
      position: ['first', 'middle'],
      variant: 'primary',
      class: {
        joinSeparator: 'text-background',
      },
    },
    {
      isJoined: true,
      position: ['first', 'middle'],
      variant: 'accent',
      class: {
        joinSeparator: 'text-background',
      },
    },

    {
      isJoined: true,
      position: ['first', 'middle'],
      variant: 'delete',
      class: {
        joinSeparator: 'text-background',
      },
    },

    {
      isJoined: true,
      position: ['first', 'middle'],
      variant: 'ghost',
      class: {
        joinSeparator: 'text-primary/20',
      },
    },
    {
      isJoined: true,
      position: ['first', 'middle'],
      variant: 'ghost-fading',
      class: {
        joinSeparator: 'text-primary/20',
      },
    },

    {
      isJoined: true,
      position: ['first', 'middle'],
      variant: 'outline',
      class: {
        base: 'mr-[-0.5px] border-r-primary/10',
        joinSeparator: 'hidden',
      },
    },
    {
      isJoined: true,
      position: ['first', 'middle'],
      variant: 'submit',
      class: {
        joinSeparator: 'text-background',
      },
    },
    {
      isJoined: true,
      position: ['first', 'middle'],
      variant: 'icon',
      class: { joinSeparator: 'text-primary/20 h-[50%]' },
    },
  ],
})
