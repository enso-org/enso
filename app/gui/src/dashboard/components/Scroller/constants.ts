/** @file Constants for `Scroller`. */
import { tv } from '#/utilities/tailwindVariants'

export const SCROLLER_STYLES = tv({
  base: 'relative w-auto min-w-0',
  variants: {
    scrollbar: {
      false: {
        content: 'no-scrollbar',
      },
    },
    orientation: {
      horizontal: {
        content: '',
      },
      vertical: {
        content: '',
      },
    },
    snap: {
      true: {
        content: '',
      },
      false: {
        content: '',
      },
    },
    startHidden: {
      true: {
        shadowStart: 'opacity-0',
      },
      false: {
        shadowStart: 'opacity-100',
      },
    },
    endHidden: {
      true: {
        shadowEnd: 'opacity-0',
      },
      false: {
        shadowEnd: 'opacity-100',
      },
    },
    showShadows: {
      true: {
        shadowStart: '',
        shadowEnd: '',
      },
      false: {
        shadowStart: 'hidden',
        shadowEnd: 'hidden',
      },
    },
  },

  slots: {
    content: '',
    shadowStart: 'pointer-events-none absolute from-dashboard transition-opacity',
    shadowEnd: 'pointer-events-none absolute from-dashboard transition-opacity',
  },

  compoundVariants: [
    {
      orientation: 'horizontal',
      snap: true,
      class: {
        content: 'snap-x snap-proximity',
      },
    },
    {
      orientation: 'horizontal',
      class: {
        content: 'overflow-x-auto min-w-0 max-w-full',
        shadowStart: 'top-0 bottom-0 left-0 w-10',
        shadowEnd: 'top-0 bottom-0 right-0 w-10',
      },
    },
    {
      orientation: 'horizontal',
      class: {
        content: 'overflow-x-auto min-w-0 max-w-full',
        shadowStart: 'top-0 bottom-0 left-0 w-10 bg-gradient-to-r',
        shadowEnd: 'top-0 bottom-0 right-0 w-10 bg-gradient-to-l',
      },
    },
    {
      orientation: 'vertical',
      snap: true,
      class: {
        content: 'snap-y snap-proximity',
      },
    },
    {
      orientation: 'vertical',
      class: {
        content: 'overflow-y-auto min-h-0 max-h-full',
        shadowStart: '-top-[0.5px] left-0 right-0 min-h-1 h-[25%] max-h-10 bg-gradient-to-b',
        shadowEnd: '-bottom-[0.5px] left-0 right-0 min-h-1 h-[25%] max-h-10 bg-gradient-to-t',
      },
    },
  ],

  defaultVariants: {
    scrollbar: false,
    snap: false,
    orientation: 'horizontal',
    showShadows: true,
    startHidden: true,
    endHidden: true,
  },
})
