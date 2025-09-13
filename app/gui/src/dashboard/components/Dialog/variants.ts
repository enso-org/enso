/**
 * @file
 *
 * Variants for the Dialog component.
 */
import { tv } from '#/utilities/tailwindVariants'

export const DIALOG_BACKGROUND = tv({
  base: 'backdrop-blur-md',
  variants: { variant: { light: 'bg-background/75', dark: 'bg-primary/70 text-invert' } },
  defaultVariants: { variant: 'light' },
})

export const DIALOG_OVERLAY_STYLES = tv({
  base: 'fixed inset-0 isolate flex items-center justify-center bg-primary/20',
  variants: {
    isEntering: { true: 'animate-in fade-in duration-200 ease-out' },
    isExiting: { true: 'animate-out fade-out duration-200 ease-in' },
    blockInteractions: { true: 'backdrop-blur-md transition-[backdrop-filter] duration-200' },
  },
})

export const DIALOG_MODAL_STYLES = tv({
  base: 'fixed inset-0 flex items-center justify-center text-xs text-primary',
  variants: {
    isEntering: { true: 'animate-in ease-out duration-200' },
    isExiting: { true: 'animate-out ease-in duration-200' },
    type: { modal: '', fullscreen: 'p-3.5' },
  },
  compoundVariants: [
    { type: 'modal', isEntering: true, class: 'slide-in-from-top-1' },
    { type: 'modal', isExiting: true, class: 'slide-out-to-top-1' },
    { type: 'fullscreen', isEntering: true, class: 'zoom-in-[1.015]' },
    { type: 'fullscreen', isExiting: true, class: 'zoom-out-[1.015]' },
  ],
})

export const DIALOG_STYLES = tv({
  base: DIALOG_BACKGROUND({
    className: 'w-full max-w-full flex flex-col text-left align-middle shadow-xl overflow-clip',
  }),
  variants: {
    type: {
      modal: {
        base: 'w-full min-h-[100px] max-h-[90vh]',
        header: 'px-3.5 pt-[3px] pb-0.5 min-h-[42px]',
      },
      fullscreen: {
        base: 'w-full h-full max-w-full max-h-full bg-clip-border',
        header: 'px-4 pt-[5px] pb-1.5 min-h-12',
      },
    },
    fitContent: {
      true: {
        base: 'min-w-max',
        content: 'min-w-max',
      },
    },
    hideCloseButton: { true: { closeButton: 'hidden' } },
    closeButton: {
      normal: { base: '', closeButton: '' },
      floating: {
        base: '',
        closeButton: 'absolute left-4 top-4 visible z-1 transition-all duration-150',
        header: 'p-0 max-h-0 min-h-0 h-0 border-0 z-1',
        content: 'isolate',
      },
      none: {},
    },
    rounded: {
      none: { base: '' },
      small: { base: 'rounded-sm' },
      medium: { base: 'rounded-md' },
      large: { base: 'rounded-lg' },
      xlarge: { base: 'rounded-xl' },
      xxlarge: { base: 'rounded-2xl', scroller: 'scroll-offset-edge-2xl' },
      xxxlarge: { base: 'rounded-3xl', scroller: 'scroll-offset-edge-3xl' },
      xxxxlarge: { base: 'rounded-4xl', scroller: 'scroll-offset-edge-4xl' },
    },
    /**
     * The size of the dialog.
     * Only applies to the `modal` type.
     */
    size: {
      small: { base: '' },
      medium: { base: '' },
      large: { base: '' },
      xlarge: { base: '' },
      xxlarge: { base: '' },
      xxxlarge: { base: '' },
      xxxxlarge: { base: '' },
    },
    padding: {
      none: { content: 'p-0' },
      small: { content: 'px-1 pt-3.5 pb-3.5' },
      medium: { content: 'px-4 pt-3 pb-4' },
      large: { content: 'px-8 pt-5 pb-5' },
      xlarge: { content: 'p-12 pt-6 pb-8' },
      xxlarge: { content: 'p-16 pt-8 pb-12' },
      xxxlarge: { content: 'p-20 pt-10 pb-16' },
    },
    scrolledToTop: { true: { header: 'border-transparent' } },
    layout: { true: { measurerWrapper: 'h-auto' }, false: { measurerWrapper: 'h-full' } },
  },
  slots: {
    header:
      'sticky z-1 top-0 grid grid-cols-[1fr_auto_1fr] items-center border-b-0.5 border-primary/10 transition-[border-color] duration-150',
    closeButton: 'col-start-1 col-end-1 mr-auto',
    heading: 'col-start-2 col-end-2 my-0 text-center',
    scroller: 'flex flex-col h-full overflow-y-auto max-h-[inherit]',
    measurerWrapper: 'inline-grid min-h-fit w-full grid-rows-1',
    content: 'inline-block max-h-fit min-h-fit [grid-area:1/1] min-w-0',
  },
  compoundVariants: [
    { type: 'modal', size: 'small', class: 'max-w-sm' },
    { type: 'modal', size: 'medium', class: 'max-w-md' },
    { type: 'modal', size: 'large', class: 'max-w-lg' },
    { type: 'modal', size: 'xlarge', class: 'max-w-xl' },
    { type: 'modal', size: 'xxlarge', class: 'max-w-2xl' },
    { type: 'modal', size: 'xxxlarge', class: 'max-w-3xl' },
    { type: 'modal', size: 'xxxxlarge', class: 'max-w-4xl' },
    { type: 'fullscreen', class: { measurerWrapper: 'h-full' } },
  ],
  defaultVariants: {
    layout: true,
    type: 'modal',
    closeButton: 'normal',
    hideCloseButton: false,
    size: 'medium',
    padding: 'none',
    rounded: 'xxxlarge',
  },
})
