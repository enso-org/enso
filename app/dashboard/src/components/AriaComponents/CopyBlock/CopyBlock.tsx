/** @file A block of text with a copy button. */

import * as React from 'react'

import * as ariaComponents from '#/components/AriaComponents'

import * as twv from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

const COPY_BLOCK_STYLES = twv.tv({
  base: 'relative grid grid-cols-[minmax(0,_1fr)_auto] max-w-full bg-primary/10 items-center',
  variants: {
    size: {
      small: 'py-2 pl-2 pr-1',
      medium: 'py-3 pl-3 pr-2',
      large: 'py-4 pl-4 pr-2.5',
    },
    roundings: {
      custom: '',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      full: 'rounded-full',
    },
  },
  slots: {
    titleBlock: 'col-span-1 text-sm text-primary/60',
    copyTextBlock: 'flex-auto text-sm text-primary/60 text-nowrap overflow-x-auto scroll-hidden',
    copyButton: 'flex-none',
  },
  defaultVariants: {
    size: 'medium',
    roundings: 'medium',
  },
})

// =================
// === CopyBlock ===
// =================

/** Props for a {@link CopyBlock}. */
export interface CopyBlockProps {
  readonly title?: React.ReactNode
  readonly copyText: string
  readonly className?: string
  readonly onCopy?: () => void
}

/** A block of text with a copy button. */
export function CopyBlock(props: CopyBlockProps) {
  const { copyText, className, onCopy = () => {} } = props
  const { copyTextBlock, base, copyButton } = COPY_BLOCK_STYLES()

  return (
    <div className={base({ className })}>
      <div className={copyTextBlock()}>{copyText}</div>
      <ariaComponents.CopyButton copyText={copyText} onCopy={onCopy} className={copyButton()} />
    </div>
  )
}
