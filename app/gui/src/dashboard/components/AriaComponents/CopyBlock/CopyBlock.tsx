/** @file A block of text with a copy button. */

import * as React from 'react'

import * as copyHook from '#/hooks/copyHooks'

import * as textProvider from '#/providers/TextProvider'

import * as twv from '#/utilities/tailwindVariants'
import { Button } from '../Button'
import { TEXT_STYLE } from '../Text'

// =================
// === Constants ===
// =================

const COPY_BLOCK_STYLES = twv.tv({
  base: TEXT_STYLE({
    class: 'max-w-full bg-primary/5 border-primary/10',
  }),
  variants: {
    size: {
      small: 'py-[1.5px] px-[5.5px]',
      medium: 'py-[3.5px] px-[7.5px]',
      large: 'py-[5.5px] px-[11.5px]',
    },
    rounded: {
      custom: '',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      full: 'rounded-full',
    },
  },
  slots: { copyTextBlock: 'flex-auto text-nowrap overflow-x-auto scroll-hidden w-full' },
  defaultVariants: { size: 'medium', rounded: 'full' },
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

  const { getText } = textProvider.useText()
  const { mutateAsync, isSuccess } = copyHook.useCopy({ copyText, onCopy })

  const { copyTextBlock, base } = COPY_BLOCK_STYLES()

  return (
    <Button
      variant="custom"
      size="custom"
      onPress={() => mutateAsync()}
      tooltip={isSuccess ? getText('copied') : getText('copy')}
      className={base({ className })}
    >
      <span className={copyTextBlock()}>{copyText}</span>
    </Button>
  )
}
