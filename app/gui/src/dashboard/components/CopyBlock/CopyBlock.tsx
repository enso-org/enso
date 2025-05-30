/** @file A block of text with a copy button. */
import { useCopy } from '#/hooks/copyHooks'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { useText } from '$/providers/react'
import type { ReactNode } from 'react'
import { Button } from '../Button'
import { TEXT_STYLE } from '../Text'

// eslint-disable-next-line react-refresh/only-export-components
export const COPY_BLOCK_STYLES = tv({
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

/** Props for a {@link CopyBlock}. */
export interface CopyBlockProps extends VariantProps<typeof COPY_BLOCK_STYLES> {
  readonly title?: ReactNode
  readonly copyText: string
  readonly className?: string
  readonly onCopy?: () => void
}

/** A block of text with a copy button. */
export function CopyBlock(props: CopyBlockProps) {
  const { copyText, className, onCopy = () => {}, variants = COPY_BLOCK_STYLES } = props

  const { getText } = useText()
  const { mutateAsync, isSuccess } = useCopy({ onCopy })

  const styles = variants()

  return (
    <Button
      variant="custom"
      size="custom"
      onPress={() => mutateAsync(copyText)}
      tooltip={isSuccess ? getText('copied') : getText('copy')}
      className={styles.base({ className })}
    >
      <span className={styles.copyTextBlock()}>{copyText}</span>
    </Button>
  )
}
