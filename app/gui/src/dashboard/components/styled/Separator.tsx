/** @file A horizontal line dividing two sections in a menu. */
import { Separator as AriaSeparator } from '#/components/aria'

/** Props for a {@link Separator}. */
export interface SeparatorProps {
  readonly hidden?: boolean
}

/** A horizontal line dividing two sections in a menu. */
export default function Separator(props: SeparatorProps) {
  const { hidden = false } = props

  return (
    !hidden && (
      <AriaSeparator className="mx-context-menu-entry-px my-separator-y border-t-0.5 border-primary/10" />
    )
  )
}
