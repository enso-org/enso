/** @file An entry in a context menu. */
import MenuEntry, { type MenuEntryProps } from '#/components/MenuEntry'

/** Props for a {@link ContextMenuEntry}. */
export type ContextMenuEntryProps = Omit<MenuEntryProps, 'isContextMenuEntry'>

/** An item in a menu. */
export default function ContextMenuEntry(props: ContextMenuEntryProps) {
  return <MenuEntry variant="context-menu" {...props} />
}
