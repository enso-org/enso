import type { DropdownEntry } from '@/components/widgets/DropdownWidget.vue'

export interface SubmenuEntry<T extends DropdownEntry> extends DropdownEntry {
  isNested: boolean
  get nestedValues(): T[]
}

/** Check if a {@link DropdownEntry} is a {@link SubmenuEntry}. */
export function isSubmenuEntry<T extends DropdownEntry>(
  entry: DropdownEntry | SubmenuEntry<T>,
): entry is SubmenuEntry<T> {
  return 'isNested' in entry && 'nestedValues' in entry
}
