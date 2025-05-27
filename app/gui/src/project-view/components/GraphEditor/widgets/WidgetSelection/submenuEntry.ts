import { DropdownEntry } from '@/components/widgets/DropdownWidget.vue'

export interface SubmenuEntry<T> extends DropdownEntry {
  isNested: boolean
  get nestedValues(): T[]
}

/** Check if a {@link DropdownEntry} is a {@link SubmenuEntry}. */
export function isSubmenuEntry(entry: DropdownEntry): entry is SubmenuEntry<unknown> {
  return 'isNested' in entry && 'nestedValues' in entry
}
