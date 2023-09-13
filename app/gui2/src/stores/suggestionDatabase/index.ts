import { defineStore } from 'pinia'
import { reactive, ref } from 'vue'
import { SuggestionKind, type SuggestionEntry, type SuggestionId } from './entry'
import mockDb from './db-formatted.json'
import { isSome } from '@/util/opt'
import { findIndexOpt } from '@/util/array'

export type SuggestionDb = Map<SuggestionId, SuggestionEntry>
export const SuggestionDb = Map<SuggestionId, SuggestionEntry>

export interface Group {
  color: string
  name: string
}

function fromJsonProtocol(data: any, groups: Group[]): SuggestionEntry {
  function tagValue(tag: string): string {
    return data.documentation.find((section: any) => section['Tag']?.tag === tag)?.Tag.body
  }
  return {
    kind: data.kind,
    definedIn: data.defined_in,
    memberOf: data.kind === SuggestionKind.Constructor ? data.return_type : data.self_type,
    selfType: !data.is_static ? data.self_type : null,
    isPrivate: isSome(tagValue('Private')),
    isUnstable: isSome(tagValue('Unstable')) || isSome(tagValue('Advanced')),
    name: data.name.content,
    aliases: Array.from(tagValue('Alias')?.split(',') ?? [], (alias) => alias.trim()),
    arguments: data.arguments,
    returnType: data.return_type,
    documentation: '',
    iconName: data.icon_name,
    groupIndex: findIndexOpt(groups, (group) => data.group_name == group.name) ?? undefined,
    reexportedIn: data.reexported_in,
  }
}

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = reactive(new SuggestionDb())
  const groups = ref<Array<Group>>([
    { color: '#4D9A29', name: 'Input' },
    { color: '#B37923', name: 'Web' },
    { color: '#9735B9', name: 'Parse' },
    { color: '#4D9A29', name: 'Select' },
    { color: '#B37923', name: 'Join' },
    { color: '#9735B9', name: 'Transform' },
    { color: '#4D9A29', name: 'Output' },
  ])
  for (const [id, entry] of Object.entries(mockDb)) {
    entries.set(+id, fromJsonProtocol(entry, groups.value))
  }

  return { entries, groups }
})
