import { Icon } from '@/util/iconMetadata/iconName'
import { assert } from './assert'

export type TabButton<T> = { tab: T; title: string; icon: Icon }
export type ExtractTabs<Buttons> = Buttons extends TabButton<infer T>[] ? T : never

/**
 * Define type-safe tab button list. Additionally generates a tab name validator funciton.
 */
export function defineTabButtons<T extends string>(buttons: TabButton<T>[]) {
  const tabs = new Set<T>(buttons.map((b) => b.tab))
  assert(tabs.size == buttons.length, 'Provided tab buttons are not unique.')

  return {
    isValidTab(tab: string): tab is T {
      return tabs.has(tab as T)
    },
    buttons,
  }
}
