import { ref } from 'vue'
import { defineStore } from 'pinia'

export interface Component {
  id: number
  icon: string
  label: string
  score: number
  group: number
}

export interface Group {
  color: string
  name: string
}

export const useComponentsStore = defineStore('components', () => {
  function* generate(sets: number) {
    for (let i = 0; i < sets; i += 10) {
      yield { id: i, icon: 'data_input', label: 'Data.read', score: 0.0, group: 0 }
      yield { id: i + 1, icon: 'cloud_from', label: 'Data.fetch', score: 0.0, group: 0 }
      yield { id: i + 2, icon: 'google', label: 'Query.google', score: 0.0, group: 0 }
      yield { id: i + 3, icon: 'chat_gpt_mod2', label: 'Query.chat_gpt', score: 0.0, group: 0 }
      yield { id: i + 4, icon: 'number_input', label: 'number', score: 0.0, group: 1 }
      yield { id: i + 5, icon: 'text_input', label: 'text', score: 0.0, group: 1 }
      yield { id: i + 6, icon: 'table_edit', label: 'Table.new', score: 0.0, group: 1 }
      yield { id: i + 7, icon: 'array_new2', label: 'Array.new', score: 0.0, group: 1 }
      yield { id: i + 8, icon: 'calendar', label: 'Date.current', score: 0.0, group: 2 }
      yield { id: i + 9, icon: 'time', label: 'Date.current_time', score: 0.0, group: 2 }
    }
  }
  const components = ref<Array<Component>>([...generate(300)].reverse())
  const groups = ref<Array<Group>>([
    { color: '#4D9A29', name: 'Data Input' },
    { color: '#B37923', name: 'Input' },
    { color: '#9735B9', name: 'Time' },
  ])
  return { components, groups }
})
