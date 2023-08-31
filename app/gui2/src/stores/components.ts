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
    for (let i = 0; i < sets; i += 3) {
      yield { id: i, icon: 'text_input', label: 'Data.read', score: 0.0, group: 0 }
      yield { id: i + 1, icon: 'text_input', label: 'text input', score: 0.0, group: 1 }
      yield { id: i + 2, icon: 'text_input', label: 'number input', score: 0.0, group: 1 }
    }
  }
  const components = ref<Array<Component>>([...generate(300)])
  console.log(components)
  const groups = ref<Array<Group>>([
    { color: '#4D9A29', name: 'Data Input' },
    { color: '#B37923', name: 'Input' },
  ])
  return { components, groups }
})
