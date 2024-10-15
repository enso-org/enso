<script setup lang="ts">
import type { HelpScreenEntry, HelpScreenSection } from '@/components/HelpScreen/types'
import { baseConfig, type ApplicationConfig, type Group, type Option } from '@/util/config'
import { computed } from 'vue'

const props = defineProps<{
  unrecognizedOptions: string[]
  config: ApplicationConfig
}>()

const headers = ['Name', 'Description', 'Default']

const title = computed(() => {
  if (props.unrecognizedOptions.length === 0) {
    return
  } else {
    const optionLabel = props.unrecognizedOptions.length > 1 ? 'options' : 'option'
    return `Unknown config ${optionLabel}: ${props.unrecognizedOptions
      .map((t) => `'${t}'`)
      .join(', ')}. Available options:`
  }
})

function optionValueToString(value: unknown) {
  return Array.isArray(value) ? JSON.stringify(value) : String(value ?? '')
}

function recursiveOptions<T extends Group>(
  group: T,
  path: string[] = [],
): { option: Option; path: string[] }[] {
  return [
    Object.entries<Option>(group.options).map(([k, v]) => ({ option: v, path: [k] })),
    ...Object.entries<Group>(group.groups).map(([k, v]) =>
      recursiveOptions(v as Group, [...path, k]),
    ),
  ].flat()
}

const sections = computed(() => {
  const sectionsData: [name: string, description: string, def: HelpScreenEntry[]][] =
    Object.entries(props.config.groups).map(([groupName, group]) => {
      const groupOptions = recursiveOptions(group as Group<any>)
      const originalGroupOptions = recursiveOptions((baseConfig.groups as any)[groupName] as Group)
      const entriesData: [string, string, string][] = groupOptions.map(({ option, path }, i) => [
        path.join('.'),
        option.description,
        optionValueToString(originalGroupOptions[i]!.option.value ?? ''),
      ])
      entriesData.sort()
      const entries = entriesData.map(
        ([name, description, def]): HelpScreenEntry => ({
          name,
          values: [description, def],
        }),
      )
      const option = (props.config.options as any)[groupName] as Option | undefined
      if (option != null) {
        entries.unshift({
          name: groupName,
          values: [
            option.description,
            optionValueToString(((baseConfig.options as any)[groupName] as Option).value),
          ],
        })
      }
      const name =
        groupName.charAt(0).toUpperCase() +
        groupName.slice(1).replace(/([A-Z])/g, ' $1') +
        ' Options'
      const description = group.description
      return [name, description, entries]
    })
  sectionsData.sort()
  const sections = sectionsData.map(
    ([name, description, entries]): HelpScreenSection => ({ name, description, entries }),
  )

  const rootEntries = Object.entries<Option>(props.config.options).flatMap(([name, option]) => {
    if (name in props.config.groups) return []
    const entry = {
      name,
      values: [
        option.description,
        optionValueToString(((baseConfig.options as any)[name] as Option).value),
      ],
    }
    return [entry]
  })

  if (rootEntries.length > 0) {
    const name = 'Other Options'
    sections.push({ name, entries: rootEntries })
  }

  return sections
})
</script>

<template>
  <div class="HelpScreen">
    <div class="help-container">
      <div class="title-container">{{ title }}</div>
      <section v-for="section in sections" :key="section.name" class="section">
        <div class="section-title">{{ section.name }}</div>
        <div v-if="section.description" class="section-description">{{ section.description }}</div>
        <table class="section-table">
          <thead>
            <tr>
              <th v-for="header in headers" :key="header">{{ header }}</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="entry in section.entries" :key="entry.name">
              <td v-for="(header, i) in headers" :key="header">
                <template v-if="i === 0">{{ entry.name }}</template>
                <template v-else>{{ entry.values[i - 1] ?? '' }}</template>
              </td>
            </tr>
          </tbody>
        </table>
      </section>
    </div>
  </div>
</template>

<style scoped>
.HelpScreen {
  width: 100%;
  height: 100%;
  display: flex;
  flex-flow: column;
  align-items: center;
  font-family: var(--font-sans);
  overflow: scroll;
  color: var(--color-primary);
}

.help-container {
  margin-top: 36px;
  padding: 10px;
}

.title-container {
  font-weight: bold;
  padding: 4px 10px;
  font-size: 13px;
  color: white;
  background: lch(60% 100 60);
  border-radius: 9999px;
  max-width: 800px;
}

.section {
  margin-top: 16px;
  padding: 8px;
  background: var(--color-frame-bg);
  border-radius: var(--radius-default);
  max-width: 800px;
}

.section-title {
  font-weight: bold;
  font-size: 13px;
  margin-bottom: 6px;
  margin-left: 2px;
}

.section-description {
  margin-bottom: 16px;
  margin-left: 2px;
}

.section-table {
  padding-top: 20px;
  border-collapse: collapse;

  th,
  td {
    padding: 4px 6px;
  }

  th {
    text-align: left;
  }

  tr:nth-child(odd) > td {
    background: #00000010;
  }

  td:first-child {
    border-top-left-radius: 16px;
    border-bottom-left-radius: 16px;
  }

  td:nth-child(2) {
    width: 100%;
  }

  td:last-child {
    border-top-right-radius: 16px;
    border-bottom-right-radius: 16px;
  }
}
</style>
