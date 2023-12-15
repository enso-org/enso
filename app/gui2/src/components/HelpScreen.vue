<script setup lang="ts">
import type { HelpScreenSection } from '@/components/HelpScreen/types'

const props = defineProps<{
  title: string
  headers: string[]
  sections: HelpScreenSection[]
}>()
</script>

<template>
  <div class="HelpScreen">
    <div class="help-container">
      <div class="title-container">{{ props.title }}</div>
      <section v-for="section in sections" :key="section.name" class="section">
        <div class="section-title">{{ section.name }}</div>
        <div v-if="section.description" class="section-description">{{ section.description }}</div>
        <table class="section-table">
          <thead>
            <tr>
              <th v-for="header in props.headers" :key="header">{{ header }}</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="entry in section.entries" :key="entry.name">
              <td v-for="(header, i) in props.headers" :key="header">
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
  font-size: 14px;
  overflow: scroll;
  color: #000000c9;
}

.help-container {
  margin-top: 36px;
  padding: 10px;
}

.title-container {
  font-weight: bold;
  padding: 8px 12px;
  font-size: 16px;
  color: white;
  background: #d5461e;
  border-radius: 9999px;
  max-width: 800px;
}

.section {
  margin-top: 16px;
  padding: 8px;
  border: 3px solid #000000c9;
  border-radius: 18px;
  max-width: 800px;
}

.section-title {
  font-weight: bold;
  font-size: 16px;
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

  th {
    text-align: left;
    padding: 8px;
  }

  td {
    padding: 8px;
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
