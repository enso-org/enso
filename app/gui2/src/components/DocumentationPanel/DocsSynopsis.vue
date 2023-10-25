<script setup lang="ts">
import iconImportant from '@/assets/icon-important.svg'
import iconInfo from '@/assets/icon-info.svg'
import type { Doc } from '@/util/docParser'

const props = defineProps<{ sections: Doc.Section[] }>()
</script>

<template>
  <!-- eslint-disable vue/no-v-html -->
  <div v-if="props.sections.length > 0">
    <template v-for="(section, _i) in props.sections" :key="_i">
      <p v-if="'Paragraph' in section" class="paragraph">
        <span v-html="section.Paragraph.body"></span>
      </p>
      <p v-else-if="'Keyed' in section" class="paragraph">
        {{ section.Keyed.key + ': ' + section.Keyed.body }}
      </p>
      <div v-else-if="'Marked' in section" :class="[section.Marked.mark, 'markedContainer']">
        <div v-if="'header' in section.Marked" class="markedHeader">
          <img :src="section.Marked.mark == 'Info' ? iconInfo : iconImportant" class="markedIcon" />
          {{ section.Marked.header }}
        </div>
        <p class="paragraph" v-html="section.Marked.body" />
      </div>
      <ul v-else-if="'List' in section">
        <li v-for="(item, index) in section.List.items" :key="index" v-html="item"></li>
      </ul>
      <ul v-else-if="'Arguments' in section">
        <li v-for="(arg, index) in section.Arguments.args" :key="index">
          <span class="argument">{{ arg.name }}</span
          >:&nbsp;
          <span v-html="arg.description"></span>
        </li>
      </ul>
    </template>
  </div>
  <!-- eslint-enable vue/no-v-html -->
</template>

<style scoped>
ul {
  margin: 0;
  padding: 0;
  list-style-type: none;
  list-style-position: inside;
}

li:before {
  content: '•';
  font-size: 13px;
  font-weight: 600;
  margin-right: 6px;
}

.paragraph {
  margin: 0;
  padding: 1px 0;
}

.argument {
  font-weight: 800;
}

/* Marked sections, such as `Info` and `Important` sections. */

.Info {
  background-color: var(--enso-docs-info-background-color);
}

.Important {
  background-color: var(--enso-docs-important-background-color);
}

.markedContainer {
  border-radius: 0.5rem;
  padding: 0.5rem;
  margin: 0.5rem 0;
}

.markedHeader {
  font-weight: 600;
  font-size: 13px;
  margin: 0;
  display: flex;
  align-items: center;
  gap: 0.25em;
}

.markedIcon {
  pointer-events: none;
  width: 1em;
  height: 1em;
  margin: 0 0.05em 0 0.05em;
  vertical-align: -0.1em;
  fill: none;
}

/* Code. The words emphasized with backticks. */

:deep(code) {
  background-color: var(--enso-docs-code-background-color);
  border-radius: 4px;
  padding: 3px;
}
</style>
