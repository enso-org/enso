<script setup lang="ts">
import type { Sections } from '@/components/DocumentationPanel.vue'
 import type { Doc } from '@/util/docParser'
 import iconImportant from '@/assets/icon-important.svg'
 import iconInfo from '@/assets/icon-info.svg'
 
const props = defineProps<{ sections: Doc.Section[] }>()

</script>

<template>
  <div v-if="props.sections.length > 0" class="sectionContent">
    <div v-for="section in props.sections">
      <p v-if="'Paragraph' in section" class="paragraph">
        <span v-html="section.Paragraph.body ?? 'Invalid body'"></span>
      </p>
      <p v-if="'Keyed' in section" class="paragraph">
        {{ section.Keyed.key + ': ' + section.Keyed.body }}
      </p>
      <div
        v-if="'Marked' in section"
        :class="[
          {
            backgroundInfo: section.Marked.mark == 'Info',
            backgroundImportant: section.Marked.mark == 'Important',
          },
          'markedContainer',
        ]"
      >
        <div v-if="'header' in section.Marked" class="markedHeader">
          <img :src="section.Marked.mark == 'Info' ? iconInfo : iconImportant" class="markedIcon" />
          {{ section.Marked.header }}
        </div>
        <p class="paragraph" v-html="section.Marked.body ?? 'Invalid body'" />
      </div>
      <ul v-if="'List' in section">
        <li v-for="item in section.List.items" v-html="item ?? 'Invalid item'"></li>
      </ul>
      <ul v-if="'Arguments' in section">
        <li v-for="arg in section.Arguments.args">
          <span class="argument">{{ arg.name }}</span>:&nbsp
          <span v-html="arg.description ?? 'Invalid description'"></span>
        </li>
      </ul>
    </div>
  </div>
</template>

<style scoped>
/* Marked sections, such as `Info` and `Important` sections. */

.backgroundInfo {
  background-color: var(--enso-docs-info-background-color);
}

.backgroundImportant {
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
 
div .markedIconImportant {
  margin: 0 0 0 0;
}

div .markedIconInfo {
  margin: 0 0.25em 0 0;
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

ul {
  margin: 0;
  padding: 0;
  list-style-type: none;
  list-style-position: inside;
}
ul li:before {
  content: '•';
  font-size: 10px;
  font-weight: 800;
  margin-right: 6px;
}

.paragraph {
  margin: 0;
  padding: 1px 0;
}

.argument {
  font-weight: 800;
}
</style>
