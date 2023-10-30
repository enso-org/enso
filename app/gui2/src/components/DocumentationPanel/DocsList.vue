<script setup lang="ts">
import type { FunctionDocs, TypeDocs } from '@/components/DocumentationPanel/ir'
import type { Doc } from '@/util/docParser'
import type { SuggestionEntryArgument, SuggestionId } from 'shared/languageServerTypes/suggestions'
import { computed } from 'vue'

const props = defineProps<{ items: ListItems }>()
const emit = defineEmits<{ linkClicked: [id: SuggestionId] }>()

interface Methods {
  kind: 'Methods'
  items: FunctionDocs[]
}

interface Constructors {
  kind: 'Constructors'
  items: FunctionDocs[]
}

interface Types {
  kind: 'Types'
  items: TypeDocs[]
}

type ListItems = Methods | Constructors | Types

function firstParagraph(synopsis: Doc.Section[]): string | undefined {
  if (synopsis[0] && 'Paragraph' in synopsis[0]) {
    return synopsis[0].Paragraph.body
  }
}

function argumentsList(args: SuggestionEntryArgument[]): string {
  return args
    .map((arg) => {
      const defaultValue = arg.defaultValue ? ` = ${arg.defaultValue}` : ''
      return `${arg.name}${defaultValue}`
    })
    .join(', ')
}

const annotations = computed<Array<string | undefined>>(() => {
  return props.items.items.map((item) => firstParagraph(item.sections.synopsis))
})
</script>

<template>
  <ul v-if="props.items.items.length > 0">
    <li v-for="(item, index) in props.items.items" :key="index" :class="props.items.kind">
      <a
        :class="['link', props.items.kind]"
        @pointerdown.stop.prevent="emit('linkClicked', item.id)"
      >
        <span class="entryName">{{ item.name }}</span>
        <span class="arguments">{{ ' ' + argumentsList(item.arguments) }}</span>
      </a>
      <!-- eslint-disable vue/no-v-html -->
      <span v-if="annotations[index]" v-html="' ' + annotations[index]"></span>
      <!-- eslint-enable vue/no-v-html -->
    </li>
  </ul>
</template>

<style scoped>
.link {
  cursor: pointer;
  font-weight: 600;

  &:hover {
    text-decoration: underline;
  }

  &.Types {
    color: var(--enso-docs-type-name-color);
  }

  &.Methods {
    color: var(--enso-docs-method-name-color);
  }

  &.Constructors {
    color: var(--enso-docs-type-name-color);
  }
}

.entryName {
  opacity: 0.85;
}

.arguments {
  opacity: 0.34;
}

ul {
  margin: 0;
  padding: 0;
  list-style-type: none;
  list-style-position: inside;
}

li {
  &:before {
    content: '•';
    font-size: 13px;
    font-weight: 700;
    margin-right: 3px;
  }

  &.Types:before {
    color: var(--enso-docs-type-name-color);
  }

  &.Methods:before {
    color: var(--enso-docs-method-name-color);
  }

  &.Constructors:before {
    color: var(--enso-docs-method-name-color);
  }
}
</style>
