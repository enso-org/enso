<script setup lang="ts">
import type { FunctionDocs, TypeDocs } from '@/components/DocumentationPanel.vue'
import type { Doc } from '@/util/docParser'
import type { SuggestionEntryArgument, SuggestionId } from 'shared/languageServerTypes/suggestions'
import { computed } from 'vue'

export type ListItems =
  | { Methods: FunctionDocs[] }
  | { Constructors: FunctionDocs[] }
  | { Types: TypeDocs[] }

const props = defineProps<{ items: ListItems }>()
const emit = defineEmits<{ linkClicked: [id: SuggestionId] }>()

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

const itemClass = computed<string>(() => {
  switch (Object.keys(props.items)[0]) {
    case 'Methods':
    case 'Constructors':
      return 'methodItem'
    case 'Types':
      return 'typeItem'
    default:
      return ''
  }
})

const linkClass = computed(() => ({
  link: true,
  method: 'Methods' in props.items,
  constructor: 'Constructors' in props.items,
  type: 'Types' in props.items,
}))

const items = computed<FunctionDocs[] | TypeDocs[]>(() => {
  const items: ListItems = props.items
  if ('Methods' in items) {
    return items.Methods
  } else if ('Constructors' in items) {
    return items.Constructors
  } else if ('Types' in items) {
    return items.Types
  } else {
    return []
  }
})

function getId(entry: FunctionDocs | TypeDocs) {
  return entry.id
}

const annotations = computed<Array<string | undefined>>(() => {
  return items.value.map((item) => firstParagraph(item.sections.synopsis))
})
</script>

<template>
  <ul v-if="items.length > 0" class="sectionContent">
    <li v-for="(item, index) in items" :key="index" :class="itemClass">
      <a :class="linkClass" @pointerdown="emit('linkClicked', getId(item))">
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

  &:hover {
    text-decoration: underline;
  }
}

.type {
  color: var(--enso-docs-type-name-color);
  font-weight: 600;
}

.method {
  color: var(--enso-docs-method-name-color);
  font-weight: 600;
}

.constructor {
  color: var(--enso-docs-type-name-color);
  font-weight: 600;
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

  li {
    &:before {
      content: '•';
      font-size: 13px;
      font-weight: 700;
      margin-right: 3px;
    }

    &.typeItem:before {
      color: var(--enso-docs-type-name-color);
    }

    &.methodItem:before {
      color: var(--enso-docs-method-name-color);
    }
  }
}
</style>
