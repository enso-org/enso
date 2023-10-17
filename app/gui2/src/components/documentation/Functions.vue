<script setup lang="ts">
import type { FunctionDocs, TypeDocs } from '@/components/DocumentationPanel.vue'
import { Doc } from '@/utils/ffi'
import type {
  SuggestionEntryArgument,
  SuggestionEntryScope,
} from 'shared/languageServerTypes/suggestions'
 import { computed } from 'vue'

 export type ListItems =
   | { Methods: FunctionDocs[] }
   | { Constructors: FunctionDocs[] }
   | { Types: TypeDocs[] }
 
const props = defineProps<{ items: ListItems }>()
const emit = defineEmits<{}>()

function firstParagraph(synopsis: Doc.Section[]): string | null {
  if (synopsis[0] && 'Paragraph' in synopsis[0]) {
    return synopsis[0].Paragraph.body
  }
  return null
}

function argumentsList(args: SuggestionEntryArgument[]): string {
  return args
    .map((arg) => {
      if (arg.defaultValue) {
        return `${arg.name} = ${arg.defaultValue},`
      } else {
        return arg.name
      }
    })
    .join(' ')
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
 const items = computed<FunctionsDocs[] | TypeDocs[]>(() => {
   const items = props.items
   return items.Methods || items.Constructors || items.Types
 })
</script>

<template>
  <ul>
    <li v-for="item in items" :class="itemClass">
      <a :class="linkClass">
        <span class="entryName">{{ item.name }}</span>
        <span class="arguments">{{ ' ' + argumentsList(item.arguments) }}</span>
      </a>
      <span
        v-if="firstParagraph(item.sections.synopsis)"
        v-html="' ' + firstParagraph(item.sections.synopsis)"
      ></span>
    </li>
  </ul>
</template>

<style scoped>
 .link {
   cursor: pointer;
 }

 .link:hover {
   text-decoration: underline;
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
 }
 ul li:before {
   content: '•';
   font-size: 13px;
   font-weight: 700;
   margin-right: 3px;
 }

 ul li.typeItem:before {
   color: var(--enso-docs-type-name-color);
 }

 ul li.methodItem:before {
   color: var(--enso-docs-method-name-color);
 }

 ul li.typeItem:before {
   color: var(--enso-docs-type-name-color);
 }
</style>
