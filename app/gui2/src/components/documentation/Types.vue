<script setup lang="ts">
import type { TypeDocs } from '@/components/DocumentationPanel.vue'
import { Doc } from '@/utils/ffi'
import type {
  SuggestionEntryArgument,
  SuggestionEntryScope,
} from 'shared/languageServerTypes/suggestions'
import { computed } from 'vue'
const props = defineProps<{ types: TypeDocs[] }>()
const emit = defineEmits<{}>()

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
</script>

<template>
  <ul>
    <li v-for="t in props.types" class="typeItem">
      <a class="link type">
        <span class="entryName">{{ t.name }}</span>
        <span class="arguments">{{ ' ' + argumentsList(t.arguments) }}</span>
      </a>
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
</style>
