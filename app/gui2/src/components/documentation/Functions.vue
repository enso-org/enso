<script setup lang="ts">
 import type { FunctionDocs } from '@/components/DocumentationPanel.vue'
 import { Doc } from '@/utils/ffi'
 import { computed } from 'vue'
 import type {
   SuggestionEntryArgument,
   SuggestionEntryScope,
 } from 'shared/languageServerTypes/suggestions'
 const props = defineProps<{ functions: FunctionDocs[] }>()
 const emit = defineEmits<{}>()

 function firstParagraph(synopsis: Doc.Section[]): string | null {
   if (synopsis[0] && 'Paragraph' in synopsis[0]) {
	 return synopsis[0].Paragraph.body
   }
   return null
 }

 function argumentsList(args: SuggestionEntryArgument[]): string {
   return args.map((arg) => {
	 if (arg.defaultValue) {
	   return `${arg.name} = ${arg.defaultValue},`
	 } else {
	   return arg.name
	 }
   }).join(' ')
 }
</script>

<template>
  <ul>
	<li v-for="func in props.functions" class="methodItem">
	 <a class="link method">
		<span class="entryName">{{ func.name }}</span>
		<span class="arguments">{{ ' ' + argumentsList(func.arguments) }}</span>
	  </a>
	  <span v-if="firstParagraph(func.sections.synopsis)" v-html="' ' + firstParagraph(func.sections.synopsis)"></span>
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
   content: "•";
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
</style>
