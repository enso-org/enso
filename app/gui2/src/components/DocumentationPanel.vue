<script setup lang="ts">
 import { makeMethod } from '@/stores/suggestionDatabase/entry'
 import { computed, ref } from 'vue'
 import { Doc } from '@/util/ffi'
 import type {
   SuggestionEntryArgument,
 } from 'shared/languageServerTypes/suggestions'
 import { default as SvgIcon } from '@/components/SvgIcon.vue'
 import { default as Synopsis } from '@/components/documentation/Synopsis.vue'
 import { default as Functions } from '@/components/documentation/Functions.vue'

 const props = defineProps<{}>()
 const emit = defineEmits<{}>()

 export interface Sections {
  tags: Doc.Section.Tag[],
  synopsis: Doc.Section[],
  examples: Doc.Section[],
 }
 
 export interface FunctionDocs {
   name: string,
   arguments: SuggestionEntryArgument[],
   sections: Sections,
 }
 
 export interface TypeDocs {
   name: string,
   arguments: SuggestionEntryArgument[],
   sections: Sections,
   methods: FunctionDocs[],
   constructors: FunctionDocs[],
 }

 export interface ModuleDocs {
   name: string,
   sections: Sections,
   types: TypeDocs[],
   methods: FunctionDocs[],
 }
  
 const entry = makeMethod('Standard.Base.Foo.foo')
 const mockFunctions = ref([
   {
	 name: 'Method',
	 arguments: [],
	 sections: { tags: [], synopsis: [], examples: [] },
   },
   {
	 name: 'Method2',
	 arguments: [
	   { name: 'a', type: 'Any', isSuspended: false, hasDefault: false }
	 ],
	 sections: { tags: [], synopsis: [ { Paragraph: { body: 'Some annotation.' } }], examples: [] },
   },
   {
	 name: 'Method3',
	 arguments: [
	   { name: 'a', type: 'Any', isSuspended: false, hasDefault: false },
	   { name: 'b', type: 'Int', isSuspended: false, hasDefault: true, defaultValue: '10' }
	 ],
	 sections: { tags: [], synopsis: [ { List: { items: [ 'Test' ] } } ], examples: [] },
   },
   {
	 name: 'Method4',
	 arguments: [],
	 sections: { tags: [], synopsis: [ { List: { items: [ 'Test' ] } }], examples: [] },
   },
 ])
 const documentation = computed(() => {
  const docs: Sections = {
	tags: [{ tag: 'Unstable', body: '' }, { tag: 'Alias', body: 'bar' }],
	synopsis: [
	  {Paragraph: { body: 'Some <code>arbitrary</code> documentation paragraph' }},
	  {Paragraph: { body: 'More text' }},
	  {Paragraph: { body: 'More text' }},
	  { Keyed: { key: 'Key 1', body: 'Some text'} },
	  { Marked: { mark: 'Important', header: 'Some header', body: 'Some important info' } },
	  { Marked: { mark: 'Info', header: 'Info', body: 'Some information' } },
	  { List: { items: [ 'Item 1', 'Item 2', 'Item 3' ] } },
	  { Arguments: { args: [
		{ name: 'Argument 1', description: 'Some argument description' },
		{ name: 'Argument 2', description: 'Some argument description' },
	  ] } },
	],
	examples: [{Marked: {mark: 'Example', body: 'Example body' }}, {Marked: {mark: 'Example', body: 'Second example' }}]
  }
	return docs
  })
</script>

<template>
  <div class="DocumentationPanel">
	<div class="sectionContent">
	  <div class="tagsContainer">
		<div class="tag" v-for="tag in documentation.tags">
		  {{ tag.tag }}
		  {{ tag.body !== '' ? `= ${tag.body}` : '' }}
		</div>
	  </div>
	</div>
	<Functions :functions="mockFunctions" />
	<Synopsis :sections="documentation.synopsis" />
	<div class="headerContainer sectionHeader examplesHeader">
	  <SvgIcon name="doc_examples" />
	  <div class="headerText">
		Examples
	  </div>
	</div>
	<div class="sectionContent">
	  <div v-for="example in documentation.examples" class="exampleContainer">
		<span v-html="example.Marked.body"></span>
	  </div>
	</div>
  </div>
</template>

<style scoped>
 /* Common parts. */
 
 .DocumentationPanel {
   --enso-docs-type-name-color: #9640da;
   --enso-docs-module-name-color: #a239e2;
   --enso-docs-methods-header-color: #1f71d3;
   --enso-docs-method-name-color: #1f71d3;
   --enso-docs-types-header-color: #1f71d3;
   --enso-docs-examples-header-color: #6da85e;
   --enso-docs-important-background-color: #edefe7;
   --enso-docs-info-background-color: #e6f1f8;
   --enso-docs-example-background-color: #e6f1f8;
   --enso-docs-background-color: #eaeaea;
   --enso-docs-text-color: #434343;
   --enso-docs-tag-background-color: #dcd8d8;
   --enso-docs-code-background-color: #dddcde;
   --enso-docs-caption-background-color: #0077f6;
   font-family: "M PLUS 1", DejaVuSansMonoBook, sans-serif;
   font-size: 11.5px;
   color: var(--enso-docs-text-color);
   background-color: var(--enso-docs-background-color);
   padding-left: 8px;
   padding-right: 8px;
   padding-bottom: 4px;
   white-space: normal;
 }

 .type {
   color: var(--enso-docs-type-name-color);
   font-weight: 600;
 }

 /* Headers. */

 div.headerIcon {
   align-self: start;
   display: flex;
 }

 .headerIcon svg {
   pointer-events: none;
   width: 0.85em;
   height: 0.85em;
   margin: 0.1em 0.05em 0 0.05em;
   fill: none;
   align-self: flex-start;
   padding-top: 0.25em;
 }

 .headerContainer {
   padding-left: 8px;
   margin-top: 16px;
   display: flex;
   align-items: center;
 }

 .headerText {
   padding-left: 0.25em;
 }

 .sectionHeader {
   font-size: 15px;
   font-weight: 600;
   margin: 1rem 0 0.25rem 0;
 }

 .methodsHeader {
   color: var(--enso-docs-methods-header-color);
 }

 .typesHeader {
   color: var(--enso-docs-types-header-color);
 }

 .examplesHeader {
   color: var(--enso-docs-examples-header-color);
 }

 /* Tags */

 .tagsContainer {
   display: flex;
   flex-direction: row;
   flex-wrap: wrap;
 }

 .tag {
   display: flex;
   align-items: center;
   justify-content: center;
   height: 24px;
   background-color: var(--enso-docs-tag-background-color);
   border-radius: 4px;
   padding: 1px 5px;
   margin-bottom: 1px;
   margin-right: 2px;
 }

 /* Examples. */

 .exampleContainer {
   background-color: var(--enso-docs-example-background-color);
   border-radius: 0.25rem;
   padding: 0.5rem;
   margin-bottom: 0.5rem;
 }

 .example {
   font-family: EnsoRegular;
   white-space: pre;
   overflow-x: auto;
   margin: 0.05rem 0.1rem;
 }

</style>
