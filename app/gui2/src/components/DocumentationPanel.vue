<script setup lang="ts">
  import { makeMethod } from '@/stores/suggestionDatabase/entry'
  import { computed } from 'vue'
  import { Doc } from '@/util/ffi'

  const props = defineProps<{}>()
  const emit = defineEmits<{}>()

  interface Sections {
  tags: Doc.Section.Tag[],
  synopsis: Doc.Section[],
  examples: Doc.Section[],
  }
  
  const entry = makeMethod('Standard.Base.Foo.foo')
  const documentation = computed(() => {
  const docs: Sections = {
  tags: [{ tag: 'Unstable', body: '' }, { tag: 'Alias', body: 'bar' }],
  synopsis: [
  {Paragraph: { body: 'Some <code>arbitrary</code> documentation paragraph' }},
  { Keyed: { key: 'Key 2', body: 'Some text'} },
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
	<div class="sectionContent">
	  <div v-for="section in documentation.synopsis">
		<p class="paragraph" v-if="'Paragraph' in section">
		  <span v-html="section.Paragraph.body"></span>
		</p>
		<p class="paragraph" v-if="'Keyed' in section">
		  {{ section.Keyed.key + ": " + section.Keyed.body }}
		</p>
		<div v-if="'Marked' in section" :class="[{backgroundInfo: section.Marked.mark == 'Info', backgroundImportant: section.Marked.mark == 'Important', }, 'markedContainer']">
		  <div v-if="'header' in section.Marked" class="markedHeader">
			{{ "Mark " + section.Marked.header }}
		  </div>
		  <p class="paragraph" v-html="section.Marked.body" />
		</div>
		<ul v-if="'List' in section">
		  <li v-for="item in section.List.items" v-html="item"></li>
		</ul>
		<ul v-if="'Arguments' in section">
		  <li v-for="arg in section.Arguments.args">
			<span class="argument">{{ arg.name }}</span>:&nbsp
			<span v-html="arg.description"></span>
		  </li>
		</ul>
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
 }

 code {
   font-family: EnsoRegular;
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

 ul li.type-item:before {
   color: var(--enso-docs-type-name-color);
 }

 ul li.method-item:before {
   color: var(--enso-docs-method-name-color);
 }

 .paragraph {
   margin: 0;
 }

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

 .type {
   color: var(--enso-docs-type-name-color);
   font-weight: 600;
 }

 .entryName {
   opacity: 0.85;
 }

 .arguments {
   opacity: 0.34;
 }

 .argument {
   font-weight: 600;
 }

 .sectionContent {
   padding-left: 8px;
   padding-right: 8px;
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
   font-weight: 700;
   font-size: 13px;
   margin: 0;
   display: flex;
 }

 div.markedIcon {
   align-self: start;
 }

 div .markedIconImportant {
   margin: 0 0 0 0;
 }

 div .markedIconInfo {
   margin: 0 0.25em 0 0;
 }

 .markedIcon svg {
   pointer-events: none;
   width: 1em;
   height: 1em;
   margin: 0 0.05em 0 0.05em;
   vertical-align: -0.1em;
   fill: none;
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

 /* Code. The words emphasized with backticks. */

 .DocumentationPanel :deep(code) {
   background-color: var(--enso-docs-code-background-color);
   border-radius: 4px;
   padding: 2px;
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

</style>
