<script setup lang="ts">
 import { Sections } from '@/components/DocumentationPanel.vue'
 import { default as SvgIcon } from '@/components/SvgIcon.vue'
 import { Doc } from '@/util/ffi'
 const props = defineProps<{ sections: Doc.Section[] }>()
 const emit = defineEmits<{}>()
</script>

<template>
  <div class="sectionContent">
	  <div v-for="section in props.sections">
		<p class="paragraph" v-if="'Paragraph' in section">
		  <span v-html="section.Paragraph.body"></span>
		</p>
		<p class="paragraph" v-if="'Keyed' in section">
		  {{ section.Keyed.key + ": " + section.Keyed.body }}
		</p>
		<div v-if="'Marked' in section" :class="[{backgroundInfo: section.Marked.mark == 'Info', backgroundImportant: section.Marked.mark == 'Important', }, 'markedContainer']">
		  <div v-if="'header' in section.Marked" class="markedHeader">
			<SvgIcon :name="section.Marked.mark == 'Info' ? 'doc_info' : 'doc_important'" />
			{{ " " + section.Marked.header }}
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

 /* Code. The words emphasized with backticks. */

 :deep(code) {
   background-color: var(--enso-docs-code-background-color);
   border-radius: 4px;
   padding: 2px;
 }

 /* TODO: move */
 .sectionContent {
   padding-left: 8px;
   padding-right: 8px;
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

 .paragraph {
   margin: 0;
 }

 .argument {
   font-weight: 600;
 }
</style>
