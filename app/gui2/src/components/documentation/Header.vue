<script setup lang="ts">
 import { computed } from 'vue'
 import iconMethods from '@/assets/icon-methods.svg'
 import iconExamples from '@/assets/icon-examples.svg'
 
 export type Kind = 'types' | 'methods' | 'examples'
 
 const props = defineProps<{ kind: Kind, label: string }>()

 const classes = computed(() => ({
   Header: true,
   typesHeader: props.kind === 'types',
   methodsHeader: props.kind === 'methods',
   examplesHeader: props.kind === 'examples',
 }))

 const iconMap: Record<Kind, string> = {
   types: iconMethods,
   methods: iconMethods,
   examples: iconExamples,
 }

 const icon = computed(() => {
   return iconMap[props.kind] || iconMethods
 })
</script>

<template>
  <div :class="classes">
    <div class="headerIcon">
      <img :src="icon">
    </div>
    <div class="headerText">{{ props.label }}</div>
  </div>
</template>

<style scoped>
 .Header {
   display: flex;
   align-items: center;
   gap: 6px;
   padding-left: 8px;
   margin-top: 16px;

   &:first-child {
     margin-top: 0;
   }
   
   &.methodsHeader {
     color: var(--enso-docs-methods-header-color);
   }

   &.typesHeader {
     color: var(--enso-docs-types-header-color);
   }

   &.examplesHeader {
     color: var(--enso-docs-examples-header-color);

     .headerIcon {
       padding-top: 3px;

       > img {
         width: 12px;
         height: 12px;
       }
     }

     .headerText {
       font-size: 13px;
       line-height: 22px;
     }
   }

   .headerIcon {
     display: flex;
     padding-top: 4px;
     align-items: flex-start;

     > img {
       pointer-events: none;
       width: 16px;
       height: 16px;
     }
   }

   .headerText {
     padding-top: 2px;
     font-size: 14px;
     font-weight: 800;
     line-height: 24px;
   }
 }
</style>
