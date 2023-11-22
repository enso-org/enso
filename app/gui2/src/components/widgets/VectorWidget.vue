<script setup lang="ts" generic="T">
import SvgIcon from '@/components/SvgIcon.vue'

const props = defineProps<{ modelValue: T[]; default: () => T }>()
const emit = defineEmits<{ 'update:modelValue': [modelValue: T[]] }>()
</script>

<template>
  <div class="VectorWidget">
    <ul class="items">
      <li v-for="(item, index) in modelValue" :key="index" class="item">
        <slot :item="item"></slot>
      </li>
    </ul>
    <SvgIcon
      class="add-item"
      name="vector_add"
      @click="emit('update:modelValue', [...props.modelValue, props.default()])"
    />
  </div>
</template>

<style scoped>
.VectorWidget {
  display: flex;
  gap: 8px;
  align-items: center;
}

.items {
  display: flex;
}

.item::before,
.item::after {
  color: rgb(255 255 255 / 0.33);
  vertical-align: middle;
  align-items: center;
  display: inline-flex;
  height: 24px;
}

.item:first-child::before {
  content: '[';
}

.item::after {
  content: ',\00a0';
}

.item:last-child::after {
  content: ']';
}

.add-item {
  transition-property: opacity;
  transition-duration: 150ms;
  transition-timing-function: ease-in-out;
  opacity: 0.8;
}

.add-item:hover {
  opacity: 1;
}
</style>
