<script setup lang="ts">
import UpsertSecretMenuReact from '#/layouts/UpsertSecretMenu'
import { reactComponent } from '@/util/react'

const WrappedUpsertSecretMenu = reactComponent(UpsertSecretMenuReact)

const emit = defineEmits<{
  accepted: [value: string, name: string]
  canceled: []
}>()
</script>

<template>
  <div class="UpsertSecretPanel">
    <header class="header">New Secret</header>
    <div class="content">
      <WrappedUpsertSecretMenu
        :doCreate="(name, value) => emit('accepted', value, name)"
        :doCancel="() => emit('canceled')"
      />
    </div>
  </div>
</template>

<style scoped>
.UpsertSecretPanel {
  width: 100%;
  height: 100%;
  overflow-y: auto;
}

.content {
  width: 100%;
  height: 100%;
  padding: 0.75rem 1rem 1rem;
}

.header {
  padding: 3px 0.875rem 0.125rem;
  text-align: center;
  min-height: 42px;
  position: sticky;
  /* Above the form's placeholder text. */
  z-index: 1;
  top: 0;
  border-color: rgb(var(--color-primary-rgb) / 0.1);
  border-bottom-width: 0.5px;
  color: rgb(var(--color-primary-rgb) / var(--color-primary-opacity));
  line-height: 29px;
  font-weight: 600;
  font-size: 19px;
}
</style>
