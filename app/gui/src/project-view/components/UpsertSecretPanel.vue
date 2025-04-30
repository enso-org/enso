<script setup lang="ts">
import { UpsertSecretForm } from '#/modals/UpsertSecretModal'
import DashboardDialogContent from '@/components/DashboardDialogContent.vue'
import { reactComponent } from '@/util/react'

const WrappedUpsertSecretForm = reactComponent(UpsertSecretForm)

const emit = defineEmits<{
  accepted: [value: string, name: string]
  canceled: []
}>()
</script>

<template>
  <DashboardDialogContent class="UpsertSecretPanel" heading="New Secret" :blockInteractions="true">
    <WrappedUpsertSecretForm
      :doCreate="(name, value) => emit('accepted', value, name)"
      :doCancel="() => emit('canceled')"
    />
  </DashboardDialogContent>
</template>
