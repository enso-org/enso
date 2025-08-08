<script setup lang="ts"></script>

<template>
  <div class="FormContainer"><slot /></div>
</template>

<style scoped>
.FormContainer {
  display: flex;
  flex-direction: column;
  gap: 4px;
}
</style>
