<template>
  <div class="MenuPanel">
    <slot />
  </div>
</template>

<style scoped>
.MenuPanel {
  display: flex;
  flex-direction: column;
  border-radius: 13px;
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  margin: 0 -4px;
  z-index: 1;
  gap: 4px;
  padding: 8px;
}
</style>
