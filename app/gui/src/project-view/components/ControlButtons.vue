<template>
  <div class="ControlButtons">
    <div class="control left-end">
      <slot name="left"></slot>
    </div>
    <div class="control right-end">
      <slot name="right"></slot>
    </div>
  </div>
</template>

<style scoped>
.ControlButtons {
  user-select: none;
  display: flex;
  place-items: center;
  gap: 1px;
}

.control {
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  padding: 4px 4px;
  width: 32px;
}

.left-end {
  border-radius: var(--radius-full) 0 0 var(--radius-full);

  > * {
    margin: 0 0 0 auto;
  }
}

.right-end {
  border-radius: 0 var(--radius-full) var(--radius-full) 0;

  > * {
    margin: 0 auto 0 0;
  }
}
</style>
