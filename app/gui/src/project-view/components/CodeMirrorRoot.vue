<script setup lang="ts">
import { useCssModule } from 'vue'

defineExpose({ highlightClasses: useCssModule() })
</script>

<template>
  <div class="CodeMirrorRoot" @contextmenu.stop @pointerdown.stop @pointerup.stop @click.stop>
    <slot />
  </div>
</template>

<style scoped>
.CodeMirrorRoot {
  display: contents;
  & :deep(.cm-content) {
    cursor: text;
  }
}
</style>

<!--suppress CssUnusedSymbol -->
<style module>
.comment,
.lineComment,
.blockComment,
.docComment {
  color: #940;
}

.name {
  color: #000;
}
.variableName,
.definition-variableName {
  color: #00f;
}

.literal,
.string {
  color: #a11;
}
.escape {
  color: #e40;
}
.number {
  color: #164;
}

.keyword,
.moduleKeyword,
.modifier {
  color: #708;
}

.punctuation,
.paren {
  color: #333;
}

.operator,
.definitionOperator {
  color: #333;
}

.invalid {
  color: #f00;
}
</style>
