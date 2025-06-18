<script setup lang="ts">
import { useCssModule } from 'vue'

const highlightClasses = useCssModule()
defineExpose({ highlightClasses })
</script>

<template>
  <div class="CodeMirrorRoot" @contextmenu.stop @pointerdown.stop @pointerup.stop @click.stop>
    <slot />
  </div>
</template>

<style scoped>
.CodeMirrorRoot {
  width: 100%;
  height: 100%;
}

.CodeMirrorRoot :deep(.cm-content) {
  cursor: text;
}

/*
 * Change the looks of codemirror placeholder nodes to look the same as placeholder widgets.
 * This is visible when a `placeholder` property is used in the `useCodeMirror` composable,
 * and the edited text value at the same time happen to be empty.
 */
.CodeMirrorRoot :deep(.cm-placeholder) {
  color: inherit;
  opacity: 0.6;
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

:global(.define-node-colors:not(.selected)) {
  .comment,
  .lineComment,
  .blockComment,
  .docComment,
  .name,
  .variableName,
  .definition-variableName,
  .literal,
  .string,
  .escape,
  .number,
  .keyword,
  .moduleKeyword,
  .modifier,
  .punctuation,
  .paren,
  .operator,
  .definitionOperator,
  .invalid {
    color: var(--color-node-text);
    transition: color 0.2s ease;
  }
}
</style>
