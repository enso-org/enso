<template>
  <div
    ref="lexicalElement"
    class="LexicalContent"
    spellcheck="false"
    contenteditable="true"
    @keydown.arrow-left.stop
    @keydown.arrow-right.stop
    @keydown.arrow-up.stop
    @keydown.arrow-down.stop
  />
</template>

<style scoped>
.LexicalContent {
  outline-style: none;
  flex: 1;
}
</style>
