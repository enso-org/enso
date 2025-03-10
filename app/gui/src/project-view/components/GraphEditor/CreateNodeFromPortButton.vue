<script setup lang="ts">
import { AstId } from 'ydoc-shared/ast'

const props = defineProps<{ portId: AstId }>()
</script>

<template>
  <g class="CreateNodeFromPortButton">
    <rect :class="{ connection: true }" fill="currentColor"></rect>
    <g :class="{ plusIcon: true }">
      <mask :id="`${props.portId}_add_node_clip_path`">
        <rect class="maskBackground"></rect>
        <rect class="plusV"></rect>
        <rect class="plusH"></rect>
      </mask>
      <circle :mask="`url(#${props.portId}_add_node_clip_path)`" fill="currentColor"></circle>
    </g>
    <rect class="hoverArea"></rect>
  </g>
</template>

<style scoped>
.CreateNodeFromPortButton {
  --radius: 6px;
  --maskSize: calc(var(--radius) * 2);
  --strokeWidth: 1.5px;
  --leftOffset: 16px;
  --topOffset: 40px;
  --color-dimmed: color-mix(in oklab, var(--color-node-primary) 60%, white 40%);
  --color: var(--color-node-primary);
  pointer-events: all;
}

.connection {
  --width: 4px;
  --direct-hover-offset: calc(
    var(--output-port-hovered-extra-width) * var(--direct-hover-animation)
  );
  width: var(--width);
  height: calc((var(--topOffset) - var(--direct-hover-offset) + 2px) * var(--hover-animation));
  transform: translate(
    calc(var(--port-clip-start) * (100% + 1px) + var(--leftOffset) - var(--width) / 2),
    calc(var(--node-size-y) + var(--direct-hover-offset) - var(--output-port-overlap))
  );
  cursor: pointer;
  color: var(--color-dimmed);
  transition: color 0.2s ease;
}

.hovered * {
  color: var(--color);
}

.plusIcon {
  transform: translate(
    calc(var(--port-clip-start) * (100% + 1px) + var(--leftOffset) - var(--radius)),
    calc(
      var(--node-size-y) + var(--output-port-max-width) + var(--node-vertical-gap) +
        var(--topOffset)
    )
  );
  color: var(--color-dimmed);
  cursor: pointer;
  & .maskBackground {
    fill: white;
    width: var(--maskSize);
    height: var(--maskSize);
  }
  & .plusV {
    x: calc(var(--maskSize) / 2 - var(--strokeWidth) / 2);
    y: calc(var(--radius) / 2);
    width: var(--strokeWidth);
    height: var(--radius);
    fill: black;
  }
  & .plusH {
    x: calc(var(--radius) / 2);
    y: calc(var(--maskSize) / 2 - var(--strokeWidth) / 2);
    width: var(--radius);
    height: var(--strokeWidth);
    fill: black;
  }
  & circle {
    cx: var(--radius);
    cy: var(--radius);
    r: calc(var(--radius) * var(--hover-animation));
    transition: color 0.2s ease;
  }
}

.hoverArea {
  --margin: 4px;
  --width: calc(var(--radius) * 2 + var(--margin) * 2);
  fill: transparent;
  width: var(--width);
  height: calc(
    var(--node-vertical-gap) + var(--output-port-max-width) + var(--margin) * 2 + var(--topOffset) +
      var(--radius)
  );
  transform: translate(
    calc(var(--port-clip-start) * (100% + 1px) + var(--leftOffset) - var(--width) / 2),
    calc(var(--node-size-y) + var(--output-port-max-width))
  );
  cursor: pointer;
}
</style>
