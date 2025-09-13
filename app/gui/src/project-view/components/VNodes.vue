<script lang="ts">
import type { VNode } from 'vue'
/**
 * A functional component that renders vnodes passed through props as-is.
 */
export default function VNodes(props: { vnodes: VNode[] }) {
  return props.vnodes
}
</script>
