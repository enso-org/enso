import type { SuggestionId } from '@/stores/suggestionDatabase/entry'
import type { ComputedRef, Ref } from 'vue'
import { computed, reactive, ref } from 'vue'

/** Simple stack for going forward and backward through the history of visited documentation pages */
export class HistoryStack {
  private stack: SuggestionId[]
  private index: Ref<number>
  public current: ComputedRef<SuggestionId | undefined>

  /**
   * Initializes the history stack.
   */
  constructor() {
    this.stack = reactive([])
    this.index = ref(0)
    this.current = computed(() => this.stack[this.index.value] ?? undefined)
  }

  /**
   * Resets the history stack to contain only the current suggestion.
   * @param current - The current suggestion ID to reset the stack with.
   */
  public reset(current: SuggestionId) {
    this.stack.length = 0
    this.stack.push(current)
    this.index.value = 0
  }

  /**
   * Adds a new suggestion ID to the history stack, removing any forward history.
   * @param id - The suggestion ID to record.
   */
  public record(id: SuggestionId) {
    this.stack.splice(this.index.value + 1)
    this.stack.push(id)
    this.index.value = this.stack.length - 1
  }

  /**
   * Moves the history index forward by one step if possible.
   */
  public forward() {
    if (this.canGoForward()) {
      this.index.value += 1
    }
  }

  /**
   * Navigates backward in the history if possible.
   */
  public backward() {
    if (this.canGoBackward()) {
      this.index.value -= 1
    }
  }

  /** @returns whether or not it is possible to navigate back in history from current position. */
  public canGoBackward(): boolean {
    return this.index.value > 0
  }

  /** @returns whether or not it is possible to navigate forward in history from current position. */
  public canGoForward(): boolean {
    return this.index.value < this.stack.length - 1
  }
}
