import { type ModuleStore } from '$/providers/openedProjects/module'
import {
  printRequiredImport,
  requiredImports,
  type RequiredImport,
} from '$/providers/openedProjects/module/imports'
import { type ProjectNameStore } from '$/providers/openedProjects/projectNames'
import { SuggestionDb } from '$/providers/openedProjects/suggestionDatabase'
import {
  entryDisplayPath,
  entryIsStatic,
  SuggestionKind,
  type SuggestionEntry,
} from '$/providers/openedProjects/suggestionDatabase/entry'
import type {
  Choice,
  WidgetConfiguration,
} from '$/providers/openedProjects/widgetRegistry/configuration'
import type { SubmenuEntry } from '@/components/GraphEditor/widgets/WidgetSelection/submenuEntry'
import { Ast } from '@/util/ast'
import type { Opt } from '@/util/data/opt'
import { isIconName, type Icon } from '@/util/iconMetadata/iconName'
import { ProjectPath } from '@/util/projectPath'
import { qnLastSegment, tryQualifiedName } from '@/util/qualifiedName'
import type { ToValue } from '@/util/reactivity'
import { computed, toValue, type ComputedRef, type Ref, type VNode } from 'vue'

/**
 * The most basic dropdown item. When you click on it, the expression is inserted.
 */
export class ExpressionTag {
  private cachedExpressionAst: Ast.Expression | undefined

  /**
   * @param expression - The expression to insert when this item is clicked.
   * @param explicitLabel - If provided, this label will be used instead of the stringified expression.
   * @param explicitIcon - If provided, this icon will be displayed with the label
   * @param requiredImports - The imports required by the expression, will be added to the code when the item is clicked.
   */
  constructor(
    readonly expression: string,
    readonly explicitLabel?: Opt<string>,
    private explicitIcon?: Opt<Icon>,
    readonly requiredImports?: RequiredImport[],
  ) {}

  /**
   * Create a new {@link ExpressionTag} from qualified path to a suggestion entry.
   */
  static FromProjectPath(
    suggestionDb: SuggestionDb,
    path: ProjectPath,
    label?: Opt<string>,
  ): ExpressionTag | null {
    const entry = suggestionDb.getEntryByProjectPath(path)
    if (entry) return ExpressionTag.FromEntry(suggestionDb, entry, label)
    else return null
  }

  /**
   * Create a new {@link ExpressionTag} from a string expression.
   */
  static FromExpression(
    suggestionDb: SuggestionDb,
    projectNames: ProjectNameStore,
    expression: string,
    label?: Opt<string>,
    icon?: Opt<string>,
  ): ExpressionTag {
    const qn = tryQualifiedName(expression)
    if (qn.ok) {
      const projectPath = projectNames.parseProjectPath(qn.value)
      if (projectPath.ok) {
        const fromProjPath = ExpressionTag.FromProjectPath(suggestionDb, projectPath.value, label)
        if (fromProjPath) return fromProjPath
      }
      return new ExpressionTag(
        qn.value,
        label ?? qnLastSegment(qn.value),
        icon && isIconName(icon) ? (icon as Icon) : undefined,
      )
    }
    return new ExpressionTag(
      expression,
      label,
      icon && isIconName(icon) ? (icon as Icon) : undefined,
    )
  }

  /**
   * Create a new {@link ExpressionTag} from a suggestion entry.
   */
  static FromEntry(
    suggestionDb: SuggestionDb,
    entry: SuggestionEntry,
    label?: Opt<string>,
  ): ExpressionTag {
    const expression =
      entryIsStatic(entry) ? entryDisplayPath(entry)
      : entry.kind === SuggestionKind.Method ? `_.${entry.name}`
      : entry.name
    return new ExpressionTag(
      expression,
      label ?? entry.name,
      undefined,
      requiredImports(suggestionDb, entry),
    )
  }

  /**
   * Get the displayed label for this tag.
   */
  get label() {
    return this.explicitLabel ?? this.expression
  }

  /**
   * Get the displayed icon for this tag.
   */
  get icon() {
    return this.explicitIcon ?? undefined
  }

  /**
   * Get the parsed expression AST for this tag.
   */
  get expressionAst() {
    if (this.cachedExpressionAst == null) {
      this.cachedExpressionAst = Ast.parseExpression(this.expression)
    }
    return this.cachedExpressionAst
  }

  /**
   * Create a non user-facing string representation of expression tag. Meant for key generation and debugging.
   */
  toString() {
    return `${this.label}[${this.requiredImports?.map(printRequiredImport).join(',')}]`
  }

  /**
   * Add any needed imports to the provided module, and return the expression with any necessary
   * qualification.
   */
  resolveExpression(edit: Ast.MutableModule, module: ModuleStore) {
    if (this.requiredImports) {
      const conflicts = module.addMissingImports(edit, this.requiredImports)
      if (conflicts != null && conflicts.length > 0) {
        // TODO: Substitution does not work, because we interpret imports wrongly. To be fixed in
        // https://github.com/enso-org/enso/issues/9356
        // And here it was wrong anyway: we should replace only conflicting name, not entire expression!
        // // Is there is a conflict, it would be a single one, because we only ask about a single entry.
        // return conflicts[0]?.fullyQualified!
      }
    }
    // Unless a conflict occurs, we use the selected expression as is.
    return this.expression
  }
}

/**
 * A dropdown item that contains a list of other dropdown items.
 */
export class NestedChoiceTag {
  /**
   * Create a new {@link NestedChoiceTag}.
   */
  constructor(
    private internalLabel: string,
    readonly choices: (ExpressionTag | NestedChoiceTag)[],
  ) {}

  /**
   * Get the label for this tag.
   */
  get label(): string {
    return this.internalLabel + ' →'
  }

  /**
   * Recursively flatten this tag into a list of {@link ExpressionTag}s. Used when filtering the dropdown content.
   * @param prefix - The prefix to add to the label of each choice.
   */
  flatten(prefix: string = ''): ExpressionTag[] {
    const result: ExpressionTag[] = []
    for (const choice of this.choices) {
      if (choice instanceof ExpressionTag) {
        const newLabel = prefix + this.internalLabel + ' → ' + choice.label
        result.push(
          new ExpressionTag(choice.expression, newLabel, choice.icon, choice.requiredImports),
        )
      } else if (choice instanceof NestedChoiceTag) {
        result.push(...choice.flatten(prefix + this.internalLabel + ' → '))
      }
    }
    return result
  }
}

/**
 * A dropdown item that performs an action when clicked.
 */
export class ActionTag {
  /**
   * Create a new {@link ActionTag}.
   */
  constructor(
    readonly label: string,
    readonly icon: Icon | undefined,
    readonly onClick: (dropdownActions: Actions) => void,
  ) {}

  /**
   * Create a new {@link ActionTag} from a {@link CustomDropdownItem}.
   */
  static FromItem(item: CustomDropdownItem): ActionTag {
    return new ActionTag(item.label, item.icon, item.onClick)
  }
}

/** Custom item added to dropdown. These items can’t be selected, but can be clicked. */
export interface CustomDropdownItem {
  /** Displayed label. */
  label: string
  /** Displayed icon. */
  icon?: Icon | undefined
  /** Action to perform when clicked. */
  onClick: (dropdownActions: Actions) => void
}

/** Actions a {@link CustomDropdownItem} may perform when clicked. */
export interface Actions {
  /**
   * Provide an alternative dialog to be rendered in place of the dropdown.
   *
   * For example, the {@link WidgetCloudBrowser} installs a custom entry that, when clicked,
   * opens a file browser where the dropdown was.
   * @param keepAlive - when set, the `activity` instance will be kept between drop-down closing
   *  and opening. The activity component must not change it type (when being a ref) and provide
   * `name` option explicitly.
   */
  setActivity: (activity: ToValue<VNode>, keepAlive?: boolean) => void
  close: () => void
}

/** A helper type for all possible dropdown entries. */
export interface Entry extends SubmenuEntry<Entry> {
  tag: ExpressionTag | NestedChoiceTag | ActionTag
}

export interface DynamicConfigTagsOptions {
  dynamicConfig: ToValue<Opt<WidgetConfiguration>>
  staticTags: ToValue<Opt<string[]>>
  suggestionDb: ToValue<SuggestionDb>
  projectNames: Readonly<Ref<ProjectNameStore>>
}
/** @returns The dropdown tags applicable to an expression. */
export function useExpressionTags({
  dynamicConfig,
  staticTags,
  suggestionDb,
  projectNames,
}: DynamicConfigTagsOptions): ComputedRef<(ExpressionTag | NestedChoiceTag)[]> {
  const staticExpressionTags = computed((): ExpressionTag[] | null => {
    const tags = toValue(staticTags)
    if (tags == null) return null
    return tags.map((t) =>
      ExpressionTag.FromExpression(toValue(suggestionDb), projectNames.value, t),
    )
  })
  const dynamicTags = computed((): (ExpressionTag | NestedChoiceTag)[] | null => {
    const config = toValue(dynamicConfig)
    if (config?.kind !== 'Single_Choice' && config?.kind !== 'Multiple_Choice') return null

    const choiceToTag = (choice: Choice): ExpressionTag | NestedChoiceTag =>
      Array.isArray(choice.value) ?
        new NestedChoiceTag(choice.label ?? '…', choice.value.map(choiceToTag))
      : ExpressionTag.FromExpression(
          toValue(suggestionDb),
          projectNames.value,
          choice.value,
          choice.label,
          choice.icon,
        )

    return config.values.map(choiceToTag)
  })
  return computed(() => dynamicTags.value ?? staticExpressionTags.value ?? [])
}

/** @returns Dropdown {@link Entry}s for the given tags. */
export function useTagEntries<T extends ExpressionTag | NestedChoiceTag | ActionTag>(
  tags: ToValue<T[]>,
  isSelected: (expression: string) => boolean,
): ComputedRef<(Entry & { tag: T | ExpressionTag | NestedChoiceTag })[]> {
  function tagToEntry(
    tag: T | ExpressionTag | NestedChoiceTag,
  ): Entry & { tag: T | ExpressionTag | NestedChoiceTag } {
    return {
      value: tag.label,
      key: tag instanceof ExpressionTag ? tag.toString() : undefined,
      selected: tag instanceof ExpressionTag && isSelected(tag.expression),
      icon: tag instanceof ExpressionTag || tag instanceof ActionTag ? tag.icon : undefined,
      tag,
      isNested: tag instanceof NestedChoiceTag,
      nestedValues: tag instanceof NestedChoiceTag ? tag.choices.map(tagToEntry) : [],
    }
  }
  return computed((): (Entry & { tag: T | ExpressionTag | NestedChoiceTag })[] =>
    toValue(tags).map(tagToEntry),
  )
}
