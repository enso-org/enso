import { DropdownEntry } from '@/components/widgets/DropdownWidget.vue'
import { RequiredImport, requiredImports } from '@/stores/graph/imports'
import { ProjectNameStore } from '@/stores/projectNames'
import { SuggestionDbStore } from '@/stores/suggestionDatabase'
import {
  entryDisplayPath,
  entryIsStatic,
  SuggestionEntry,
  SuggestionKind,
} from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { Opt } from '@/util/data/opt'
import { Icon, isIconName } from '@/util/iconMetadata/iconName'
import { ProjectPath } from '@/util/projectPath'
import { qnLastSegment, tryQualifiedName } from '@/util/qualifiedName'
import { type ToValue } from '@/util/reactivity'
import { VNode } from 'vue'

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
    suggestions: SuggestionDbStore,
    path: ProjectPath,
    label?: Opt<string>,
  ): ExpressionTag | null {
    const entry = suggestions.entries.getEntryByProjectPath(path)
    if (entry) return ExpressionTag.FromEntry(suggestions, entry, label)
    else return null
  }

  /**
   * Create a new {@link ExpressionTag} from a string expression.
   */
  static FromExpression(
    suggestions: SuggestionDbStore,
    projectNames: ProjectNameStore,
    expression: string,
    label?: Opt<string>,
    icon?: Opt<string>,
  ): ExpressionTag {
    const qn = tryQualifiedName(expression)
    if (qn.ok) {
      const projectPath = projectNames.parseProjectPath(qn.value)
      if (projectPath.ok) {
        const fromProjPath = ExpressionTag.FromProjectPath(suggestions, projectPath.value, label)
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
    suggestions: SuggestionDbStore,
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
      requiredImports(suggestions.entries, entry),
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
export interface Entry extends DropdownEntry {
  tag: ExpressionTag | NestedChoiceTag | ActionTag
}

/** Check if a {@link DropdownEntry} is an {@link Entry}. */
export function isEntry(entry: DropdownEntry): entry is Entry {
  return (
    'tag' in entry &&
    (entry.tag instanceof ExpressionTag ||
      entry.tag instanceof NestedChoiceTag ||
      entry.tag instanceof ActionTag)
  )
}
