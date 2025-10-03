import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import type { ToolbarItem } from '@/components/visualizations/toolbar'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import type { ToValue } from '@/util/reactivity'
import { computed, type ComputedRef, type Ref, toValue } from 'vue'
import type { Expression, MutableExpression } from 'ydoc-shared/ast'
import type { TextFormatOptions } from '../TableVisualization.vue'
import {
  actionMap,
  type FilterAction,
  type FilterType,
  type FilterValueRange,
  getFilterValue,
  type GridFilterModel,
} from './tableVizFilterUtils'

type SortDirection = 'asc' | 'desc'
export type SortModel = {
  columnName: string
  sortDirection: SortDirection
  sortIndex: number
}

export interface SortFilterNodesButtonOptions {
  filterModel: ToValue<GridFilterModel[]>
  sortModel: ToValue<SortModel[]>
  isCreateNewNodeEnabled: ToValue<boolean>
  createNodes: (...options: NodeCreationOptions[]) => void
  getColumnValueToEnso: (
    columnName: string,
  ) => (columnValue: string, module: Ast.MutableModule) => Ast.Owned<Ast.MutableAst>
}

export interface FormatMenuOptions {
  textFormatterSelected: Ref<TextFormatOptions>
}

export interface ColumnNodeButton {
  isCreateNewNodeEnabled: ToValue<boolean>
  createNodes: (...options: NodeCreationOptions[]) => void
  hiddenColumns: ToValue<string[]>
  vizColumnOrder: ToValue<string[] | null>
}

interface NewNodeOptions extends SortFilterNodesButtonOptions, ColumnNodeButton {
  tableFilteredOrSorted: ToValue<boolean>
}

export interface RefreshButtonOptions {
  refreshGrid: () => void
  tableFilteredOrSorted: ToValue<boolean>
}

export interface Options extends NewNodeOptions, FormatMenuOptions, RefreshButtonOptions {}

type ConstructivePattern = (
  placeholder: Ast.Owned<Ast.MutableExpression>,
) => Ast.Owned<Ast.MutableExpression>

/***
 * function that returns a toolbar button item used to apply new nodes to the graph reflecting the sort filter and column changes applied to the current table visualization
 *
 * @param {FilterModel} options.filterModel - The current filter model applied to the table.
 * @param {SortModel} options.sortModel - The current sort model applied to the table.
 * @param {boolean} options.tableFilteredOrSorted - Are there changes to the table viz sorting or filtering. Used to enable/disable buttons
 * @param {boolean} options.isCreateNewNodeEnabled - Whether the functionality to create new nodes is enabled, only enabled for tables (i.e not rows, vectors).
 * @param options.createNodes - Function to trigger creation of new nodes.
 * @param {(columnId: string, value: unknown) => EnsoValue} options.getColumnValueToEnso - Function to convert column values to a format compatible with Enso.
 * @param {string[]} options.hiddenColumns - A list of column Ids that are currently hidden.
 * @param {string[] | null} options.vizColumnOrder - A list of all column Ids in their new order; if the order is unchanged, this will be null.
 *
 * @returns {ComputedRef<ToolbarItem | undefined>} A computed reference to a toolbar item,
 * or undefined if the button should not be rendered.
 */
function useSortFilterNodesButton({
  filterModel,
  sortModel,
  tableFilteredOrSorted,
  isCreateNewNodeEnabled,
  createNodes,
  getColumnValueToEnso,
  hiddenColumns,
  vizColumnOrder,
}: NewNodeOptions): ComputedRef<ToolbarItem | undefined> {
  const sortPattern = computed(() => Pattern.parseExpression('(..Name __ __ )')!)
  const filterPattern = computed(() => Pattern.parseExpression('__ (__ __)')!)
  const filterBetweenPattern = computed(() => Pattern.parseExpression('__ (..Between __ __)')!)
  const filterNothingPattern = computed(() => Pattern.parseExpression('__ __')!)

  function createSimpleAstCall(
    name: string,
    arg: Ast.Owned<MutableExpression>,
    ast: Ast.Owned<Ast.MutableExpression>,
  ) {
    return Ast.App.positional(Ast.PropertyAccess.new(ast.module, ast, Ast.identifier(name)!), arg)
  }

  function createFilterCall(
    ast: Ast.Owned<Ast.MutableExpression>,
    expr: Ast.Owned<MutableExpression>,
    parentPattern: Ast.Owned<Ast.MutableExpression>,
  ) {
    const style = {
      spaced: parentPattern !== undefined,
    }
    return Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, parentPattern, Ast.identifier('filter')!, style),
      expr,
    )
  }

  function buildFilterExpression(
    ast: Ast.Owned<Ast.MutableExpression>,
    columnName: string,
    filterType: FilterType,
    filterAction: FilterAction,
    value?: string[] | string | FilterValueRange,
  ) {
    const column = Ast.TextLiteral.new(columnName)
    const valueFormatter = getColumnValueToEnso(columnName)

    if (filterAction === 'blank' || filterAction === 'notBlank') {
      return filterNothingPattern.value.instantiateCopied([
        column,
        Ast.parseExpression(actionMap[filterAction])!,
      ])
    }

    if (filterType === 'set') {
      if (Array.isArray(value) && value.length === 1) {
        return filterPattern.value.instantiateCopied([
          column,
          Ast.parseExpression('..Equal')!,
          valueFormatter(value[0]!, ast.module) as Expression | MutableExpression,
        ])
      }
      return filterPattern.value.instantiateCopied([
        column,
        Ast.parseExpression('..Is_In')!,
        Ast.Vector.build(
          value as string[],
          (element, tempModule) => valueFormatter(element, tempModule),
          ast.module,
        ),
      ])
    }

    if (filterType === 'number') {
      if (filterAction === 'inRange' && typeof value === 'object') {
        const { fromValue, toValue } = value as FilterValueRange
        return filterBetweenPattern.value.instantiateCopied([
          column,
          valueFormatter(fromValue, ast.module) as Expression | MutableExpression,
          valueFormatter(toValue, ast.module) as Expression | MutableExpression,
        ])
      }

      return filterPattern.value.instantiateCopied([
        column,
        Ast.parseExpression(actionMap[filterAction])!,
        valueFormatter(value as string, ast.module) as Expression | MutableExpression,
      ])
    }

    return filterPattern.value.instantiateCopied([
      column,
      Ast.parseExpression(actionMap[filterAction])!,
      valueFormatter(value as string, ast.module) as Expression | MutableExpression,
    ])
  }

  function buildPattern(
    ast: Ast.Owned<Ast.MutableExpression>,
    parentPattern: Ast.Owned<Ast.MutableExpression>,
    columnName: string,
    filterType: FilterType,
    filterAction: FilterAction,
    value?: string[] | string | FilterValueRange,
  ) {
    const filterExpr = buildFilterExpression(ast, columnName, filterType, filterAction, value)
    return createFilterCall(ast, filterExpr, parentPattern)
  }

  const sortDirection = computed(() => ({
    asc: '..Ascending',
    desc: '..Descending',
  }))

  function makeSortPattern(ast: Ast.Owned<Ast.MutableExpression>) {
    const sorts = toValue(sortModel)
      .filter((s) => s?.columnName)
      .sort((a, b) => a.sortIndex - b.sortIndex)
      .map((sort) =>
        sortPattern.value.instantiateCopied([
          Ast.TextLiteral.new(sort.columnName),
          Ast.parseExpression(sortDirection.value[sort.sortDirection as SortDirection])!,
        ]),
      )
    return Ast.Vector.new(ast.module, sorts)
  }

  function getAstPatternSort(): Pattern {
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('sort')!),
        makeSortPattern(ast),
      ),
    )
  }

  function createNewNodes() {
    const patterns: Pattern[] = []
    const filters = toValue(filterModel)
    const sorts = toValue(sortModel)

    if (sorts.length > 0) {
      patterns.push(getAstPatternSort())
    }

    const columnsToRemove = toValue(hiddenColumns)
    if (columnsToRemove.length) {
      patterns.push(
        Pattern.new((ast) =>
          createSimpleAstCall(
            'remove_columns',
            Ast.Vector.build(columnsToRemove, Ast.TextLiteral.new, ast.module),
            ast,
          ),
        ),
      )
    }

    const columnOrder = toValue(vizColumnOrder)
    if (columnOrder?.length) {
      patterns.push(
        Pattern.new((ast) =>
          createSimpleAstCall(
            'reorder_columns',
            Ast.Vector.build(columnOrder, Ast.TextLiteral.new, ast.module),
            ast,
          ),
        ),
      )
    }

    function projector(parentPattern: ConstructivePattern | undefined) {
      return (columnName: any, filterAction: any, filterType: any, value: any) =>
        (ast: Ast.Owned<Ast.MutableExpression>) => {
          const parentPat = parentPattern ? parentPattern(ast) : ast
          return buildPattern(ast, parentPat, columnName, filterAction, filterType, value)
        }
    }

    let filterPatterns = new Array<ConstructivePattern>()
    for (const filter of filters) {
      const { columnName, filterAction, filterType } = filter
      const value = getFilterValue(filter)

      filterPatterns = (filterPatterns.length ? filterPatterns : [undefined]).flatMap((parent) => {
        const projectionFunction = projector(parent)
        const pattern = projectionFunction(columnName, filterType, filterAction, value)
        return [pattern]
      })
    }

    createNodes(
      ...filterPatterns.map(
        (pattern) =>
          ({ content: Pattern.new(pattern), commit: true }) satisfies NodeCreationOptions,
      ),
    )

    createNodes(
      ...patterns.map(
        (pattern) => ({ content: pattern, commit: true }) satisfies NodeCreationOptions,
      ),
    )
  }

  const createNodesButton: ToolbarItem = {
    icon: 'add_to_graph_editor',
    title: "Create new component(s) with the current grid's state applied to the workflow",
    disabled: computed(() => !toValue(tableFilteredOrSorted)),
    onClick: createNewNodes,
  }

  return computed(() => (toValue(isCreateNewNodeEnabled) ? createNodesButton : undefined))
}
function createFormatMenu({ textFormatterSelected }: FormatMenuOptions): ToolbarItem {
  return {
    selected: textFormatterSelected,
    title: 'Text Display Options',
    options: {
      full: {
        icon: 'paragraph',
        iconStyle: {
          stroke: 'black',
          color: 'black',
        },
        title:
          'Text displayed in monospace font and all whitespace characters displayed as symbols',
        label: 'Full whitespace rendering',
      },
      partial: {
        icon: 'paragraph',
        iconStyle: {
          stroke: '#525252',
          color: '#525252',
        },
        title: 'Text displayed in monospace font, only multiple spaces displayed with "\xB7"',
        label: 'Partial whitespace rendering',
      },
      off: {
        icon: 'not_paragraph',
        title: 'No formatting applied to text',
        label: 'No whitespace rendering',
      },
    },
  }
}

function createRefreshMenu({
  refreshGrid,
  tableFilteredOrSorted,
}: RefreshButtonOptions): ToolbarItem {
  return {
    title: 'Reset any sort, filter or column changes made to the table',
    icon: 'undo',
    disabled: computed(() => !toValue(tableFilteredOrSorted)),
    onClick: refreshGrid,
  }
}

/** TODO: Add docs */
export function useTableVizToolbar(options: Options): ComputedRef<ToolbarItem[]> {
  const createNodesButton = useSortFilterNodesButton(options)
  const formatMenu = createFormatMenu(options)
  const refreshButton = createRefreshMenu(options)
  return computed(() => [
    formatMenu,
    ...(createNodesButton.value ? [createNodesButton.value] : []),
    refreshButton,
  ])
}
