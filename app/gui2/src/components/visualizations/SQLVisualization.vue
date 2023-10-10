<script lang="ts">
export const name = 'SQL Query'
export const inputType = 'Standard.Database.Data.Table.Table | Standard.Database.Data.Column.Column'
export const defaultPreprocessor = [
  'Standard.Visualization.SQL.Visualization',
  'prepare_visualization',
] as const

/**
 * A visualization that pretty-prints generated SQL code and displays type hints related to
 * interpolated query parameters.
 */

type Data = SQLData | Error

interface SQLData {
  error: undefined
  dialect: string
  code: string
  interpolations: SQLInterpolation[]
}

interface SQLInterpolation {
  enso_type: string
  value: string
}

interface Error {
  error: string
  dialect: undefined
  code: undefined
  interpolations: undefined
}

declare const sqlFormatter: typeof import('sql-formatter')
</script>

<script setup lang="ts">
import { computed } from 'vue'

// @ts-expect-error
// eslint-disable-next-line no-redeclare
import * as sqlFormatter from 'https://cdn.jsdelivr.net/npm/sql-formatter@13.0.0/+esm'

import VisualizationContainer from '@/components/VisualizationContainer.vue'
import { DEFAULT_THEME, type RGBA, type Theme } from './builtins.ts'

const props = defineProps<{ data: Data }>()

const theme: Theme = DEFAULT_THEME

const language = computed(() =>
  props.data.dialect != null && sqlFormatter.supportedDialects.includes(props.data.dialect)
    ? props.data.dialect
    : 'sql',
)
const formatted = computed(() => {
  if (props.data.error != null) {
    return undefined
  }
  const params = props.data.interpolations.map((param) =>
    renderInterpolationParameter(theme, param),
  )

  return sqlFormatter.format(props.data.code, {
    params: params,
    language: language.value,
  })
})

/** The qualified name of the Text type. */
const TEXT_TYPE = 'Builtins.Main.Text'
/** Specifies opacity of interpolation background color. */
const INTERPOLATION_BACKGROUND_OPACITY = 0.2

// === Handling Colors ===

/** Render a 4-element array representing a color into a CSS-compatible rgba string. */
function convertColorToRgba(color: RGBA) {
  const r = 255 * color.red
  const g = 255 * color.green
  const b = 255 * color.blue
  const a = color.alpha
  return 'rgba(' + r + ',' + g + ',' + b + ',' + a + ')'
}

/** Replace the alpha component of a color (represented as a 4-element array),
 * returning a new color. */
function replaceAlpha(color: RGBA, newAlpha: number) {
  return {
    red: color.red,
    green: color.green,
    blue: color.blue,
    alpha: newAlpha,
  }
}

/**
 * Renders HTML for displaying an Enso parameter that is interpolated into the SQL code.
 */
function renderInterpolationParameter(theme: Theme, param: { enso_type: string; value: string }) {
  const actualType = param.enso_type
  let value = param.value

  if (actualType === TEXT_TYPE) {
    value = "'" + value.replace(/'/g, "''") + "'"
  }

  const actualTypeColor = theme.getColorForType(actualType)
  const fgColor = actualTypeColor
  let bgColor = replaceAlpha(fgColor, INTERPOLATION_BACKGROUND_OPACITY)

  return renderRegularInterpolation(value, fgColor, bgColor)
}

/**
 * A helper that renders the HTML representation of a regular SQL interpolation.
 */
function renderRegularInterpolation(value: string, fgColor: RGBA, bgColor: RGBA) {
  let html = `<div class="interpolation" style="color:${convertColorToRgba(
    fgColor,
  )};background-color:${convertColorToRgba(bgColor)};">`
  html += value
  html += '</div>'
  return html
}
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <div class="sql-visualization scrollable">
      <pre v-if="data.error" class="sql" v-text="data.error"></pre>
      <!-- eslint-disable-next-line vue/no-v-html This is SAFE, beause it is not user input. -->
      <pre v-else class="sql" v-html="formatted"></pre>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
@import url('https://fonts.cdnfonts.com/css/dejavu-sans-mono');

.SQLVisualization {
  padding: 4px;
}
</style>

<style>
.SQLVisualization .sql {
  font-family: 'DejaVu Sans Mono', monospace;
  font-size: 12px;
  margin-left: 7px;
  margin-top: 5px;
}

.SQLVisualization .interpolation {
  border-radius: 6px;
  padding: 1px 2px 1px 2px;
  display: inline;
}

.SQLVisualization .mismatch-parent {
  position: relative;
  display: inline-flex;
  justify-content: center;
}

.SQLVisualization .mismatch-mouse-area {
  display: inline;
  position: absolute;
  width: 150%;
  height: 150%;
  align-self: center;
  z-index: 0;
}

.SQLVisualization .mismatch {
  z-index: 1;
}

.SQLVisualization .modulepath {
  color: rgba(150, 150, 150, 0.9);
}

.SQLVisualization .tooltip {
  font-family: DejaVuSansMonoBook, sans-serif;
  font-size: 12px;
  opacity: 0;
  transition: opacity 0.2s;
  display: inline-block;
  white-space: nowrap;
  background-color: rgba(249, 249, 249, 1);
  box-shadow: 0 0 16px rgba(0, 0, 0, 0.16);
  text-align: left;
  border-radius: 6px;
  padding: 5px;
  position: absolute;
  z-index: 99999;
  pointer-events: none;
}
</style>
