import * as fs from 'node:fs/promises'

/** @type {{ name: string | undefined, inputType: string | undefined }[]} */
const visualizationMetadata = []

const order = [
  'JSON',
  'Table',
  'Scatter Plot',
  'Histogram',
  'Heatmap',
  'SQL Query',
  'Geo Map',
  'Image',
  'Warnings',
]

console.info('Listing and reading visualization files...')
for (const path of [
  ...(await fs.readdir('./public/visualizations')).map((path) => './public/visualizations/' + path),
  ...(await fs.readdir('./src/components/visualizations')).map(
    (path) => './src/components/visualizations/' + path,
  ),
]) {
  if (!path.endsWith('Visualization.vue')) continue
  const file = await fs.readFile(path, 'utf-8')
  const name = file.match(/(?<=export\s+const\s+name\s+=\s+['"])[^'"]*/)?.[0]
  const inputType = file.match(/(?<=export\s+const\s+inputType\s+=\s+['"])[^'"]*/)?.[0]
  visualizationMetadata.push({ name, inputType })
}
visualizationMetadata.sort((a, b) => order.indexOf(a.name ?? '') - order.indexOf(b.name ?? ''))

console.info('Writing visualization metadata to "./src/stores/visualization/metadata.json"...')
await fs.writeFile(
  './src/stores/visualization/metadata.json',
  JSON.stringify(visualizationMetadata, undefined, 2) + '\n',
)
console.info('Done.')
