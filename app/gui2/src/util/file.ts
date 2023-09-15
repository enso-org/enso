export function fileName(path: string) {
  const [, name] = path.match(/([^/\\])(?:\.[^/\\.]+)?$/) ?? []
  return name
}
