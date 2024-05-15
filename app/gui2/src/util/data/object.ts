export function selectFields<T extends object>(
  mask: { [P in keyof T]?: boolean },
  value: T,
): Partial<T> {
  return Object.fromEntries(
    Object.entries(mask)
      .filter(([_key, value]) => value)
      .map(([key]) => [key, value[key as keyof T]]),
  ) as Partial<T>
}
