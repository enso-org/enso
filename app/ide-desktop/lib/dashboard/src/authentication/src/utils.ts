/** @file Module containing utility functions used throughout our Dashboard code, but that don't fit
 * anywhere else. */

export function handleEvent<T>(callback: () => Promise<T>) {
    return async (event: React.FormEvent) => {
        event.preventDefault()
        await callback()
    }
}
