/** @file Module containing utility functions used throughout our Dashboard code, but that don't fit
 * anywhere else. */
import * as react from 'react'

export const handleEvent =
    <T>(callback: () => Promise<T>) =>
    async (event: react.FormEvent) => {
        event.preventDefault()
        await callback()
    }
