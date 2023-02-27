import { FormEvent } from "react";

// FIXME [NP]: doc this?
// eslint-disable-next-line @typescript-eslint/no-unnecessary-type-constraint
export const handleEvent = <T extends any>(callback: () => Promise<T>) => async (event: FormEvent) => {
    event.preventDefault();
    await callback();
};
