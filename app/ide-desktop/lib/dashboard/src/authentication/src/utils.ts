import { FormEvent } from "react";

export const handleEvent = <T>(callback: () => Promise<T>) => async (event: FormEvent) => {
    event.preventDefault();
    await callback();
};
