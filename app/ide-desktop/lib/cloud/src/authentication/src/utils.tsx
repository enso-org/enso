import { FormEvent } from "react";

export const handleEvent = (callback: () => Promise<void>) => async (event: FormEvent) => {
    event.preventDefault();
    await callback();
};
