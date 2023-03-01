import * as react from "react";

export const handleEvent = <T>(callback: () => Promise<T>) => async (event: react.FormEvent) => {
    event.preventDefault();
    await callback();
};
