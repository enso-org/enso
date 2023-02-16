import { DependencyList, useEffect, useState } from "react";
import { Logger } from "./components/app";



// ================
// === useInput ===
// ================

/**
 * A custom hook to handle input fields.
 *
 * In React, managing state (e.g., user input values) must be done via the `useState` hook, which
 * returns a prop (e.g., `value`) containing the current value of the state, and a function (e.g.,
 * `setValue`) to update the state. Because of this, to bind a `value` to an input field, we must
 * use the `value` prop and the `onChange` event handler. However, this can be tedious to do for
 * every input field, so we can use a custom hook to handle this for us.
 */
export const useInput = (initialValue: string) => {
    const [value, setValue] = useState(initialValue);
    const onChange = (event: React.ChangeEvent<HTMLInputElement>) => setValue(event.target.value);
    const bind = {
        value,
        onChange,
    };

    return {
        value,
        bind,
    };
}


// ======================
// === useAsyncEffect ===
// ======================

// FIXME [NP]: docs: https://stackoverflow.com/questions/61751728/asynchronous-calls-with-react-usememo
// https://devtrium.com/posts/async-functions-useeffect
// FIXME [NP]: use useLogger here
export const useAsyncEffect = (logger: Logger, fetch: <T>() => Promise<T>, deps?: DependencyList) => {
    const [value, setValue] = useState(undefined);

    useEffect(() => {
        let active = true;

        // Declare the async data fetching function.
        const load = async () => {
            const result = await fetch();

            // Set state with the result if `active` is true.
            if (!active) return;
            // FIXME [NP]: make this type safe
            // @ts-expect-error
            setValue(result);
        }

        load()
            // FIXME [NP]: use logger.error here
            .catch(error => logger.log("Error while fetching data", error));

        // Cancel any future `setValue` calls.
        return () => { active = false }
    }, deps)

    return {
        value,
    };
}
