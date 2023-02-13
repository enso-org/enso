import { useState } from "react";



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
const useInput = (initialValue: string) => {
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

export { useInput }
