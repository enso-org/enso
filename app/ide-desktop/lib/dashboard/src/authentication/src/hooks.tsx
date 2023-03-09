/** @file Module containing common custom React hooks used throughout out Dashboard. */
import * as React from "react";
import * as loggerProvider from "./providers/logger";



// ======================
// === useAsyncEffect ===
// ======================

/** A React hook for re-rendering a component once an asynchronous call is over.
 *
 * This hook will take care of setting an initial value for the component state (so that it can
 * render immediately), updating the state once the asynchronous call is over (to re-render the
 * component), and cancelling any in-progress asynchronous calls when the component is unmounted (to
 * avoid race conditions where "update 1" starts, "update 2" starts and finishes, then "update 1"
 * finishes and sets the state).
 *
 * For further details, see: https://devtrium.com/posts/async-functions-useeffect
 * Also see: https://stackoverflow.com/questions/61751728/asynchronous-calls-with-react-usememo
 *
 * @param initialValue - The initial value of the state controlled by this hook.
 * @param fetch - The asynchronous function used to load the state controlled by this hook.
 * @param deps - The list of dependencies that, when updated, trigger the asynchronous fetch.
 * @returns value - The current value of the state controlled by this hook. */
export function useAsyncEffect<T>(
    initialValue: T,
    fetch: () => Promise<T>,
    deps?: React.DependencyList
): [T] {
    const logger = loggerProvider.useLogger();
    const [value, setValue] = React.useState<T>(initialValue);

    React.useEffect(() => {
        let active = true;

        // Declare the async data fetching function.
        const load = async () => {
            const result = await fetch();

            // Set state with the result if `active` is true. This is what prevents race conditions.
            if (!active) return;
            setValue(result);
        };

        load().catch((error) => logger.error("Error while fetching data", error));

        // Cancel any future `setValue` calls.
        return () => {
            active = false;
        };
    }, deps);

    return [value];
}
