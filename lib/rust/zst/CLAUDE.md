# enso-zst

Zero-sized-type utility. Provides `Zst<T>` as a clearer, `Serialize`-able
replacement for bare `PhantomData<T>`. Use when you want to tag a value (e.g. a
unit for dimensional analysis) without paying any size.

Published to crates.io. Kept dependency-light (`serde` only).
