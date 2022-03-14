// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]



#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
