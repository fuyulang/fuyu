// SPDX-License-Identifier: MPL-2.0

pub fn fuyu() -> &'static str {
    "fuyu"
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = fuyu();
        assert_eq!(result, "fuyu");
    }
}
