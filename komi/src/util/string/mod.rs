pub fn is_digit(s: &str) -> bool {
    s.len() == 1 && s.chars().next().unwrap().is_ascii_digit()
}

pub fn is_whitespace(s: &str) -> bool {
    (s.len() == 1 && s.chars().next().unwrap().is_ascii_whitespace()) || (s == "\r\n")
}
