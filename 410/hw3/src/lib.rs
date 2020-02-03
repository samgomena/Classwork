#[derive(Debug, Default)]
pub struct KWIndex<'a>(Vec<&'a str>);

impl<'a> KWIndex<'a> {
    /// Make a new empty target words list.
    pub fn new() -> Self {
        KWIndex(Vec::new())
    }
    /// Parse the `target` text and add the sequence of
    /// valid words contained in it to this `KWIndex`
    /// index.
    ///
    /// This is a "builder method": calls can be
    /// conveniently chained to build up an index.
    ///
    /// Words are separated by whitespace or punctuation,
    /// and consist of a span of one or more consecutive
    /// letters (any UTF-8 character in the "letter" class)
    /// with no internal punctuation.
    ///
    /// For example, the text
    ///
    /// ```text
    /// "It ain't over untïl it ain't, over."
    /// ```
    ///
    /// contains the sequence of words `"It"`, `"over"`,
    /// `"untïl"`, `"it"`, `"over"`.
    ///
    /// # Examples
    ///
    ///
    /// let index = kwindex::KWIndex::new()
    ///     .extend_from_text("Hello world.");
    /// assert_eq!(2, index.len());
    /// assert_eq!(1, index.count_matches("world"));
    /// ```
    pub fn extend_from_text(mut self, target: &'a str) -> Self {
        self.0.extend(
            target
                .split_whitespace() // Split on whitespace
                .map(|s| s.trim_matches(|c: char| c.is_ascii_punctuation())) // Remove leading/trailing punctuation
                .filter(|s| s.chars().all(|c| !c.is_ascii_punctuation())) // Remove words with punctuation in them
                .collect::<Vec<&str>>(), // Specify type with turbofish
        );
        self
    }

    pub fn print(&self) {
        println!("{:?}", self.0)
    }

    /// Count the number of occurrences of the given `keyword`
    /// that are indexed by this `KWIndex`.
    pub fn count_matches(&self, keyword: &str) -> usize {
        self.0
            .iter()
            .filter(|x| x.chars().eq(keyword.chars()))
            .count()
    }

    /// Count the number of words that are indexed by this
    /// `KWIndex`.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Is this index empty?
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_empty() {
        assert!(KWIndex::new().is_empty());
        assert!(!KWIndex::new().extend_from_text("TEST").is_empty());
    }

    #[test]
    fn test_len() {
        assert_eq!(KWIndex::new().len(), 0);
        assert_eq!(KWIndex::new().extend_from_text("TEST").len(), 1);
    }

    #[test]
    fn test_extend_from_text() {
        let index = KWIndex::new().extend_from_text("It ain't over untïl it ain't, over.");
        index.print();
        assert_eq!(5, index.len());
        assert_eq!(1, index.count_matches("it"));
        assert_eq!(0, index.count_matches("ain't"));
    }
    #[test]
    fn test_count_matches() {
        let index = KWIndex::new()
            .extend_from_text("Hello world.")
            .extend_from_text("Hello world.");
        assert_eq!(4, index.len());
        assert_eq!(2, index.count_matches("world"));
    }

    #[test]
    fn test_rand() {
        let index = KWIndex::new()
            .extend_from_text("(text in parentheses)")
            .extend_from_text("[text in brackets]")
            .extend_from_text("text with we|rd punctuation");

        index.print();
        assert_eq!(3, index.count_matches("text"));
    }
}
