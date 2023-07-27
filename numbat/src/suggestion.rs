pub fn did_you_mean<S: AsRef<str>, T: AsRef<str>>(
    entries: impl Iterator<Item = S>,
    user_input: T,
) -> Option<String> {
    if user_input.as_ref().len() < 3 {
        return None;
    }

    entries
        .map(|ref id| {
            (
                id.as_ref().to_string(),
                strsim::damerau_levenshtein(
                    &id.as_ref().to_lowercase(),
                    &user_input.as_ref().to_lowercase(),
                ),
            )
        })
        .min_by_key(|(_, dist)| *dist)
        .filter(|(id, dist)| id.len() >= 2 && *dist <= 3)
        .map(|(id, _)| id)
}
