pub fn did_you_mean<S: AsRef<str>, T: AsRef<str>>(
    entries: impl Iterator<Item = S>,
    user_input: T,
) -> Option<String> {
    if user_input.as_ref().len() < 3 {
        return None;
    }

    let input_lower = user_input.as_ref().to_lowercase();

    entries
        .map(|ref id| {
            let id_lower = id.as_ref().to_lowercase();
            let edit_dist = strsim::damerau_levenshtein(&id_lower, &input_lower);

            // Also consider substring matches: if the input is a substring
            // of the entry (e.g. "Density" matching "MassDensity"), use half
            // the length difference as the effective distance, since a
            // substring match is a stronger signal than random edits.
            let substr_dist = if input_lower.len() >= 3 && id_lower.contains(&input_lower) {
                (id_lower.len() - input_lower.len()) / 2
            } else {
                usize::MAX
            };

            (id.as_ref().to_string(), edit_dist.min(substr_dist))
        })
        .min_by(|(id_a, dist_a), (id_b, dist_b)| dist_a.cmp(dist_b).then(id_a.cmp(id_b)))
        .filter(|(id, dist)| id.len() >= 2 && *dist <= 3)
        .map(|(id, _)| id)
}
