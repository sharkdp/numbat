use std::hash::Hash;
use std::{borrow::Borrow, collections::HashMap};

/// A stack of hash maps. All insertions affect the hash map at the top of the
/// stack (which is the last element of the `stack` vector), preserving any
/// entries in maps below. The `save` function can be used to push a new map on
/// the top of the stack, in effect saving the current state of the map, which
/// one can then restore with `restore`.
///
/// The stack vector should never be empty
#[derive(Debug, Clone)]
pub(crate) struct MapStack<K, V> {
    stack: Vec<HashMap<K, V>>,
}

impl<K, V> Default for MapStack<K, V> {
    fn default() -> Self {
        MapStack {
            stack: vec![Default::default()],
        }
    }
}

impl<K: Hash + Eq, V> MapStack<K, V> {
    fn iter_dict(&self) -> impl Iterator<Item = &HashMap<K, V>> {
        self.stack.iter().rev()
    }

    fn iter_dict_mut(&mut self) -> impl Iterator<Item = &mut HashMap<K, V>> {
        self.stack.iter_mut().rev()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.iter_dict().flatten()
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        self.iter_dict_mut().flatten()
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = &K> {
        self.iter_dict().map(|dict| dict.keys()).flatten()
    }

    pub(crate) fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.iter_dict().filter_map(|dict| dict.get(key)).nth(0)
    }

    pub(crate) fn insert(&mut self, key: K, value: V) {
        let _ = self.stack.last_mut().unwrap().insert(key, value);
    }

    pub(crate) fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.iter_dict().any(|dict| dict.contains_key(key))
    }

    /// Remove the top hash map from the stack, making the next one the
    /// current top of the stack, restoring the state of the map before the
    /// last call to save.
    pub(crate) fn restore(&mut self) {
        let _ = self.stack.pop();
        // The stack should never be empty
        assert!(
            !self.stack.is_empty(),
            "Tried to restore the last saved state but nothing was saved"
        );
    }

    /// Save the current state of the map by making the top of the stack a
    /// new empty map.
    pub(crate) fn save(&mut self) {
        self.stack.push(HashMap::default());
    }
}
