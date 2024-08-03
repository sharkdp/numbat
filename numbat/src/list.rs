//! This module defines a custom kind of list used in the [`numbat::Value::List`].
//! The specificities of this list are that:
//! - It's based on a `VecDeque`, which means insertion at the beginning or the end is `O(1)`.
//! - It's stored behind an `Arc`, which makes cloning the value very cheap.
//! - It tries to reduce the number of allocations as much as possible, even when shared between multiple values.
//!   If you are the only owner of the list or while you're not pushing elements in it in, never re-allocate.

use std::{collections::VecDeque, fmt, sync::Arc};

use crate::{value::Value, RuntimeError};

/// Reference counted list / list view
#[derive(Clone, Eq)]
pub struct NumbatList<T> {
    /// The original alloc shared between all values
    alloc: Arc<VecDeque<T>>,
    /// The indexes accessible to us. If `None` we own the whole allocation
    view: Option<(usize, usize)>,
}

impl<T> Default for NumbatList<T> {
    fn default() -> Self {
        Self {
            alloc: Default::default(),
            view: Default::default(),
        }
    }
}

impl<T: fmt::Debug + Clone> fmt::Debug for NumbatList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: PartialEq> PartialEq for NumbatList<T> {
    fn eq(&self, other: &Self) -> bool {
        // Best case scenario, the slice don't have the same len and we can early exit
        if self.len() != other.len() {
            return false;
        }
        // Second best case scenario, the slice comes from the same allocation
        // and have the same view => they are equal
        if Arc::as_ptr(&self.alloc) == Arc::as_ptr(&other.alloc) && self.view == other.view {
            true
        } else {
            // Worst case scenario, we need to compare all the elements one by one
            self.iter().zip(other.iter()).all(|(l, r)| l == r)
        }
    }
}

impl<T> NumbatList<T> {
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        if let Some(view) = self.view {
            view.1 - view.0
        } else {
            self.alloc.len()
        }
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            alloc: Arc::new(VecDeque::with_capacity(capacity)),
            view: None,
        }
    }

    /// Advance the view you have of the list by one.
    pub fn advance_view(&mut self) -> Result<(), RuntimeError> {
        if self.is_empty() {
            return Err(RuntimeError::EmptyList);
        }
        if let Some(view) = &mut self.view {
            view.0 += 1;
            // should be ensured by the if above
            debug_assert!(view.0 <= view.1);
        } else {
            self.view = Some((1, self.len()));
        }
        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.alloc
            .iter()
            .skip(self.view.map_or(0, |(start, _end)| start))
            .take(
                self.view
                    .map_or(self.alloc.len(), |(start, end)| end - start),
            )
    }
}

impl<T: Clone> NumbatList<T> {
    fn make_mut(&mut self) -> (&mut Option<(usize, usize)>, &mut VecDeque<T>) {
        if Arc::strong_count(&self.alloc) != 1 {
            // If someone else is using the list we must clone it
            self.alloc = Arc::new(self.iter().cloned().collect());
            self.view = None;
        }
        // With our usage, this should never allocate since we know we're the only
        // one holding a reference to this `Arc` and we don't use weak references.
        (&mut self.view, Arc::make_mut(&mut self.alloc))
    }

    /// Return the first element of the list. If we're the only owner of the list,
    /// drop the list and do not copy anything. If another list is alive, only
    /// clone the value that's being returned.
    pub fn head(self) -> Option<T> {
        let front = self.view.map_or(0, |(start, _end)| start);
        if Arc::strong_count(&self.alloc) == 1 {
            // safety: unwrap cannot fail because we ensured there was only one strong reference above
            let mut alloc = Arc::try_unwrap(self.alloc).map_err(|_| ()).unwrap();
            alloc.swap_remove_front(front)
        } else {
            self.alloc.get(front).cloned()
        }
    }

    /// Allocate iif the list being used by another value at the same time
    pub fn push_front(&mut self, element: T) {
        let (view, inner) = self.make_mut();
        if let Some((start, end)) = view {
            // if we were alone on the allocation and had a view of the inner allocation
            // we can keep the allocation and overwrite the start-1 element.
            // but we need to take care of the special case where the start is 0.
            if *start == 0 {
                inner.push_front(element);
                *end += 1;
            } else {
                *start -= 1;
                inner[*start] = element;
            }
        } else {
            inner.push_front(element);
        }
    }

    /// Allocate iif the list being used by another value at the same time
    pub fn push_back(&mut self, element: T) {
        let (view, inner) = self.make_mut();
        if let Some((_start, end)) = view {
            // if we were alone on the allocation and had a view of the inner allocation
            // we can keep the allocation and overwrite the end+1 element.
            // but we need to take care of the special case where the end is `alloc.len()`.
            if *end == inner.len() {
                inner.push_back(element);
                *end += 1;
            } else {
                *end += 1;
                inner[*end] = element;
            }
        } else {
            inner.push_back(element);
        }
    }
}

impl From<NumbatList<Value>> for Value {
    fn from(list: NumbatList<Value>) -> Self {
        Value::List(list)
    }
}

impl From<VecDeque<Value>> for Value {
    fn from(list: VecDeque<Value>) -> Self {
        Value::List(NumbatList {
            alloc: Arc::new(list),
            view: None,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic() {
        let mut list = NumbatList::<usize>::new();
        let alloc = Arc::as_ptr(&list.alloc);

        assert_eq!(list.len(), 0);
        list.push_front(1);
        assert_eq!(list.len(), 1);
        list.push_front(0);
        assert_eq!(list.len(), 2);
        list.push_back(2);
        assert_eq!(list.len(), 3);
        list.push_back(3);
        assert_eq!(list.len(), 4);
        assert_eq!(alloc, Arc::as_ptr(&list.alloc));

        insta::assert_debug_snapshot!(list, @r###"
        [
            0,
            1,
            2,
            3,
        ]
        "###);
        let iter: Vec<_> = list.iter().collect();
        insta::assert_debug_snapshot!(iter, @r###"
        [
            0,
            1,
            2,
            3,
        ]
        "###);

        list.advance_view().unwrap();
        assert_eq!(list.len(), 3);
        list.advance_view().unwrap();
        assert_eq!(list.len(), 2);
        assert_eq!(alloc, Arc::as_ptr(&list.alloc));

        insta::assert_debug_snapshot!(list, @r###"
        [
            2,
            3,
        ]
        "###);
        let iter: Vec<_> = list.iter().collect();
        insta::assert_debug_snapshot!(iter, @r###"
        [
            2,
            3,
        ]
        "###);

        list.advance_view().unwrap();
        list.advance_view().unwrap();
        assert!(list.is_empty());
        assert_eq!(alloc, Arc::as_ptr(&list.alloc));

        assert_eq!(list.advance_view(), Err(RuntimeError::EmptyList));
    }

    #[test]
    fn allocate() {
        let mut list1 = NumbatList::<usize>::new();

        list1.push_front(1);
        list1.push_back(0);

        let mut list2 = list1.clone();

        assert_eq!(Arc::as_ptr(&list1.alloc), Arc::as_ptr(&list2.alloc));

        list2.advance_view().unwrap();
        // Even after advancing the list2 the alloc can still be shared between both instance
        assert_eq!(Arc::as_ptr(&list1.alloc), Arc::as_ptr(&list2.alloc));

        // Pushing something new in the first list should re-allocate
        list1.push_front(2);
        assert_ne!(Arc::as_ptr(&list1.alloc), Arc::as_ptr(&list2.alloc));

        // Now that list2 is alone on its allocation it should be able
        // to push an element to the front without re-allocating anything
        let alloc = Arc::as_ptr(&list2.alloc);
        // Pushing something new in the first list should re-allocate
        list2.push_front(2);
        assert_eq!(alloc, Arc::as_ptr(&list2.alloc));
    }

    #[test]
    fn equality() {
        let mut list1 = NumbatList::<usize>::new();

        list1.push_front(1);
        list1.push_back(0);

        let mut list2 = list1.clone();

        assert_eq!(list1, list2);

        list2.advance_view().unwrap();
        assert_ne!(list1, list2);

        // Even if the list do not share the same allocation they should match
        list2.push_front(1);
        assert_eq!(list1, list2);
    }

    #[test]
    fn bug_1() {
        let mut list1 = NumbatList::<usize>::new();
        list1.push_front(1);
        let _list2 = list1.clone();
        list1.advance_view().unwrap();
        list1.push_front(2);
    }
}
