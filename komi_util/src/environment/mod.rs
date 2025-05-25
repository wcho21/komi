use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment<T> {
    outer: Option<Box<Environment<T>>>,
    table: HashMap<String, T>,
}

impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        Self { outer: None, table: HashMap::new() }
    }

    pub fn from_outer(outer: Environment<T>) -> Self {
        Self { outer: Some(Box::new(outer)), table: HashMap::new() }
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        // Return the value if found.
        if let Some(value) = self.table.get(name) {
            return Some(value);
        }

        // Since not found in the current environment, try in the outer environment.
        match &self.outer {
            Some(x) => x.get(name),
            None => None,
        }
    }

    pub fn set(&mut self, name: &str, value: &T) -> () {
        self.table.insert(name.to_string(), value.clone());
    }
}

// For unit tests, see the `Environment` in the `komi_evaluator` crate.
