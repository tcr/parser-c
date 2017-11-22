// Original file: "NameSpaceMap.hs"
// File auto-generated using Corollary.

use std::collections::BTreeMap;

#[derive(Clone, Debug)]
pub struct NameSpaceMap<K, V> {
    gs: BTreeMap<K, V>,
    ls: Vec<Vec<(K, V)>>,
}

impl<K: Ord, V: Clone> NameSpaceMap<K, V> {

    pub fn new() -> NameSpaceMap<K, V> {
        NameSpaceMap { gs: BTreeMap::new(), ls: vec![] }
    }

    pub fn globalNames(&self) -> &BTreeMap<K, V> {
        &self.gs
    }

    pub fn hasLocalNames(&self) -> bool {
        !self.ls.is_empty()
    }

    pub fn localNames(&self) -> &[Vec<(K, V)>] {
        &self.ls
    }

    pub fn defGlobal(&mut self, ident: K, def: V) -> Option<V> {
        let prev = self.gs.get(&ident).cloned();
        self.gs.insert(ident, def);
        prev
    }

    pub fn enterNewScope(&mut self) {
        self.ls.push(vec![]);
    }

    pub fn leaveScope(&mut self) -> Vec<(K, V)> {
        match self.ls.pop() {
            None => panic!("NsMaps.leaveScope: No local scope!"),
            Some(v) => v
        }
    }

    pub fn defLocal(&mut self, ident: K, def: V) -> Option<V> {
        if self.ls.len() == 0 { // XXX: would match last_mut() with nonlexical borrows
            return self.defGlobal(ident, def);
        }
        let scope = self.ls.last_mut().unwrap();
        let prev = None; // TODO lookup ident in Vec<k, a>
        scope.push((ident, def));
        prev
    }

    pub fn lookupName(&self, ident: &K) -> Option<&V> {
        for _scope in self.ls.iter().rev() {
            if /* TODO lookup */ false {
                return None; // Some(def)
            }
        }
        self.lookupGlobal(ident)
    }

    pub fn lookupGlobal(&self, ident: &K) -> Option<&V> {
        self.gs.get(ident)
    }

    pub fn lookupInnermostScope(&self, ident: &K) -> Option<&V> {
        if let Some(_scope) = self.ls.last() {
            if /* TODO lookup */ false {
                return None; // Some(def)
            }
        }
        self.lookupGlobal(ident)
    }

    pub fn toList(self) -> Vec<(K, V)> {
        let NameSpaceMap { gs, ls } = self;
        ls.into_iter().flat_map(|v| v).chain(gs.into_iter()).collect()
    }
}

pub fn mergeNameSpace<K: Ord + PartialEq, V>(map1: NameSpaceMap<K, V>, map2: NameSpaceMap<K, V>)
                                             -> NameSpaceMap<K, V> {
    let NameSpaceMap { gs: mut global1, ls: mut local1 } = map1;
    let NameSpaceMap { gs: mut global2, ls: mut local2 } = map2;
    global1.append(&mut global2);

    let mut localUnion = Vec::new();
    // TODO use zip_longest from Itertools instead
    loop {
        match (local1.pop(), local2.pop()) {
            (Some(mut l1), Some(mut l2)) => {
                // TODO naive algorithm
                l2.retain(|l2el| !l1.iter().any(|l1el| l1el.0 == l2el.0));
                l1.append(&mut l2);
                localUnion.push(l1);
            }
            (Some(l), _) | (_, Some(l)) => {
                localUnion.push(l);
            }
            _ => break,
        }
    }

    localUnion.reverse();
    NameSpaceMap { gs: global1, ls: localUnion }
}
