// use super::NodeId;
// use slotmap::SecondaryMap;
// use smallvec::SmallVec;

// #[derive(Default)]
// struct VecTargetList {
//     targets: SecondaryMap<NodeId, Vec<NodeId>>,
// }

// #[derive(Default)]
// struct SmallVecTargetList {
//     targets: SecondaryMap<NodeId, SmallVec<[NodeId; 1]>>,
// }

// #[derive(Default)]
// struct AdjacencyTargetList {
//     edges: Vec<(NodeId, usize)>,
// }

// #[derive(Default)]
// struct HalfAdjacencyEdge {
//     target: NodeId,
//     next: usize,
// }

// struct HalfAdjacencyList {
//     edges: Vec<HalfAdjacencyEdge>,
// }

// trait TargetList: Default {
//     type Iter<'a>: Iterator<Item = &'a NodeId>
//     where
//         Self: 'a;
//     fn add(&mut self, source: NodeId, target: NodeId);
//     fn retain(&mut self, source: NodeId, f: impl FnMut(NodeId) -> bool);
//     fn iter<'a>(&'a self, source: NodeId) -> Self::Iter<'a>;
// }

// impl TargetList for VecTargetList {
//     type Iter<'a> = std::slice::Iter<'a, NodeId>;
//     fn add(&mut self, source: NodeId, target: NodeId) {
//         self.targets[source].push(target);
//     }
//     fn retain(&mut self, source: NodeId, mut f: impl FnMut(NodeId) -> bool) {
//         self.targets[source].retain(move |n| f(*n));
//     }
//     fn iter<'a>(&'a self, source: NodeId) -> Self::Iter<'a> {
//         self.targets[source].iter()
//     }
// }

// impl TargetList for SmallVecTargetList {
//     type Iter<'a> =  std::slice::Iter<'a, NodeId>
//     where
//         Self: 'a;

//     fn add(&mut self, source: NodeId, target: NodeId) {
//         self.targets[source].push(target);
//     }

//     fn retain(&mut self, source: NodeId, mut f: impl FnMut(NodeId) -> bool) {
//         self.targets[source].retain(move |n| f(*n));
//     }

//     fn iter<'a>(&'a self, source: NodeId) -> Self::Iter<'a> {
//         self.targets[source].iter()
//     }
// }
