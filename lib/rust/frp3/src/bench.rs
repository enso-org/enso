trait EventConsumer {
    fn on_event(&self, event: usize) -> usize;
}

struct Node1;
struct Node2;
struct Node3;

impl EventConsumer for Node1 {
    #[inline(always)]
    fn on_event(&self, event: usize) -> usize {
        event + 1
    }
}

impl EventConsumer for Node2 {
    fn on_event(&self, event: usize) -> usize {
        event + 2
    }
}

impl EventConsumer for Node3 {
    fn on_event(&self, _event: usize) -> usize {
        0
    }
}


enum AnyNode {
    Node1,
    Node2,
    Node3,
}

impl AnyNode {
    fn on_event(&self, event: usize) -> usize {
        match self {
            AnyNode::Node1 => event + 1,
            AnyNode::Node2 => event + 2,
            AnyNode::Node3 => 0,
        }
    }
}


#[cfg(test)]
mod benches {
    use super::*;
    extern crate test;
    use test::Bencher;

    const REPS: usize = 100_000;

    #[bench]
    fn bench_dyn_trait(bencher: &mut Bencher) {
        let mut nodes: Vec<Box<dyn EventConsumer>> = vec![];
        for _ in 0..REPS {
            nodes.push(Box::new(Node1));
            nodes.push(Box::new(Node2));
            nodes.push(Box::new(Node3));
        }
        bencher.iter(move || {
            let mut event = 0;
            for node in &nodes {
                event = node.on_event(event);
            }
        });
    }

    #[bench]
    fn bench_enum_dispatch(bencher: &mut Bencher) {
        let mut nodes: Vec<AnyNode> = vec![];
        for _ in 0..REPS {
            nodes.push(AnyNode::Node1);
            nodes.push(AnyNode::Node2);
            nodes.push(AnyNode::Node3);
        }
        bencher.iter(move || {
            let mut event = 0;
            for node in &nodes {
                event = node.on_event(event);
            }
        });
    }
}
