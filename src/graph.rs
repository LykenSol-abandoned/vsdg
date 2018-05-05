use std::cell::{Cell, Ref, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::io::{self, Write};
use std::mem;
use std::num::NonZeroU32;

#[derive(Copy, Clone, PartialEq, Eq)]
struct NodeId(NonZeroU32);

impl NodeId {
    fn index(self) -> usize {
        (self.0).get() as usize
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct PortId {
    node: NodeId,
    port: u32,
}

impl PortId {
    fn port_index(self) -> usize {
        self.port as usize
    }
}

#[derive(Copy, Clone)]
struct PortList {
    first: PortId,
    last: PortId,
}

#[derive(Clone, Default)]
struct PortData {
    edges: Cell<Option<PortList>>,
    prev_edge: Cell<Option<PortId>>,
    next_edge: Cell<Option<PortId>>,
}

struct NodeData {
    ports: Vec<PortData>,
    kind: u32,
    ref_count: Cell<u32>,
    input_count: u32,
}

pub struct Graph<K> {
    nodes: RefCell<Vec<NodeData>>,
    kinds: RefCell<Vec<K>>,
    kind_index: RefCell<HashMap<K, u32>>,
}

pub trait NodeKind: Clone + Eq + Hash + fmt::Debug {
    fn num_inputs(&self) -> usize;
    fn num_outputs(&self) -> usize;
}

impl<K> Graph<K> {
    pub fn new() -> Graph<K>
    where
        K: NodeKind,
    {
        Graph {
            nodes: RefCell::new(vec![NodeData {
                ports: vec![],
                kind: 0,
                ref_count: Cell::new(0),
                input_count: 0,
            }]),
            kinds: RefCell::new(vec![]),
            kind_index: RefCell::new(HashMap::new()),
        }
    }

    pub fn add(&self, kind: K) -> Node<K>
    where
        K: NodeKind,
    {
        let num_inputs = kind.num_inputs();
        let num_outputs = kind.num_outputs();
        let kind = *self
            .kind_index
            .borrow_mut()
            .entry(kind.clone())
            .or_insert_with(|| {
                let mut kinds = self.kinds.borrow_mut();
                kinds.push(kind);
                kinds.len() as u32 - 1
            });
        let id = {
            let mut nodes = self.nodes.borrow_mut();
            nodes.push(NodeData {
                ports: vec![PortData::default(); num_inputs + num_outputs],
                kind: kind as u32,
                ref_count: Cell::new(0),
                input_count: num_inputs as u32,
            });
            NodeId(NonZeroU32::new(nodes.len() as u32 - 1).unwrap())
        };
        self.node_handle(id)
    }

    pub fn print(&self, out: &mut Write) -> io::Result<()>
    where
        K: NodeKind,
    {
        writeln!(out, "digraph vsdg {{")?;
        for id in 1..self.nodes.borrow().len() {
            let node = self.node_handle(NodeId(NonZero::new(id as u32).unwrap()));
            writeln!(out, r#"    {} [label="{:?}"]"#, node.id.index(), node)?;
            for i in 0..node.num_inputs() {
                for source in node.input(i).sources() {
                    writeln!(
                        out,
                        "    {} -> {}",
                        source.0.node.id.index(),
                        node.id.index()
                    )?;
                }
            }
        }
        writeln!(out, "}}")
    }

    fn node_data(&self, id: NodeId) -> Ref<NodeData> {
        Ref::map(self.nodes.borrow(), |nodes| &nodes[id.index()])
    }

    fn port_data(&self, id: PortId) -> Ref<PortData> {
        Ref::map(self.node_data(id.node), |data| &data.ports[id.port_index()])
    }

    fn maybe_release_node(&self, id: NodeId) {
        // HACK keep all nodes alive.
        if self.node_data(id).ref_count.get() == 0 && false {
            // FIXME: do this without borrow_mut.
            let mut nodes = self.nodes.borrow_mut();
            let data = &mut nodes[id.index()];
            data.ports.clear();
            data.kind = 0;
            data.input_count = 0;
        }
    }

    fn node_handle(&self, id: NodeId) -> Node<K> {
        let data = self.node_data(id);
        data.ref_count.set(data.ref_count.get() + 1);
        Node { graph: self, id }
    }

    fn port_handle(&self, id: PortId) -> Port<K> {
        Port {
            node: self.node_handle(id.node),
            port: id.port,
        }
    }
}

pub struct Node<'g, K: 'g> {
    graph: &'g Graph<K>,
    id: NodeId,
}

impl<'g, K> Node<'g, K> {
    fn data(&self) -> Ref<'g, NodeData> {
        self.graph.node_data(self.id)
    }

    pub fn num_inputs(&self) -> usize {
        self.data().input_count as usize
    }

    pub fn num_outputs(&self) -> usize {
        let data = self.data();
        data.ports.len() - data.input_count as usize
    }

    pub fn input(&self, i: usize) -> Input<'g, K> {
        assert!(i < self.num_inputs());
        Input(Port {
            node: self.clone(),
            port: i as u32,
        })
    }

    pub fn output(&self, i: usize) -> Output<'g, K> {
        assert!(i < self.num_outputs());
        Output(Port {
            node: self.clone(),
            port: (self.num_inputs() + i) as u32,
        })
    }
}

impl<'g, K: fmt::Debug> fmt::Debug for Node<'g, K> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}",
            self.graph.kinds.borrow()[self.data().kind as usize]
        )
    }
}

impl<'g, K> Clone for Node<'g, K> {
    fn clone(&self) -> Self {
        self.graph.node_handle(self.id)
    }
}

impl<'g, K> Drop for Node<'g, K> {
    fn drop(&mut self) {
        {
            let data = self.data();
            data.ref_count.set(data.ref_count.get() - 1);
        }
        self.graph.maybe_release_node(self.id);
    }
}

#[derive(Clone)]
struct Port<'g, K: 'g> {
    node: Node<'g, K>,
    port: u32,
}

impl<'g, K> Port<'g, K> {
    fn id(&self) -> PortId {
        PortId {
            node: self.node.id,
            port: self.port,
        }
    }

    fn data(&self) -> Ref<'g, PortData> {
        self.node.graph.port_data(self.id())
    }

    fn edges(&self) -> Edges<'g, K> {
        let edges = self.data().edges.get();
        Edges {
            first_and_last: edges.map(|edges| {
                (
                    self.node.graph.port_handle(edges.first),
                    self.node.graph.port_handle(edges.last),
                )
            }),
        }
    }
}

struct Edges<'g, K: 'g> {
    first_and_last: Option<(Port<'g, K>, Port<'g, K>)>,
}

impl<'g, K> Iterator for Edges<'g, K> {
    type Item = Port<'g, K>;

    fn next(&mut self) -> Option<Port<'g, K>> {
        match self.first_and_last.take() {
            Some((first, last)) => {
                if first.id() != last.id() {
                    let next = first.data().next_edge.get();
                    if let Some(next) = next {
                        self.first_and_last = Some((first.node.graph.port_handle(next), last));
                    }
                }
                Some(first)
            }
            None => None,
        }
    }
}

impl<'g, K> DoubleEndedIterator for Edges<'g, K> {
    fn next_back(&mut self) -> Option<Port<'g, K>> {
        match self.first_and_last.take() {
            Some((first, last)) => {
                if first.id() != last.id() {
                    let prev = last.data().prev_edge.get();
                    if let Some(prev) = prev {
                        self.first_and_last = Some((first, last.node.graph.port_handle(prev)));
                    }
                }
                Some(last)
            }
            None => None,
        }
    }
}

#[derive(Clone)]
pub struct Input<'g, K: 'g>(Port<'g, K>);

impl<'g, K> Input<'g, K> {
    pub fn sources(&self) -> impl Iterator<Item = Output<'g, K>> {
        self.0.edges().map(Output)
    }

    pub fn maybe_source(&self) -> Option<Output<'g, K>> {
        let mut sources = self.sources();
        let source = sources.next();
        assert!(sources.next().is_none());
        source
    }

    pub fn source(&self) -> Output<'g, K> {
        self.maybe_source().unwrap()
    }

    pub fn set_source(&self, source: Output<'g, K>) {
        let source_data = source.0.data();
        let sink_data = self.0.data();
        assert!(sink_data.edges.get().is_none());
        sink_data.edges.set(Some(PortList {
            first: source.0.id(),
            last: source.0.id(),
        }));
        if let Some(mut edges) = source_data.edges.get() {
            source
                .0
                .node
                .graph
                .port_data(edges.last)
                .next_edge
                .set(Some(self.0.id()));
            sink_data.prev_edge.set(Some(edges.last));
            edges.last = self.0.id();
            source_data.edges.set(Some(edges));
        } else {
            source_data.edges.set(Some(PortList {
                first: self.0.id(),
                last: self.0.id(),
            }));
        }
        // HACK keep the source alive.
        mem::forget(source);
    }
}

#[derive(Clone)]
pub struct Output<'g, K: 'g>(Port<'g, K>);

impl<'g, K> Output<'g, K> {
    pub fn sinks(&self) -> impl Iterator<Item = Input<'g, K>> {
        self.0.edges().map(Input)
    }
}
