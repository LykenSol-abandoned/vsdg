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
}

pub struct Graph<K> {
    nodes: RefCell<Vec<NodeData>>,
    kinds: RefCell<Vec<K>>,
    kind_index: RefCell<HashMap<K, u32>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Sig {
    pub val_ins: u32,
    pub st_ins: u32,
    pub val_outs: u32,
    pub st_outs: u32,
}

pub trait NodeKind: Clone + Eq + Hash + fmt::Debug {
    fn sig(&self) -> Sig;
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
            }]),
            kinds: RefCell::new(vec![]),
            kind_index: RefCell::new(HashMap::new()),
        }
    }

    pub fn add(&self, kind: K) -> Node<K>
    where
        K: NodeKind,
    {
        let sig = kind.sig();
        let num_ports = sig.val_ins + sig.st_ins + sig.val_outs + sig.st_outs;
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
                ports: vec![PortData::default(); num_ports as usize],
                kind: kind as u32,
                ref_count: Cell::new(0),
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
            let node = self.node_handle(NodeId(NonZeroU32::new(id as u32).unwrap()));
            writeln!(out, r#"    {} [label="{:?}"]"#, node.id.index(), node)?;
            let sig = node.kind().sig();
            for i in 0..sig.val_ins {
                for source in node.val_in(i as usize).sources() {
                    writeln!(
                        out,
                        "    {} -> {} [color=blue]",
                        source.0.node.id.index(),
                        node.id.index()
                    )?;
                }
            }
            for i in 0..sig.st_ins {
                for source in node.st_in(i as usize).sources() {
                    writeln!(
                        out,
                        "    {} -> {} [style=dashed, color=red]",
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

    pub fn kind(&self) -> Ref<'g, K> {
        Ref::map(self.graph.kinds.borrow(), |kinds| {
            &kinds[self.data().kind as usize]
        })
    }
}

impl<'g, K: NodeKind> Node<'g, K> {
    pub fn val_in(&self, i: usize) -> ValIn<'g, K> {
        let sig = self.kind().sig();
        assert!(i < sig.val_ins as usize);
        ValIn(Port {
            node: self.clone(),
            port: i as u32,
        })
    }

    pub fn st_in(&self, i: usize) -> StIn<'g, K> {
        let sig = self.kind().sig();
        assert!(i < sig.st_ins as usize);
        StIn(Port {
            node: self.clone(),
            port: sig.val_ins + i as u32,
        })
    }

    pub fn val_out(&self, i: usize) -> ValOut<'g, K> {
        let sig = self.kind().sig();
        assert!(i < sig.val_outs as usize);
        ValOut(Port {
            node: self.clone(),
            port: sig.val_ins + sig.st_ins + i as u32,
        })
    }

    pub fn st_out(&self, i: usize) -> StOut<'g, K> {
        let sig = self.kind().sig();
        assert!(i < sig.st_outs as usize);
        StOut(Port {
            node: self.clone(),
            port: sig.val_ins + sig.st_ins + sig.val_outs + i as u32,
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
pub struct ValIn<'g, K: 'g>(Port<'g, K>);

impl<'g, K> ValIn<'g, K> {
    pub fn sources(&self) -> impl Iterator<Item = ValOut<'g, K>> {
        self.0.edges().map(ValOut)
    }

    pub fn maybe_source(&self) -> Option<ValOut<'g, K>> {
        let mut sources = self.sources();
        let source = sources.next();
        assert!(sources.next().is_none());
        source
    }

    pub fn source(&self) -> ValOut<'g, K> {
        self.maybe_source().unwrap()
    }

    pub fn set_source(&self, source: ValOut<'g, K>) {
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
pub struct StIn<'g, K: 'g>(Port<'g, K>);

impl<'g, K> StIn<'g, K> {
    pub fn sources(&self) -> impl Iterator<Item = StOut<'g, K>> {
        self.0.edges().map(StOut)
    }

    pub fn maybe_source(&self) -> Option<StOut<'g, K>> {
        let mut sources = self.sources();
        let source = sources.next();
        assert!(sources.next().is_none());
        source
    }

    pub fn source(&self) -> StOut<'g, K> {
        self.maybe_source().unwrap()
    }

    pub fn set_source(&self, source: StOut<'g, K>) {
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
pub struct ValOut<'g, K: 'g>(Port<'g, K>);

impl<'g, K> ValOut<'g, K> {
    pub fn sinks(&self) -> impl Iterator<Item = ValIn<'g, K>> {
        self.0.edges().map(ValIn)
    }
}

#[derive(Clone)]
pub struct StOut<'g, K: 'g>(Port<'g, K>);

impl<'g, K> StOut<'g, K> {
    pub fn sinks(&self) -> impl Iterator<Item = StIn<'g, K>> {
        self.0.edges().map(StIn)
    }
}
