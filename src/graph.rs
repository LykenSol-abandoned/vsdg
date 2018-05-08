use std::cell::{Cell, Ref, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::io::{self, Write};
use std::mem;
use std::num::NonZeroU32;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct NodeId(NonZeroU32);

impl NodeId {
    fn index(self) -> usize {
        (self.0).get() as usize
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct InId {
    node: NodeId,
    index: u32,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct OutId {
    node: NodeId,
    index: u32,
}

#[derive(Clone, Default)]
struct InData {
    source: Cell<Option<OutId>>,
    prev_sink: Cell<Option<InId>>,
    next_sink: Cell<Option<InId>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct SinkList {
    first: InId,
    last: InId,
}

#[derive(Clone, Default)]
struct OutData {
    sinks: Cell<Option<SinkList>>,
}

struct NodeData {
    ins: Vec<InData>,
    outs: Vec<OutData>,
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
                ins: vec![],
                outs: vec![],
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
                ins: vec![InData::default(); (sig.val_ins + sig.st_ins) as usize],
                outs: vec![OutData::default(); (sig.val_outs + sig.st_outs) as usize],
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
            let id = NodeId(NonZeroU32::new(id as u32).unwrap());
            if self.node_data(id).ref_count.get() == 0 {
                continue;
            }
            let node = self.node_handle(id);
            writeln!(out, r#"    {} [label="{:?}"]"#, node.id.index(), node)?;
            let sig = node.kind().sig();
            for i in 0..sig.val_ins {
                for source in node.val_in(i as usize).maybe_source() {
                    writeln!(
                        out,
                        "    {} -> {} [color=blue]",
                        source.0.node.id.index(),
                        node.id.index()
                    )?;
                }
            }
            for i in 0..sig.st_ins {
                for source in node.st_in(i as usize).maybe_source() {
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

    fn in_data(&self, id: InId) -> Ref<InData> {
        Ref::map(self.node_data(id.node), |data| &data.ins[id.index as usize])
    }

    fn out_data(&self, id: OutId) -> Ref<OutData> {
        Ref::map(self.node_data(id.node), |data| {
            &data.outs[id.index as usize]
        })
    }

    fn release_node(&self, id: NodeId) {
        let ins = {
            let data = self.node_data(id);
            data.ref_count.set(data.ref_count.get() - 1);
            if data.ref_count.get() != 0 {
                return;
            }
            for out in &data.outs {
                assert_eq!(out.sinks.get(), None);
            }
            data.ins.len() as u32
        };

        for index in 0..ins {
            self.disconnect(InId { node: id, index });
        }

        // FIXME: do this without borrow_mut.
        let mut nodes = self.nodes.borrow_mut();
        let data = &mut nodes[id.index()];
        data.ins.clear();
        data.outs.clear();
        data.kind = 0;
    }

    fn connect(&self, sink: InId, source: OutId) {
        {
            let data = self.node_data(source.node);
            data.ref_count.set(data.ref_count.get() + 1);
        }

        let sink_data = self.in_data(sink);
        assert_eq!(sink_data.source.get(), None);
        assert_eq!(sink_data.prev_sink.get(), None);
        assert_eq!(sink_data.next_sink.get(), None);
        sink_data.source.set(Some(source));

        let source_data = self.out_data(source);
        let sinks = if let Some(mut sinks) = source_data.sinks.get() {
            self.in_data(sinks.last).next_sink.set(Some(sink));
            sink_data.prev_sink.set(Some(sinks.last));
            sinks.last = sink;
            sinks
        } else {
            SinkList {
                first: sink,
                last: sink,
            }
        };
        source_data.sinks.set(Some(sinks));
    }

    fn disconnect(&self, sink: InId) {
        let (source, prev_sink, next_sink) = {
            let sink_data = self.in_data(sink);
            match sink_data.source.get() {
                Some(source) => (source, sink_data.prev_sink.get(), sink_data.next_sink.get()),
                None => return,
            }
        };

        if let Some(prev_sink) = prev_sink {
            assert_eq!(
                self.in_data(prev_sink).next_sink.replace(next_sink),
                Some(sink)
            );
        }

        if let Some(next_sink) = next_sink {
            assert_eq!(
                self.in_data(next_sink).prev_sink.replace(prev_sink),
                Some(sink)
            );
        }

        {
            let source_data = self.out_data(source);
            let mut sinks = source_data.sinks.get().unwrap();
            let sinks = match (prev_sink, next_sink) {
                (None, None) => None,
                (Some(last), None) => {
                    assert_eq!(mem::replace(&mut sinks.last, last), sink);
                    Some(sinks)
                }
                (None, Some(first)) => {
                    assert_eq!(mem::replace(&mut sinks.first, first), sink);
                    Some(sinks)
                }
                (Some(_), Some(_)) => Some(sinks),
            };
            source_data.sinks.set(sinks);
        }

        self.release_node(source.node)
    }

    fn node_handle(&self, id: NodeId) -> Node<K> {
        let data = self.node_data(id);
        data.ref_count.set(data.ref_count.get() + 1);
        Node { graph: self, id }
    }

    fn in_handle(&self, id: InId) -> In<K> {
        In {
            node: self.node_handle(id.node),
            port: id.index,
        }
    }

    fn out_handle(&self, id: OutId) -> Out<K> {
        Out {
            node: self.node_handle(id.node),
            port: id.index,
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
        ValIn(In {
            node: self.clone(),
            port: i as u32,
        })
    }

    pub fn st_in(&self, i: usize) -> StIn<'g, K> {
        let sig = self.kind().sig();
        assert!(i < sig.st_ins as usize);
        StIn(In {
            node: self.clone(),
            port: sig.val_ins + i as u32,
        })
    }

    pub fn val_out(&self, i: usize) -> ValOut<'g, K> {
        let sig = self.kind().sig();
        assert!(i < sig.val_outs as usize);
        ValOut(Out {
            node: self.clone(),
            port: i as u32,
        })
    }

    pub fn st_out(&self, i: usize) -> StOut<'g, K> {
        let sig = self.kind().sig();
        assert!(i < sig.st_outs as usize);
        StOut(Out {
            node: self.clone(),
            port: sig.val_outs + i as u32,
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
        self.graph.release_node(self.id);
    }
}

#[derive(Clone)]
struct In<'g, K: 'g> {
    node: Node<'g, K>,
    port: u32,
}

impl<'g, K> In<'g, K> {
    fn id(&self) -> InId {
        InId {
            node: self.node.id,
            index: self.port,
        }
    }

    fn data(&self) -> Ref<'g, InData> {
        self.node.graph.in_data(self.id())
    }

    fn maybe_source(&self) -> Option<Out<'g, K>> {
        self.data()
            .source
            .get()
            .map(|source| self.node.graph.out_handle(source))
    }

    fn set_source(&self, source: Out<'g, K>) {
        self.node.graph.connect(self.id(), source.id());
    }
}

#[derive(Clone)]
struct Out<'g, K: 'g> {
    node: Node<'g, K>,
    port: u32,
}

impl<'g, K> Out<'g, K> {
    fn id(&self) -> OutId {
        OutId {
            node: self.node.id,
            index: self.port,
        }
    }

    fn data(&self) -> Ref<'g, OutData> {
        self.node.graph.out_data(self.id())
    }

    fn sinks(&self) -> Sinks<'g, K> {
        Sinks {
            first_and_last: self.data().sinks.get().map(|sinks| {
                (
                    self.node.graph.in_handle(sinks.first),
                    self.node.graph.in_handle(sinks.last),
                )
            }),
        }
    }
}

struct Sinks<'g, K: 'g> {
    first_and_last: Option<(In<'g, K>, In<'g, K>)>,
}

impl<'g, K> Iterator for Sinks<'g, K> {
    type Item = In<'g, K>;

    fn next(&mut self) -> Option<In<'g, K>> {
        match self.first_and_last.take() {
            Some((first, last)) => {
                if first.id() != last.id() {
                    let next = first.data().next_sink.get();
                    if let Some(next) = next {
                        self.first_and_last = Some((first.node.graph.in_handle(next), last));
                    }
                }
                Some(first)
            }
            None => None,
        }
    }
}

impl<'g, K> DoubleEndedIterator for Sinks<'g, K> {
    fn next_back(&mut self) -> Option<In<'g, K>> {
        match self.first_and_last.take() {
            Some((first, last)) => {
                if first.id() != last.id() {
                    let prev = last.data().prev_sink.get();
                    if let Some(prev) = prev {
                        self.first_and_last = Some((first, last.node.graph.in_handle(prev)));
                    }
                }
                Some(last)
            }
            None => None,
        }
    }
}

#[derive(Clone)]
pub struct ValIn<'g, K: 'g>(In<'g, K>);

impl<'g, K> ValIn<'g, K> {
    pub fn maybe_source(&self) -> Option<ValOut<'g, K>> {
        self.0.maybe_source().map(ValOut)
    }

    pub fn source(&self) -> ValOut<'g, K> {
        self.maybe_source().unwrap()
    }

    pub fn set_source(&self, source: ValOut<'g, K>) {
        self.0.set_source(source.0)
    }
}

#[derive(Clone)]
pub struct StIn<'g, K: 'g>(In<'g, K>);

impl<'g, K> StIn<'g, K> {
    pub fn maybe_source(&self) -> Option<StOut<'g, K>> {
        self.0.maybe_source().map(StOut)
    }

    pub fn source(&self) -> StOut<'g, K> {
        self.maybe_source().unwrap()
    }

    pub fn set_source(&self, source: StOut<'g, K>) {
        self.0.set_source(source.0)
    }
}

#[derive(Clone)]
pub struct ValOut<'g, K: 'g>(Out<'g, K>);

impl<'g, K> ValOut<'g, K> {
    pub fn sinks(&self) -> impl Iterator<Item = ValIn<'g, K>> {
        self.0.sinks().map(ValIn)
    }
}

#[derive(Clone)]
pub struct StOut<'g, K: 'g>(Out<'g, K>);

impl<'g, K> StOut<'g, K> {
    pub fn sinks(&self) -> impl Iterator<Item = StIn<'g, K>> {
        self.0.sinks().map(StIn)
    }
}
