use std::cell::{Cell, Ref, RefCell};
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

struct NodeData<C> {
    ins: Vec<InData>,
    outs: Vec<OutData>,
    kind: NodeKind<C>,
    ref_count: Cell<u32>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Sig {
    pub val_ins: u32,
    pub st_ins: u32,
    pub val_outs: u32,
    pub st_outs: u32,
}

pub trait Callee: Clone + Eq + Hash + fmt::Debug {
    fn sig(&self) -> Sig;
}

#[derive(Debug)]
pub enum NodeKind<C> {
    Call(C),
}

impl<C: Callee> NodeKind<C> {
    pub fn sig(&self) -> Sig {
        match self {
            NodeKind::Call(c) => c.sig(),
        }
    }
}

pub struct Graph<C> {
    nodes: RefCell<Vec<Option<NodeData<C>>>>,
}

impl<C> Graph<C> {
    pub fn new() -> Graph<C>
    where
        C: Callee,
    {
        Graph {
            nodes: RefCell::new(vec![None]),
        }
    }

    pub fn create(&self, kind: NodeKind<C>) -> Node<C>
    where
        C: Callee,
    {
        let sig = kind.sig();
        let id = {
            let mut nodes = self.nodes.borrow_mut();
            nodes.push(Some(NodeData {
                ins: vec![InData::default(); (sig.val_ins + sig.st_ins) as usize],
                outs: vec![OutData::default(); (sig.val_outs + sig.st_outs) as usize],
                kind,
                ref_count: Cell::new(0),
            }));
            NodeId(NonZeroU32::new(nodes.len() as u32 - 1).unwrap())
        };
        self.node_handle(id)
    }

    pub fn call(&self, callee: C) -> Node<C>
    where
        C: Callee,
    {
        self.create(NodeKind::Call(callee))
    }

    pub fn print(&self, out: &mut Write) -> io::Result<()>
    where
        C: Callee,
    {
        writeln!(out, "digraph vsdg {{")?;
        for id in 1..self.nodes.borrow().len() {
            let id = NodeId(NonZeroU32::new(id as u32).unwrap());
            if self.node_data(id).ref_count.get() == 0 {
                continue;
            }
            let node = self.node_handle(id);
            match *node.kind() {
                NodeKind::Call(ref callee) => {
                    writeln!(out, r#"    {} [label="{:?}"]"#, node.id.index(), callee)?;
                }
            }
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

    fn node_data(&self, id: NodeId) -> Ref<NodeData<C>> {
        Ref::map(self.nodes.borrow(), |nodes| {
            nodes[id.index()].as_ref().unwrap()
        })
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
        self.nodes.borrow_mut()[id.index()] = None;
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

    fn node_handle(&self, id: NodeId) -> Node<C> {
        let data = self.node_data(id);
        data.ref_count.set(data.ref_count.get() + 1);
        Node { graph: self, id }
    }

    fn in_handle(&self, id: InId) -> In<C> {
        In {
            node: self.node_handle(id.node),
            port: id.index,
        }
    }

    fn out_handle(&self, id: OutId) -> Out<C> {
        Out {
            node: self.node_handle(id.node),
            port: id.index,
        }
    }
}

pub struct Node<'g, C: 'g> {
    graph: &'g Graph<C>,
    id: NodeId,
}

impl<'g, C> Node<'g, C> {
    fn data(&self) -> Ref<'g, NodeData<C>> {
        self.graph.node_data(self.id)
    }

    pub fn kind(&self) -> Ref<'g, NodeKind<C>> {
        Ref::map(self.data(), |data| &data.kind)
    }
}

impl<'g, C: Callee> Node<'g, C> {
    pub fn val_in(&self, i: usize) -> ValIn<'g, C> {
        let sig = self.kind().sig();
        assert!(i < sig.val_ins as usize);
        ValIn(In {
            node: self.clone(),
            port: i as u32,
        })
    }

    pub fn st_in(&self, i: usize) -> StIn<'g, C> {
        let sig = self.kind().sig();
        assert!(i < sig.st_ins as usize);
        StIn(In {
            node: self.clone(),
            port: sig.val_ins + i as u32,
        })
    }

    pub fn val_out(&self, i: usize) -> ValOut<'g, C> {
        let sig = self.kind().sig();
        assert!(i < sig.val_outs as usize);
        ValOut(Out {
            node: self.clone(),
            port: i as u32,
        })
    }

    pub fn st_out(&self, i: usize) -> StOut<'g, C> {
        let sig = self.kind().sig();
        assert!(i < sig.st_outs as usize);
        StOut(Out {
            node: self.clone(),
            port: sig.val_outs + i as u32,
        })
    }
}

impl<'g, C: fmt::Debug> fmt::Debug for Node<'g, C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind())
    }
}

impl<'g, C> Clone for Node<'g, C> {
    fn clone(&self) -> Self {
        self.graph.node_handle(self.id)
    }
}

impl<'g, C> Drop for Node<'g, C> {
    fn drop(&mut self) {
        self.graph.release_node(self.id);
    }
}

#[derive(Clone)]
struct In<'g, C: 'g> {
    node: Node<'g, C>,
    port: u32,
}

impl<'g, C> In<'g, C> {
    fn id(&self) -> InId {
        InId {
            node: self.node.id,
            index: self.port,
        }
    }

    fn data(&self) -> Ref<'g, InData> {
        self.node.graph.in_data(self.id())
    }

    fn maybe_source(&self) -> Option<Out<'g, C>> {
        self.data()
            .source
            .get()
            .map(|source| self.node.graph.out_handle(source))
    }

    fn set_source(&self, source: Out<'g, C>) {
        self.node.graph.connect(self.id(), source.id());
    }
}

#[derive(Clone)]
struct Out<'g, C: 'g> {
    node: Node<'g, C>,
    port: u32,
}

impl<'g, C> Out<'g, C> {
    fn id(&self) -> OutId {
        OutId {
            node: self.node.id,
            index: self.port,
        }
    }

    fn data(&self) -> Ref<'g, OutData> {
        self.node.graph.out_data(self.id())
    }

    fn sinks(&self) -> Sinks<'g, C> {
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

struct Sinks<'g, C: 'g> {
    first_and_last: Option<(In<'g, C>, In<'g, C>)>,
}

impl<'g, C> Iterator for Sinks<'g, C> {
    type Item = In<'g, C>;

    fn next(&mut self) -> Option<In<'g, C>> {
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

impl<'g, C> DoubleEndedIterator for Sinks<'g, C> {
    fn next_back(&mut self) -> Option<In<'g, C>> {
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
pub struct ValIn<'g, C: 'g>(In<'g, C>);

impl<'g, C> ValIn<'g, C> {
    pub fn maybe_source(&self) -> Option<ValOut<'g, C>> {
        self.0.maybe_source().map(ValOut)
    }

    pub fn source(&self) -> ValOut<'g, C> {
        self.maybe_source().unwrap()
    }

    pub fn set_source(&self, source: ValOut<'g, C>) {
        self.0.set_source(source.0)
    }
}

#[derive(Clone)]
pub struct StIn<'g, C: 'g>(In<'g, C>);

impl<'g, C> StIn<'g, C> {
    pub fn maybe_source(&self) -> Option<StOut<'g, C>> {
        self.0.maybe_source().map(StOut)
    }

    pub fn source(&self) -> StOut<'g, C> {
        self.maybe_source().unwrap()
    }

    pub fn set_source(&self, source: StOut<'g, C>) {
        self.0.set_source(source.0)
    }
}

#[derive(Clone)]
pub struct ValOut<'g, C: 'g>(Out<'g, C>);

impl<'g, C> ValOut<'g, C> {
    pub fn sinks(&self) -> impl Iterator<Item = ValIn<'g, C>> {
        self.0.sinks().map(ValIn)
    }
}

#[derive(Clone)]
pub struct StOut<'g, C: 'g>(Out<'g, C>);

impl<'g, C> StOut<'g, C> {
    pub fn sinks(&self) -> impl Iterator<Item = StIn<'g, C>> {
        self.0.sinks().map(StIn)
    }
}
