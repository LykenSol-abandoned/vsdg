use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::instructions::{InstructionData, InstructionFormat};
use cranelift_codegen::ir::{self, FuncRef, Heap, MemFlags, Opcode, TrapCode, Value, ValueDef};
use graph::{self, NodeKind, Sig};
use std::collections::HashMap;
use std::fmt;
use std::mem;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Op {
    Simple { opcode: Opcode },
    Const { opcode: Opcode, imm: u64 },
    IntCmp { opcode: Opcode, cond: IntCC },
    FloatCmp { opcode: Opcode, cond: FloatCC },
    FuncAddr { opcode: Opcode, func_ref: FuncRef },
    HeapAddr { opcode: Opcode, heap: Heap },
    Memory { opcode: Opcode, flags: MemFlags },
    Trap { opcode: Opcode, code: TrapCode },
    IndirectCall { sig: Sig },
    ParamVal,
    ParamSt,
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Simple { opcode } => write!(f, "{:?}", opcode),
            Op::Const { opcode, imm } => write!(f, "{:?}({:?})", opcode, imm),
            Op::IntCmp { opcode, cond } => write!(f, "{:?}({:?})", opcode, cond),
            Op::FloatCmp { opcode, cond } => write!(f, "{:?}({:?})", opcode, cond),
            Op::FuncAddr { opcode, func_ref } => write!(f, "{:?}({:?})", opcode, func_ref),
            Op::HeapAddr { opcode, heap } => write!(f, "{:?}({:?})", opcode, heap),
            Op::Memory { opcode, flags } => write!(f, "{:?}({:?})", opcode, flags),
            Op::Trap { opcode, code } => write!(f, "{:?}({:?})", opcode, code),
            Op::IndirectCall { sig } => write!(f, "IndirectCall({:?})", sig),
            Op::ParamVal => write!(f, "ParamVal"),
            Op::ParamSt => write!(f, "ParamSt"),
        }
    }
}

impl NodeKind for Op {
    fn sig(&self) -> Sig {
        use self::Op::*;
        let (val_ins, st_ins, val_outs, st_outs) = match *self {
            Simple { opcode } => (
                match opcode.format() {
                    InstructionFormat::NullAry => 0,
                    InstructionFormat::Unary => 1,
                    InstructionFormat::Binary | InstructionFormat::BinaryImm => 2,
                    InstructionFormat::Ternary => 3,
                    _ => unreachable!(),
                },
                0,
                1,
                0,
            ),
            Const { .. } | FuncAddr { .. } | ParamVal => (0, 0, 1, 0),
            HeapAddr { .. } => (2, 0, 1, 0),
            IntCmp { .. } | FloatCmp { .. } => (2, 0, 1, 0),

            ParamSt => (0, 0, 0, 1),
            Memory { opcode, .. } => (
                1 + opcode.can_store() as u32,
                1,
                opcode.can_load() as u32,
                1,
            ),
            Trap { .. } => (0, 1, 0, 1),

            IndirectCall { mut sig } => {
                sig.val_ins += 1;
                return sig;
            }
        };
        Sig {
            val_ins,
            st_ins,
            val_outs,
            st_outs,
        }
    }
}

pub type Graph = graph::Graph<Op>;
pub type Node<'g> = graph::Node<'g, Op>;
pub type ValIn<'g> = graph::ValIn<'g, Op>;
pub type StIn<'g> = graph::StIn<'g, Op>;
pub type ValOut<'g> = graph::ValOut<'g, Op>;
pub type StOut<'g> = graph::StOut<'g, Op>;

pub struct ConstructCx<'g> {
    func: &'g ir::Function,
    graph: &'g Graph,
    instructions: HashMap<ir::Inst, Node<'g>>,
    ebb_params: HashMap<ir::Ebb, (Vec<ValOut<'g>>, StOut<'g>)>,
    cur_st: Option<StOut<'g>>,
}

impl<'g> ConstructCx<'g> {
    fn value(&self, value: Value) -> ValOut<'g> {
        match self.func.dfg.value_def(value) {
            ValueDef::Result(inst, num) => match self.instructions.get(&inst) {
                Some(node) => node.val_out(num),
                None => panic!(
                    "Instruction {:?} (producing value {:?}) missing!\n{:?}",
                    inst, value, self.func
                ),
            },
            ValueDef::Param(ebb, num) => self.ebb_params[&ebb].0[num].clone(),
        }
    }

    fn const_u64(&self, opcode: Opcode, imm: u64) -> Node<'g> {
        self.graph.add(Op::Const { opcode, imm })
    }

    fn const_i64(&self, opcode: Opcode, imm: i64) -> Node<'g> {
        self.const_u64(opcode, imm as u64)
    }

    fn const_i32(&self, opcode: Opcode, imm: i32) -> Node<'g> {
        self.const_i64(opcode, imm as i64)
    }

    fn unary(&self, op: Op, x: ValOut<'g>) -> Node<'g> {
        let node = self.graph.add(op);
        node.val_in(0).set_source(x);
        node
    }

    fn binary(&self, op: Op, a: ValOut<'g>, b: ValOut<'g>) -> Node<'g> {
        let node = self.graph.add(op);
        node.val_in(0).set_source(a);
        node.val_in(1).set_source(b);
        node
    }

    fn ternary(&self, op: Op, a: ValOut<'g>, b: ValOut<'g>, c: ValOut<'g>) -> Node<'g> {
        let node = self.graph.add(op);
        node.val_in(0).set_source(a);
        node.val_in(1).set_source(b);
        node.val_in(2).set_source(c);
        node
    }

    fn iadd_offset32(&self, val: ValOut<'g>, offset: ir::immediates::Offset32) -> ValOut<'g> {
        let offset = offset.into();
        if offset == 0 {
            return val;
        }
        self.binary(
            Op::Simple {
                opcode: Opcode::Iadd,
            },
            val,
            self.const_i32(Opcode::Iconst, offset).val_out(0),
        ).val_out(0)
    }

    fn convert_inst(&mut self, inst: ir::Inst) {
        let node = match self.func.dfg[inst] {
            InstructionData::NullAry { opcode } => self.graph.add(Op::Simple { opcode }),

            InstructionData::Unary { opcode, arg } => {
                self.unary(Op::Simple { opcode }, self.value(arg))
            }

            InstructionData::UnaryImm { opcode, imm } => self.const_i64(opcode, imm.into()),

            // | InstructionData::UnaryGlobalVar { opcode, .. }
            // | InstructionData::UnaryBool { opcode, imm }
            // | InstructionData::UnaryIeee32 { opcode, imm }
            InstructionData::UnaryIeee64 { opcode, imm } => self.const_u64(opcode, imm.bits()),

            InstructionData::Binary {
                opcode,
                args: [a, b],
            } => self.binary(Op::Simple { opcode }, self.value(a), self.value(b)),

            InstructionData::BinaryImm { opcode, arg, imm } => self.binary(
                Op::Simple { opcode },
                self.value(arg),
                self.const_i64(Opcode::Iconst, imm.into()).val_out(0),
            ),

            InstructionData::Ternary {
                opcode,
                args: [a, b, c],
            } => self.ternary(
                Op::Simple { opcode },
                self.value(a),
                self.value(b),
                self.value(c),
            ),

            InstructionData::IntCompare {
                opcode,
                cond,
                args: [a, b],
            } => self.binary(Op::IntCmp { opcode, cond }, self.value(a), self.value(b)),

            InstructionData::IntCompareImm {
                opcode,
                cond,
                arg,
                imm,
            } => self.binary(
                Op::IntCmp { opcode, cond },
                self.value(arg),
                self.const_i64(Opcode::Iconst, imm.into()).val_out(0),
            ),

            InstructionData::IntCond { opcode, cond, arg } => {
                self.unary(Op::IntCmp { opcode, cond }, self.value(arg))
            }

            InstructionData::FloatCompare {
                opcode,
                cond,
                args: [a, b],
            } => self.binary(Op::FloatCmp { opcode, cond }, self.value(a), self.value(b)),

            InstructionData::FloatCond { opcode, cond, arg } => {
                self.unary(Op::FloatCmp { opcode, cond }, self.value(arg))
            }

            //InstructionData::IndirectCall
            InstructionData::Call {
                opcode: Opcode::Call,
                ref args,
                func_ref,
            } => {
                let args = args.as_slice(&self.func.dfg.value_lists);
                let node = self.unary(
                    Op::IndirectCall {
                        sig: Sig {
                            val_ins: args.len() as u32,
                            st_ins: 1,
                            val_outs: self.func.dfg.inst_results(inst).len() as u32,
                            st_outs: 1,
                        },
                    },
                    self.graph
                        .add(Op::FuncAddr {
                            opcode: Opcode::FuncAddr,
                            func_ref,
                        })
                        .val_out(0),
                );
                for (i, &arg) in args.iter().enumerate() {
                    node.val_in(1 + i).set_source(self.value(arg));
                }
                node
            }

            InstructionData::FuncAddr { opcode, func_ref } => {
                self.graph.add(Op::FuncAddr { opcode, func_ref })
            }
            InstructionData::HeapAddr {
                opcode,
                heap,
                arg,
                imm,
            } => self.binary(
                Op::HeapAddr { opcode, heap },
                self.value(arg),
                self.const_i64(Opcode::Iconst, imm.into()).val_out(0),
            ),

            InstructionData::Load {
                opcode,
                flags,
                arg,
                offset,
            } => self.unary(
                Op::Memory { opcode, flags },
                self.iadd_offset32(self.value(arg), offset),
            ),
            InstructionData::Store {
                opcode,
                flags,
                args: [arg, ptr],
                offset,
            } => self.binary(
                Op::Memory { opcode, flags },
                self.value(arg),
                self.iadd_offset32(self.value(ptr), offset),
            ),

            InstructionData::Trap { opcode, code } => self.graph.add(Op::Trap { opcode, code }),

            _ => {
                panic!(
                    "{:?} = {:?}",
                    self.func.dfg.inst_results(inst),
                    self.func.dfg[inst]
                );
            }
        };
        let sig = node.kind().sig();
        assert_eq!(sig.st_ins, sig.st_outs);
        if sig.st_ins == 1 {
            node.st_in(0).set_source(self.cur_st.take().unwrap());
            self.cur_st = Some(node.st_out(0));
        }
        self.instructions.insert(inst, node);
    }
}

pub fn construct_function<'g>(graph: &'g Graph, func: &ir::Function) {
    let mut cx = ConstructCx {
        func,
        graph,
        instructions: HashMap::new(),
        ebb_params: func.layout
            .ebbs()
            .map(|ebb| {
                (
                    ebb,
                    (
                        (0..func.dfg.num_ebb_params(ebb))
                            .map(|_| graph.add(Op::ParamVal).val_out(0))
                            .collect(),
                        graph.add(Op::ParamSt).st_out(0),
                    ),
                )
            })
            .collect(),
        cur_st: None,
    };

    for ebb in func.layout.ebbs() {
        cx.cur_st = Some(cx.ebb_params[&ebb].1.clone());
        for inst in func.layout.ebb_insts(ebb) {
            match func.dfg[inst] {
                InstructionData::Jump { .. }
                | InstructionData::Branch { .. }
                | InstructionData::MultiAry { .. } => continue,
                _ => {}
            }
            cx.convert_inst(inst);
        }
    }

    // HACK keep the graph nodes live.
    mem::forget(cx);
}
