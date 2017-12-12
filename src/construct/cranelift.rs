use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::instructions::{InstructionData, InstructionFormat};
use cranelift_codegen::ir::{self, FuncRef, Heap, MemFlags, Opcode, TrapCode, Value, ValueDef};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Op {
    Simple { opcode: Opcode },
    Const { opcode: Opcode, imm: u64 },
    IntCmp { opcode: Opcode, cond: IntCC },
    FloatCmp { opcode: Opcode, cond: FloatCC },
    FuncAddr { opcode: Opcode, func_ref: FuncRef },
    HeapAddr { opcode: Opcode, heap: Heap },
    Memory { opcode: Opcode, flags: MemFlags },
    Trap { opcode: Opcode, code: TrapCode },
    IndirectCall { input_count: u32, output_count: u32 },
    Param,
}

impl ::graph::NodeKind for Op {
    fn num_inputs(&self) -> usize {
        use self::Op::*;
        match *self {
            Simple { opcode } => match opcode.format() {
                InstructionFormat::NullAry => 0,
                InstructionFormat::Unary => 1,
                InstructionFormat::Binary => 2,
                InstructionFormat::Ternary => 3,
                _ => unreachable!(),
            },
            Const { .. } | FuncAddr { .. } | Param => 0,
            HeapAddr { .. } => 2,
            IntCmp { .. } | FloatCmp { .. } => 2,

            Memory { opcode, .. } => 1 + opcode.can_store() as usize + 1,
            Trap { .. } => 0 + 1,

            IndirectCall { input_count, .. } => 1 + input_count as usize,
        }
    }

    fn num_outputs(&self) -> usize {
        use self::Op::*;
        match *self {
            Simple { .. }
            | Const { .. }
            | IntCmp { .. }
            | FloatCmp { .. }
            | FuncAddr { .. }
            | HeapAddr { .. }
            | Param => 1,

            Memory { opcode, .. } => opcode.can_load() as usize + 1,
            Trap { .. } => 0 + 1,

            IndirectCall { output_count, .. } => output_count as usize,
        }
    }
}

pub type Graph = ::graph::Graph<Op>;
pub type Node<'g> = ::graph::Node<'g, Op>;
pub type Input<'g> = ::graph::Input<'g, Op>;
pub type Output<'g> = ::graph::Output<'g, Op>;

pub struct ConstructCx<'g> {
    func: &'g ir::Function,
    graph: &'g Graph,
    instructions: HashMap<ir::Inst, Node<'g>>,
    ebb_params: HashMap<(ir::Ebb, usize), Output<'g>>,
}

impl<'g> ConstructCx<'g> {
    fn value(&self, value: Value) -> Output<'g> {
        match self.func.dfg.value_def(value) {
            ValueDef::Result(inst, num) => match self.instructions.get(&inst) {
                Some(node) => node.output(num),
                None => panic!(
                    "Instruction {:?} (producing value {:?}) missing!\n{:?}",
                    inst, value, self.func
                ),
            },
            ValueDef::Param(ebb, num) => self.ebb_params.get(&(ebb, num)).unwrap().clone(),
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

    fn unary(&self, op: Op, x: Output<'g>) -> Node<'g> {
        let node = self.graph.add(op);
        node.input(0).set_source(x);
        node
    }

    fn binary(&self, op: Op, a: Output<'g>, b: Output<'g>) -> Node<'g> {
        let node = self.graph.add(op);
        node.input(0).set_source(a);
        node.input(1).set_source(b);
        node
    }

    fn ternary(&self, op: Op, a: Output<'g>, b: Output<'g>, c: Output<'g>) -> Node<'g> {
        let node = self.graph.add(op);
        node.input(0).set_source(a);
        node.input(1).set_source(b);
        node.input(2).set_source(c);
        node
    }

    // fn intCompare(&mut self, op: Op, a: Output<'g>, b: Output<'g>) -> Node<'g> {
    //     let node =  self.graph.add(op);
    //     node.input(0).set_source(a);
    //     node.input(1).set_source(b);
    //     node
    // }

    fn convert_inst(&mut self, inst: ir::Inst) {
        let node = match self.func.dfg[inst] {
            InstructionData::NullAry { opcode } => self.graph.add(Op::Simple { opcode }),

            InstructionData::Unary { opcode, arg } => {
                self.unary(Op::Simple { opcode }, self.value(arg))
            }

            InstructionData::UnaryImm { opcode, imm } => self.const_i64(opcode, imm.into()),

            InstructionData::UnaryIeee64 { opcode, imm } => self.const_u64(opcode, imm.bits()),

            InstructionData::Binary {
                opcode,
                args: [a, b],
            } => self.binary(Op::Simple { opcode }, self.value(a), self.value(b)),

            InstructionData::Ternary {
                opcode,
                args: [a, b, c],
            } => self.ternary(
                Op::Simple { opcode },
                self.value(a),
                self.value(b),
                self.value(c),
            ),

            // | InstructionData::UnaryGlobalVar { opcode, .. }
            // | InstructionData::UnaryBool { opcode, imm } => {}
            // | InstructionData::UnaryIeee32 { opcode, imm }
            // | InstructionData::BinaryImm { opcode, .. }
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
                self.const_i64(Opcode::Iconst, imm.into()).output(0),
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

            //InstructionData::IndirectCall | InstructionData::Call
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
                self.const_i64(Opcode::Iconst, imm.into()).output(0),
            ),

            InstructionData::Load {
                opcode,
                flags,
                arg,
                offset,
            } => self.unary(
                Op::Memory { opcode, flags },
                self.binary(
                    Op::Simple {
                        opcode: Opcode::Iadd,
                    },
                    self.value(arg),
                    self.const_i32(Opcode::Iconst, offset.into()).output(0),
                ).output(0),
            ),
            InstructionData::Store {
                opcode,
                flags,
                args: [arg, ptr],
                offset,
            } => self.binary(
                Op::Memory { opcode, flags },
                self.value(arg),
                self.binary(
                    Op::Simple {
                        opcode: Opcode::Iadd,
                    },
                    self.value(ptr),
                    self.const_i32(Opcode::Iconst, offset.into()).output(0),
                ).output(0),
            ),

            InstructionData::Trap { opcode, code } => self.graph.add(Op::Trap { opcode, code }),
            _ => {
                println!(
                    "{:?} = {:?}",
                    self.func.dfg.inst_results(inst),
                    self.func.dfg[inst]
                );
                self.graph.add(Op::Trap {
                    opcode: Opcode::Trap,
                    code: TrapCode::User(0),
                })
            }
        };
        self.instructions.insert(inst, node);
    }
}

pub fn construct_function<'g>(
    graph: &'g Graph,
    func: &ir::Function,
) -> (Vec<Vec<Input<'g>>>, Vec<Output<'g>>) {
    let mut cx = ConstructCx {
        func,
        graph,
        instructions: HashMap::new(),
        ebb_params: HashMap::new(),
    };

    for ebb in func.layout.ebbs() {
        for num in 0..func.dfg.num_ebb_params(ebb) {
            cx.ebb_params
                .insert((ebb, num), graph.add(Op::Param).output(0));
        }
    }

    for ebb in func.layout.ebbs() {
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
    (vec![], vec![])
}
