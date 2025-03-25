/* jshint esversion:11, bitwise:false */
class rv32ima {
    constructor() {
        this.registers = new Map(); // Dynamic register storage (x0-x31)
        this.pc = 0n; // Program Counter
        this.memory = new Map(); // Dynamic memory storage
        this.program = []; // Array to hold the program
    }

    // Helper function to sign-extend a value to 64 bits
    signExtend(value, bits) {
        const mask = 1n << (BigInt(bits) - 1n);
        return (value ^ mask) - mask;
    }

    // Load Upper Immediate
    LUI(rd, imm) {
        this.registers.set(rd, BigInt(imm));
        this.pc += 4n;
    }

    // Add Upper Immediate to PC
    AUIPC(rd, imm) {
        this.registers.set(rd, this.pc + BigInt(imm));
        this.pc += 4n;
    }

    // Jump and Link
    JAL(rd, imm) {
        this.registers.set(rd, this.pc + 4n);
        this.pc += BigInt(imm);
    }

    // Jump and Link Register
    JALR(rd, rs1, imm) {
        const target = ((this.registers.get(rs1) || 0n) + BigInt(imm)) & ~1n;
        this.registers.set(rd, this.pc + 4n);
        this.pc = target;
    }

    // Branch if Equal
    BEQ(rs1, rs2, imm) {
        if (
            (this.registers.get(rs1) || 0n) === (this.registers.get(rs2) || 0n)
        ) {
            this.pc += BigInt(imm);
        } else {
            this.pc += 4n;
        }
    }

    // Branch if Not Equal
    BNE(rs1, rs2, imm) {
        if (
            (this.registers.get(rs1) || 0n) !== (this.registers.get(rs2) || 0n)
        ) {
            this.pc += BigInt(imm);
        } else {
            this.pc += 4n;
        }
    }

    // Branch if Less Than
    BLT(rs1, rs2, imm) {
        if ((this.registers.get(rs1) || 0n) < (this.registers.get(rs2) || 0n)) {
            this.pc += BigInt(imm);
        } else {
            this.pc += 4n;
        }
    }

    // Branch if Greater Than or Equal
    BGE(rs1, rs2, imm) {
        if (
            (this.registers.get(rs1) || 0n) >= (this.registers.get(rs2) || 0n)
        ) {
            this.pc += BigInt(imm);
        } else {
            this.pc += 4n;
        }
    }

    // Branch if Less Than Unsigned
    BLTU(rs1, rs2, imm) {
        if (
            BigInt.asUintN(64, this.registers.get(rs1) || 0n) <
            BigInt.asUintN(64, this.registers.get(rs2) || 0n)
        ) {
            this.pc += BigInt(imm);
        } else {
            this.pc += 4n;
        }
    }

    // Branch if Greater Than or Equal Unsigned
    BGEU(rs1, rs2, imm) {
        if (
            BigInt.asUintN(64, this.registers.get(rs1) || 0n) >=
            BigInt.asUintN(64, this.registers.get(rs2) || 0n)
        ) {
            this.pc += BigInt(imm);
        } else {
            this.pc += 4n;
        }
    }

    // Load Byte
    LB(rd, rs1, imm) {
        const addr = (this.registers.get(rs1) || 0n) + BigInt(imm);
        const value = this.memory.get(addr) || 0n;
        this.registers.set(rd, this.signExtend(value, 8));
        this.pc += 4n;
    }

    // Load Halfword
    LH(rd, rs1, imm) {
        const addr = (this.registers.get(rs1) || 0n) + BigInt(imm);
        const value =
            (this.memory.get(addr) || 0n) |
            ((this.memory.get(addr + 1n) || 0n) << 8n);
        this.registers.set(rd, this.signExtend(value, 16));
        this.pc += 4n;
    }

    // Load Word
    LW(rd, rs1, imm) {
        const addr = (this.registers.get(rs1) || 0n) + BigInt(imm);
        const value =
            (this.memory.get(addr) || 0n) |
            ((this.memory.get(addr + 1n) || 0n) << 8n) |
            ((this.memory.get(addr + 2n) || 0n) << 16n) |
            ((this.memory.get(addr + 3n) || 0n) << 24n);
        this.registers.set(rd, this.signExtend(value, 32));
        this.pc += 4n;
    }

    // Load Doubleword
    LD(rd, rs1, imm) {
        const addr = (this.registers.get(rs1) || 0n) + BigInt(imm);
        const value =
            (this.memory.get(addr) || 0n) |
            ((this.memory.get(addr + 1n) || 0n) << 8n) |
            ((this.memory.get(addr + 2n) || 0n) << 16n) |
            ((this.memory.get(addr + 3n) || 0n) << 24n) |
            ((this.memory.get(addr + 4n) || 0n) << 32n) |
            ((this.memory.get(addr + 5n) || 0n) << 40n) |
            ((this.memory.get(addr + 6n) || 0n) << 48n) |
            ((this.memory.get(addr + 7n) || 0n) << 56n);
        this.registers.set(rd, value);
        this.pc += 4n;
    }

    // Store Byte
    SB(rs1, rs2, imm) {
        const addr = (this.registers.get(rs2) || 0n) + BigInt(imm); // Calculate the address
        const value = this.registers.get(rs1) || 0n; // Get the value from rs1
        this.memory.set(addr, value & 0xffn); // Store the least significant byte
        this.pc += 4n; // Increment the program counter
    }

    // Store Halfword
    SH(rs1, rs2, imm) {
        const addr = (this.registers.get(rs2) || 0n) + BigInt(imm);
        const value = (this.registers.get(rs1) || 0n) & 0xffffn;
        this.memory.set(addr, value & 0xffn);
        this.memory.set(addr + 1n, (value >> 8n) & 0xffn);
        this.pc += 4n;
    }

    // Store Word
    SW(rs1, rs2, imm) {
        const addr = (this.registers.get(rs2) || 0n) + BigInt(imm);
        const value = (this.registers.get(rs1) || 0n) & 0xffffffffn;
        this.memory.set(addr, value & 0xffn);
        this.memory.set(addr + 1n, (value >> 8n) & 0xffn);
        this.memory.set(addr + 2n, (value >> 16n) & 0xffn);
        this.memory.set(addr + 3n, (value >> 24n) & 0xffn);
        this.pc += 4n;
    }

    // Store Doubleword
    SD(rs1, rs2, imm) {
        const addr = (this.registers.get(rs2) || 0n) + BigInt(imm);
        const value = this.registers.get(rs1) || 0n;
        this.memory.set(addr, value & 0xffn);
        this.memory.set(addr + 1n, (value >> 8n) & 0xffn);
        this.memory.set(addr + 2n, (value >> 16n) & 0xffn);
        this.memory.set(addr + 3n, (value >> 24n) & 0xffn);
        this.memory.set(addr + 4n, (value >> 32n) & 0xffn);
        this.memory.set(addr + 5n, (value >> 40n) & 0xffn);
        this.memory.set(addr + 6n, (value >> 48n) & 0xffn);
        this.memory.set(addr + 7n, (value >> 56n) & 0xffn);
        this.pc += 4n;
    }

    // Add Immediate
    ADDI(rd, rs1, imm) {
        this.registers.set(rd, (this.registers.get(rs1) || 0n) + BigInt(imm));
        this.pc += 4n;
    }

    // Shift Left Logical Immediate
    SLLI(rd, rs1, shamt) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) << BigInt(shamt)
        );
        this.pc += 4n;
    }

    // Set Less Than Immediate
    SLTI(rd, rs1, imm) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) < BigInt(imm) ? 1n : 0n
        );
        this.pc += 4n;
    }

    // Set Less Than Immediate Unsigned
    SLTIU(rd, rs1, imm) {
        this.registers.set(
            rd,
            BigInt.asUintN(64, this.registers.get(rs1) || 0n) <
                BigInt.asUintN(64, BigInt(imm))
                ? 1n
                : 0n
        );
        this.pc += 4n;
    }

    // XOR Immediate
    XORI(rd, rs1, imm) {
        this.registers.set(rd, (this.registers.get(rs1) || 0n) ^ BigInt(imm));
        this.pc += 4n;
    }

    // Shift Right Logical Immediate
    SRLI(rd, rs1, shamt) {
        this.registers.set(
            rd,
            BigInt.asUintN(64, this.registers.get(rs1) || 0n) >> BigInt(shamt)
        );
        this.pc += 4n;
    }

    // Shift Right Arithmetic Immediate
    SRAI(rd, rs1, shamt) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) >> BigInt(shamt)
        );
        this.pc += 4n;
    }

    // OR Immediate
    ORI(rd, rs1, imm) {
        this.registers.set(rd, (this.registers.get(rs1) || 0n) | BigInt(imm));
        this.pc += 4n;
    }

    // AND Immediate
    ANDI(rd, rs1, imm) {
        this.registers.set(rd, (this.registers.get(rs1) || 0n) & BigInt(imm));
        this.pc += 4n;
    }

    // Add
    ADD(rd, rs1, rs2) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) + (this.registers.get(rs2) || 0n)
        );
        this.pc += 4n;
    }

    // Subtract
    SUB(rd, rs1, rs2) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) - (this.registers.get(rs2) || 0n)
        );
        this.pc += 4n;
    }

    // Shift Left Logical
    SLL(rd, rs1, rs2) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) <<
                ((this.registers.get(rs2) || 0n) & 0x3fn)
        );
        this.pc += 4n;
    }

    // Set Less Than
    SLT(rd, rs1, rs2) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) < (this.registers.get(rs2) || 0n)
                ? 1n
                : 0n
        );
        this.pc += 4n;
    }

    // Set Less Than Unsigned
    SLTU(rd, rs1, rs2) {
        this.registers.set(
            rd,
            BigInt.asUintN(64, this.registers.get(rs1) || 0n) <
                BigInt.asUintN(64, this.registers.get(rs2) || 0n)
                ? 1n
                : 0n
        );
        this.pc += 4n;
    }

    // XOR
    XOR(rd, rs1, rs2) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) ^ (this.registers.get(rs2) || 0n)
        );
        this.pc += 4n;
    }

    // Shift Right Logical
    SRL(rd, rs1, rs2) {
        this.registers.set(
            rd,
            BigInt.asUintN(64, this.registers.get(rs1) || 0n) >>
                ((this.registers.get(rs2) || 0n) & 0x3fn)
        );
        this.pc += 4n;
    }

    // Shift Right Arithmetic
    SRA(rd, rs1, rs2) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) >>
                ((this.registers.get(rs2) || 0n) & 0x3fn)
        );
        this.pc += 4n;
    }

    // OR
    OR(rd, rs1, rs2) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) | (this.registers.get(rs2) || 0n)
        );
        this.pc += 4n;
    }

    // AND
    AND(rd, rs1, rs2) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) & (this.registers.get(rs2) || 0n)
        );
        this.pc += 4n;
    }

    // Multiply
    MUL(rd, rs1, rs2) {
        this.registers.set(
            rd,
            (this.registers.get(rs1) || 0n) * (this.registers.get(rs2) || 0n)
        );
        this.pc += 4n;
    }

    // Multiply High (Signed)
    MULH(rd, rs1, rs2) {
        const product =
            BigInt.asIntN(128, this.registers.get(rs1) || 0n) *
            BigInt.asIntN(128, this.registers.get(rs2) || 0n);
        this.registers.set(rd, product >> 64n);
        this.pc += 4n;
    }

    // Multiply High (Unsigned)
    MULHU(rd, rs1, rs2) {
        const product =
            BigInt.asUintN(128, this.registers.get(rs1) || 0n) *
            BigInt.asUintN(128, this.registers.get(rs2) || 0n);
        this.registers.set(rd, product >> 64n);
        this.pc += 4n;
    }

    // Multiply High (Signed * Unsigned)
    MULHSU(rd, rs1, rs2) {
        const product =
            BigInt.asIntN(128, this.registers.get(rs1) || 0n) *
            BigInt.asUintN(128, this.registers.get(rs2) || 0n);
        this.registers.set(rd, product >> 64n);
        this.pc += 4n;
    }

    // Divide (Signed)
    DIV(rd, rs1, rs2) {
        const divisor = this.registers.get(rs2) || 0n;
        if (divisor === 0n) {
            this.registers.set(rd, -1n); // Division by zero
        } else {
            this.registers.set(
                rd,
                BigInt.asIntN(64, (this.registers.get(rs1) || 0n) / divisor)
            );
        }
        this.pc += 4n;
    }

    // Divide (Unsigned)
    DIVU(rd, rs1, rs2) {
        const divisor = this.registers.get(rs2) || 0n;
        if (divisor === 0n) {
            this.registers.set(rd, -1n); // Division by zero
        } else {
            this.registers.set(
                rd,
                BigInt.asUintN(64, (this.registers.get(rs1) || 0n) / divisor)
            );
        }
        this.pc += 4n;
    }

    // Remainder (Signed)
    REM(rd, rs1, rs2) {
        const divisor = this.registers.get(rs2) || 0n;
        if (divisor === 0n) {
            this.registers.set(rd, this.registers.get(rs1) || 0n); // Division by zero
        } else {
            this.registers.set(
                rd,
                BigInt.asIntN(64, (this.registers.get(rs1) || 0n) % divisor)
            );
        }
        this.pc += 4n;
    }

    // Remainder (Unsigned)
    REMU(rd, rs1, rs2) {
        const divisor = this.registers.get(rs2) || 0n;
        if (divisor === 0n) {
            this.registers.set(rd, this.registers.get(rs1) || 0n); // Division by zero
        } else {
            this.registers.set(
                rd,
                BigInt.asUintN(64, (this.registers.get(rs1) || 0n) % divisor)
            );
        }
        this.pc += 4n;
    }

    // Print a string from memory
    printString(addr, length) {
        let str = "";
        for (let i = 0; i < length; i++) {
            const charCode = Number(this.memory.get(addr + BigInt(i)) || 0);
            str += String.fromCharCode(charCode);
        }
        console.log(str);
    }

    // Load a program into memory
    loadProgram(program) {
        this.program = program;
        for (let i = 0; i < program.length; i++) {
            this.memory.set(BigInt(i * 4), program[i]); // Assume 4-byte instructions
        }
    }

    // Execute the program
    execute() {
        while (this.pc < BigInt(this.program.length * 4)) {
            const instruction = this.memory.get(this.pc) || 0n;
            this.decodeAndExecute(instruction);
        }
    }

    // Decode and execute an instruction
    decodeAndExecute(instruction) {
        const opcode = instruction & 0x7fn; // Extract opcode (7 bits)
        const rd = (instruction >> 7n) & 0x1fn; // Extract rd (5 bits)
        const rs1 = (instruction >> 15n) & 0x1fn; // Extract rs1 (5 bits)
        const rs2 = (instruction >> 20n) & 0x1fn; // Extract rs2 (5 bits)
        const funct3 = (instruction >> 12n) & 0x7n; // Extract funct3 (3 bits)
        const funct7 = (instruction >> 25n) & 0x7fn; // Extract funct7 (7 bits)
        let imm;
        switch (opcode) {
            case 0x03n: // I-type (load instructions)
            case 0x13n: // I-type (arithmetic instructions)
                imm = (instruction >> 20n) & 0xfffn; // 12-bit immediate
                imm = this.signExtend(imm, 12); // Sign-extend to 64 bits
                break;
            case 0x23n: // S-type (store instructions)
                // Corrected immediate calculation for S-type
                imm =
                    ((instruction >> 25n) << 5n) |
                    ((instruction >> 7n) & 0x1fn);
                imm = this.signExtend(imm, 12);
                break;
            case 0x63n: // B-type (branch instructions)
                imm =
                    ((instruction >> 7n) & 0x1en) |
                    ((instruction >> 20n) & 0x7e0n) |
                    ((instruction >> 19n) & 0x800n) |
                    ((instruction >> 31n) & 0x1000n); // 13-bit immediate (multiplied by 2)
                imm = this.signExtend(imm, 13); // Sign-extend to 64 bits
                break;
            case 0x37n: // U-type (LUI)
            case 0x17n: // U-type (AUIPC)
                imm = instruction & 0xfffff000n; // 20-bit immediate
                break;
            case 0x6fn: // J-type (JAL)
                imm =
                    ((instruction >> 20n) & 0x7fen) |
                    ((instruction >> 9n) & 0x800n) |
                    (instruction & 0xff000n) |
                    ((instruction >> 11n) & 0x100000n); // 21-bit immediate (multiplied by 2)
                imm = this.signExtend(imm, 21); // Sign-extend to 64 bits
                break;
            default:
                imm = 0n; // Default to 0 if immediate is not applicable
        }
        //console.log(imm);
        switch (opcode) {
            case 0x00n:
                this.pc += 4n;
                break;
            case 0x13n: // ADDI, SLLI, SLTI, SLTIU, XORI, SRLI, SRAI, ORI, ANDI
                switch (funct3) {
                    case 0x0n:
                        this.ADDI(rd, rs1, imm);
                        break;
                    case 0x1n:
                        this.SLLI(rd, rs1, imm);
                        break;
                    case 0x2n:
                        this.SLTI(rd, rs1, imm);
                        break;
                    case 0x3n:
                        this.SLTIU(rd, rs1, imm);
                        break;
                    case 0x4n:
                        this.XORI(rd, rs1, imm);
                        break;
                    case 0x5n:
                        if (funct7 === 0x00n) this.SRLI(rd, rs1, imm);
                        else if (funct7 === 0x20n) this.SRAI(rd, rs1, imm);
                        break;
                    case 0x6n:
                        this.ORI(rd, rs1, imm);
                        break;
                    case 0x7n:
                        this.ANDI(rd, rs1, imm);
                        break;
                    default:
                        throw new Error(
                            `Unknown funct3 for opcode 0x13: ${funct3}`
                        );
                }
                break;
            case 0x03n: // LB, LH, LW, LD
                switch (funct3) {
                    case 0x0n:
                        this.LB(rd, rs1, imm);
                        break;
                    case 0x1n:
                        this.LH(rd, rs1, imm);
                        break;
                    case 0x2n:
                        this.LW(rd, rs1, imm);
                        break;
                    case 0x3n:
                        this.LD(rd, rs1, imm);
                        break;
                    default:
                        throw new Error(
                            `Unknown funct3 for opcode 0x03: ${funct3}`
                        );
                }
                break;
            case 0x23n: // SB, SH, SW, SD
                switch (funct3) {
                    case 0x0n:
                        this.SB(rs1, rs2, imm);
                        break;
                    case 0x1n:
                        this.SH(rs1, rs2, imm);
                        break;
                    case 0x2n:
                        this.SW(rs1, rs2, imm);
                        break;
                    case 0x3n:
                        this.SD(rs1, rs2, imm);
                        break;
                    default:
                        throw new Error(
                            `Unknown funct3 for opcode 0x23: ${funct3}`
                        );
                }
                break;
            case 0x37n: // LUI
                this.LUI(rd, imm);
                break;
            case 0x17n: // AUIPC
                this.AUIPC(rd, imm);
                break;
            case 0x6fn: // JAL
                this.JAL(rd, imm);
                break;
            case 0x67n: // JALR
                this.JALR(rd, rs1, imm);
                break;
            case 0x63n: // BEQ, BNE, BLT, BGE, BLTU, BGEU
                switch (funct3) {
                    case 0x0n:
                        this.BEQ(rs1, rs2, imm);
                        break;
                    case 0x1n:
                        this.BNE(rs1, rs2, imm);
                        break;
                    case 0x4n:
                        this.BLT(rs1, rs2, imm);
                        break;
                    case 0x5n:
                        this.BGE(rs1, rs2, imm);
                        break;
                    case 0x6n:
                        this.BLTU(rs1, rs2, imm);
                        break;
                    case 0x7n:
                        this.BGEU(rs1, rs2, imm);
                        break;
                    default:
                        throw new Error(
                            `Unknown funct3 for opcode 0x63: ${funct3}`
                        );
                }
                break;
            case 0x33n: // ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
                switch (funct3) {
                    case 0x0n:
                        if (funct7 === 0x00n) this.ADD(rd, rs1, rs2);
                        else if (funct7 === 0x20n) this.SUB(rd, rs1, rs2);
                        break;
                    case 0x1n:
                        this.SLL(rd, rs1, rs2);
                        break;
                    case 0x2n:
                        this.SLT(rd, rs1, rs2);
                        break;
                    case 0x3n:
                        this.SLTU(rd, rs1, rs2);
                        break;
                    case 0x4n:
                        this.XOR(rd, rs1, rs2);
                        break;
                    case 0x5n:
                        if (funct7 === 0x00n) this.SRL(rd, rs1, rs2);
                        else if (funct7 === 0x20n) this.SRA(rd, rs1, rs2);
                        break;
                    case 0x6n:
                        this.OR(rd, rs1, rs2);
                        break;
                    case 0x7n:
                        this.AND(rd, rs1, rs2);
                        break;
                    default:
                        throw new Error(
                            `Unknown funct3 for opcode 0x33: ${funct3}`
                        );
                }
                break;
            case 0x3bn: // MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU
                switch (funct3) {
                    case 0x0n:
                        this.MUL(rd, rs1, rs2);
                        break;
                    case 0x1n:
                        this.MULH(rd, rs1, rs2);
                        break;
                    case 0x2n:
                        this.MULHSU(rd, rs1, rs2);
                        break;
                    case 0x3n:
                        this.MULHU(rd, rs1, rs2);
                        break;
                    case 0x4n:
                        this.DIV(rd, rs1, rs2);
                        break;
                    case 0x5n:
                        this.DIVU(rd, rs1, rs2);
                        break;
                    case 0x6n:
                        this.REM(rd, rs1, rs2);
                        break;
                    case 0x7n:
                        this.REMU(rd, rs1, rs2);
                        break;
                    default:
                        throw new Error(
                            `Unknown funct3 for opcode 0x3B: ${funct3}`
                        );
                }
                break;
            case 0x73n: // SYSTEM (ECALL/EBREAK)
                if (funct3 === 0x0n) {
                    this.handleECALL();
                } else {
                    throw new Error(`Unsupported SYSTEM instruction`);
                }
                break;
            default:
                throw new Error(`Unknown opcode: ${opcode.toString(16)}`);
        }
    }

    // Translate human-readable instructions into a program array
    static translate(instructions) {
        const program = [];
        let dataOffset = 0x1000n;

        for (const instr of instructions) {
            const parts = instr.match(/'[^']*'|\S+/g);
            //console.log(parts)
            if (parts.length === 0) continue;
            const op = parts[0];
            const args = parts.slice(1);

            let instruction = 0n;
            let shouldPush = true;

            // Handle pseudo-instructions first
            if (op.toLowerCase() === "li") {
                program.push(
                    ...this.translate([`addi ${args[0]} x0 ${args[1]}`])
                );
                continue;
            }

            // Main instruction switch with grouping
            switch (op.toLowerCase()) {
                case "lui":
                    instruction = 0x37n;
                    instruction |= BigInt(args[0].slice(1)) << 7n;
                    instruction |= BigInt(args[1]) << 20n;
                    break;

                case "auipc":
                    instruction = 0x17n;
                    instruction |= BigInt(args[0].slice(1)) << 7n;
                    instruction |= BigInt(args[1]) << 20n;
                    break;

                case "jal": {
                    instruction = 0x6fn;
                    instruction |= BigInt(args[0].slice(1)) << 7n;
                    const imm = BigInt(args[1]);
                    instruction |= imm << 20n;
                    break;
                }

                case "jalr": {
                    instruction = 0x67n;
                    instruction |= BigInt(args[0].slice(1)) << 7n;
                    instruction |= BigInt(args[1].slice(1)) << 15n;
                    instruction |= BigInt(args[2]) << 20n;
                    break;
                }

                // Branch instructions
                case "beq":
                case "bne":
                case "blt":
                case "bge":
                case "bltu":
                case "bgeu": {
                    instruction = 0x63n;
                    const funct3Map = {
                        beq: 0x0n,
                        bne: 0x1n,
                        blt: 0x4n,
                        bge: 0x5n,
                        bltu: 0x6n,
                        bgeu: 0x7n,
                    };
                    instruction |= funct3Map[op] << 12n;
                    instruction |= BigInt(args[0].slice(1)) << 15n;
                    instruction |= BigInt(args[1].slice(1)) << 20n;
                    const imm = BigInt(args[2]);
                    instruction |= (imm & 0x800n) << 19n; // imm[12]
                    instruction |= (imm & 0x1e) << 7n; // imm[4:1]
                    instruction |= (imm & 0x7e0n) << 20n; // imm[10:5]
                    instruction |= (imm & 0x1000n) << 19n; // imm[11]
                    break;
                }

                // Load instructions
                case "lb":
                case "lh":
                case "lw":
                case "ld": {
                    instruction = 0x03n;
                    const funct3Map = {
                        lb: 0x0n,
                        lh: 0x1n,
                        lw: 0x2n,
                        ld: 0x3n,
                    };
                    instruction |= funct3Map[op] << 12n;
                    instruction |= BigInt(args[0].slice(1)) << 7n;
                    instruction |= BigInt(args[1].slice(1)) << 15n;
                    instruction |= BigInt(args[2]) << 20n;
                    break;
                }

                // Store instructions
                case "sb":
                case "sh":
                case "sw":
                case "sd": {
                    instruction = 0x23n;
                    const funct3Map = {
                        sb: 0x0n,
                        sh: 0x1n,
                        sw: 0x2n,
                        sd: 0x3n,
                    };
                    instruction |= funct3Map[op] << 12n;
                    instruction |= BigInt(args[0].slice(1)) << 15n;
                    instruction |= BigInt(args[1].slice(1)) << 20n;
                    const imm = BigInt(args[2]);
                    instruction |=
                        ((imm & 0xfe0n) << 20n) | ((imm & 0x1fn) << 7n);
                    break;
                }

                // I-type instructions
                case "addi":
                case "slti":
                case "sltiu":
                case "xori":
                case "ori":
                case "andi": {
                    instruction = 0x13n;
                    const funct3Map = {
                        addi: 0x0n,
                        slti: 0x2n,
                        sltiu: 0x3n,
                        xori: 0x4n,
                        ori: 0x6n,
                        andi: 0x7n,
                    };
                    instruction |= funct3Map[op] << 12n;
                    instruction |= BigInt(args[0].slice(1)) << 7n;
                    instruction |= BigInt(args[1].slice(1)) << 15n;
                    instruction |= BigInt(args[2]) << 20n;
                    break;
                }

                // Shift instructions
                case "slli":
                case "srli":
                case "srai": {
                    instruction = 0x13n;
                    const functMap = {
                        slli: { funct3: 0x1n, funct7: 0x00n },
                        srli: { funct3: 0x5n, funct7: 0x00n },
                        srai: { funct3: 0x5n, funct7: 0x20n },
                    };
                    const { funct3, funct7 } = functMap[op];
                    instruction |= funct3 << 12n;
                    instruction |= funct7 << 25n;
                    instruction |= BigInt(args[0].slice(1)) << 7n;
                    instruction |= BigInt(args[1].slice(1)) << 15n;
                    instruction |= BigInt(args[2]) << 20n;
                    break;
                }

                // R-type instructions
                case "add":
                case "sub":
                case "sll":
                case "slt":
                case "sltu":
                case "xor":
                case "srl":
                case "sra":
                case "or":
                case "and": {
                    instruction = 0x33n;
                    const functMap = {
                        add: { funct3: 0x0n, funct7: 0x00n },
                        sub: { funct3: 0x0n, funct7: 0x20n },
                        sll: { funct3: 0x1n, funct7: 0x00n },
                        slt: { funct3: 0x2n, funct7: 0x00n },
                        sltu: { funct3: 0x3n, funct7: 0x00n },
                        xor: { funct3: 0x4n, funct7: 0x00n },
                        srl: { funct3: 0x5n, funct7: 0x00n },
                        sra: { funct3: 0x5n, funct7: 0x20n },
                        or: { funct3: 0x6n, funct7: 0x00n },
                        and: { funct3: 0x7n, funct7: 0x00n },
                    };
                    const { funct3, funct7 } = functMap[op];
                    instruction |= funct3 << 12n;
                    instruction |= funct7 << 25n;
                    instruction |= BigInt(args[0].slice(1)) << 7n;
                    instruction |= BigInt(args[1].slice(1)) << 15n;
                    instruction |= BigInt(args[2].slice(1)) << 20n;
                    break;
                }

                // M-extension instructions
                case "mul":
                case "mulh":
                case "mulhsu":
                case "mulhu":
                case "div":
                case "divu":
                case "rem":
                case "remu": {
                    instruction = 0x33n;
                    const functMap = {
                        mul: { funct3: 0x0n, funct7: 0x01n },
                        mulh: { funct3: 0x1n, funct7: 0x01n },
                        mulhsu: { funct3: 0x2n, funct7: 0x01n },
                        mulhu: { funct3: 0x3n, funct7: 0x01n },
                        div: { funct3: 0x4n, funct7: 0x01n },
                        divu: { funct3: 0x5n, funct7: 0x01n },
                        rem: { funct3: 0x6n, funct7: 0x01n },
                        remu: { funct3: 0x7n, funct7: 0x01n },
                    };
                    const { funct3, funct7 } = functMap[op];
                    instruction |= funct3 << 12n;
                    instruction |= funct7 << 25n;
                    instruction |= BigInt(args[0].slice(1)) << 7n;
                    instruction |= BigInt(args[1].slice(1)) << 15n;
                    instruction |= BigInt(args[2].slice(1)) << 20n;
                    break;
                }

                case "ecall":
                    instruction = 0x73n;
                    break;

                case ".string": {
                    const addrReg = "x11";
                    const str = args
                        //.slice(1)
                        .join(" ")
                        .replace(/^'(.*)'$/, "$1");
                    //console.log(str);
                    const processedStr = this.processEscapes(str);

                    // Store string at dataOffset
                    const upper = (dataOffset >> 12n) & 0xfffffn;
                    const lower = dataOffset & 0xfffn;
                    //console.log(lower);
                    program.push(
                        ...this.translate([`li x12 ${processedStr.length}`])
                    );
                    program.push(
                        ...this.translate([
                            `lui ${addrReg} ${upper}`,
                            `addi ${addrReg} ${addrReg} ${lower}`,
                        ])
                    );

                    for (let i = 0; i < processedStr.length; i++) {
                        const charCode = processedStr.charCodeAt(i);
                        program.push(
                            ...this.translate([
                                `li x5 ${charCode}`,
                                `sb x5 ${addrReg} ${i}`,
                            ])
                        );
                    }

                    dataOffset += BigInt(processedStr.length);
                    shouldPush = false;
                    break;
                }

                default:
                    throw new Error(`Unknown instruction: ${op}`);
            }

            if (shouldPush) program.push(instruction);
        }
        return program;
    }

    static untranslate(program) {
        const instructions = [];
        for (const inst of program) {
            const decoded = this.decodeInstruction(BigInt(inst));
            const { opcode, funct3, funct7, rd, rs1, rs2, imm } = decoded;
            let mnemonic;
            let operands = [];

            switch (opcode) {
                case 0x37n: // LUI
                    mnemonic = "lui";
                    operands = [`x${rd}`, (imm >> 12n).toString()];
                    break;
                case 0x17n: // AUIPC
                    mnemonic = "auipc";
                    operands = [`x${rd}`, (imm >> 12n).toString()];
                    break;
                case 0x6fn: // JAL
                    mnemonic = "jal";
                    operands = [`x${rd}`, imm.toString()];
                    break;
                case 0x67n: // JALR
                    mnemonic = "jalr";
                    operands = [`x${rd}`, `x${rs1}`, imm.toString()];
                    break;
                case 0x63n: // B-type
                    switch (funct3) {
                        case 0x0n:
                            mnemonic = "beq";
                            break;
                        case 0x1n:
                            mnemonic = "bne";
                            break;
                        case 0x4n:
                            mnemonic = "blt";
                            break;
                        case 0x5n:
                            mnemonic = "bge";
                            break;
                        case 0x6n:
                            mnemonic = "bltu";
                            break;
                        case 0x7n:
                            mnemonic = "bgeu";
                            break;
                        default:
                            throw new Error(`Unknown branch funct3: ${funct3}`);
                    }
                    operands = [`x${rs1}`, `x${rs2}`, imm.toString()];
                    break;
                case 0x03n: // Load (formatted as `mnemonic rd, rs1, imm`)
                    switch (funct3) {
                        case 0x0n:
                            mnemonic = "lb";
                            break;
                        case 0x1n:
                            mnemonic = "lh";
                            break;
                        case 0x2n:
                            mnemonic = "lw";
                            break;
                        case 0x3n:
                            mnemonic = "ld";
                            break;
                        default:
                            throw new Error(`Unknown load funct3: ${funct3}`);
                    }
                    operands = [`x${rd}`, `x${rs1}`, imm.toString()];
                    break;
                case 0x23n: // Store (formatted as `mnemonic rs1, rs2, imm`)
                    switch (funct3) {
                        case 0x0n:
                            mnemonic = "sb";
                            break;
                        case 0x1n:
                            mnemonic = "sh";
                            break;
                        case 0x2n:
                            mnemonic = "sw";
                            break;
                        case 0x3n:
                            mnemonic = "sd";
                            break;
                        default:
                            throw new Error(`Unknown store funct3: ${funct3}`);
                    }
                    operands = [`x${rs2}`, `x${rs1}`, imm.toString()];
                    break;
                case 0x13n: // I-type
                    switch (funct3) {
                        case 0x0n:
                            mnemonic = "addi";
                            break;
                        case 0x1n: // SLLI
                            if (funct7 === 0x00n) {
                                mnemonic = "slli";
                            } else {
                                throw new Error(
                                    `Invalid funct7 for SLLI: ${funct7}`
                                );
                            }
                            break;
                        case 0x2n:
                            mnemonic = "slti";
                            break;
                        case 0x3n:
                            mnemonic = "sltiu";
                            break;
                        case 0x4n:
                            mnemonic = "xori";
                            break;
                        case 0x5n:
                            if (funct7 === 0x00n) mnemonic = "srli";
                            else if (funct7 === 0x20n) mnemonic = "srai";
                            else
                                throw new Error(
                                    `Invalid funct7 for SRxI: ${funct7}`
                                );
                            break;
                        case 0x6n:
                            mnemonic = "ori";
                            break;
                        case 0x7n:
                            mnemonic = "andi";
                            break;
                        default:
                            throw new Error(`Unknown I-type funct3: ${funct3}`);
                    }
                    operands = [`x${rd}`, `x${rs1}`, imm.toString()];
                    break;
                case 0x33n: // R-type
                    if (funct7 === 0x01n) {
                        // M-extension
                        switch (funct3) {
                            case 0x0n:
                                mnemonic = "mul";
                                break;
                            case 0x1n:
                                mnemonic = "mulh";
                                break;
                            case 0x2n:
                                mnemonic = "mulhsu";
                                break;
                            case 0x3n:
                                mnemonic = "mulhu";
                                break;
                            case 0x4n:
                                mnemonic = "div";
                                break;
                            case 0x5n:
                                mnemonic = "divu";
                                break;
                            case 0x6n:
                                mnemonic = "rem";
                                break;
                            case 0x7n:
                                mnemonic = "remu";
                                break;
                            default:
                                throw new Error(
                                    `Unknown M-extension funct3: ${funct3}`
                                );
                        }
                    } else {
                        switch (funct3) {
                            case 0x0n:
                                if (funct7 === 0x00n) mnemonic = "add";
                                else if (funct7 === 0x20n) mnemonic = "sub";
                                else
                                    throw new Error(
                                        `Invalid funct7 for ADD/SUB: ${funct7}`
                                    );
                                break;
                            case 0x1n:
                                mnemonic = "sll";
                                break;
                            case 0x2n:
                                mnemonic = "slt";
                                break;
                            case 0x3n:
                                mnemonic = "sltu";
                                break;
                            case 0x4n:
                                mnemonic = "xor";
                                break;
                            case 0x5n:
                                if (funct7 === 0x00n) mnemonic = "srl";
                                else if (funct7 === 0x20n) mnemonic = "sra";
                                else
                                    throw new Error(
                                        `Invalid funct7 for SRx: ${funct7}`
                                    );
                                break;
                            case 0x6n:
                                mnemonic = "or";
                                break;
                            case 0x7n:
                                mnemonic = "and";
                                break;
                            default:
                                throw new Error(
                                    `Unknown R-type funct3: ${funct3}`
                                );
                        }
                    }
                    operands = [`x${rd}`, `x${rs1}`, `x${rs2}`];
                    break;
                case 0x73n: // ECALL
                    if (funct3 === 0x0n) {
                        mnemonic = "ecall";
                        operands = [];
                    } else {
                        throw new Error(`Unsupported SYSTEM instruction`);
                    }
                    break;
                default:
                    throw new Error(`Unknown opcode: 0x${opcode.toString(16)}`);
            }
            instructions.push(`${mnemonic} ${operands.join(", ")}`);
        }
        return instructions;
    }

    static decodeInstruction(inst) {
        inst = BigInt(inst);
        const opcode = inst & 0x7fn;
        const rd = (inst >> 7n) & 0x1fn;
        const funct3 = (inst >> 12n) & 0x7n;
        const rs1 = (inst >> 15n) & 0x1fn;
        const rs2 = (inst >> 20n) & 0x1fn;
        const funct7 = (inst >> 25n) & 0x7fn;
        let imm = 0n;

        switch (opcode) {
            case 0x03n: // I-type (load)
            case 0x13n: // I-type (ALU)
            case 0x67n: // JALR
                imm = (inst >> 20n) & 0xfffn;
                imm = this.signExtend(imm, 12);
                break;
            case 0x23n: // S-type (store)
                imm = ((inst >> 25n) << 5n) | ((inst >> 7n) & 0x1fn);
                imm = this.signExtend(imm, 12);
                break;
            case 0x63n: // B-type (branch)
                imm =
                    ((inst >> 7n) & 0x1en) |
                    ((inst >> 20n) & 0x7e0n) |
                    ((inst >> 19n) & 0x800n) |
                    ((inst >> 31n) << 12n);
                imm = this.signExtend(imm, 13);
                break;
            case 0x37n: // U-type (LUI)
            case 0x17n: // U-type (AUIPC)
                imm = inst & 0xfffff000n;
                break;
            case 0x6fn: // J-type (JAL)
                imm =
                    ((inst >> 20n) & 0x7fen) |
                    ((inst >> 9n) & 0x800n) |
                    (inst & 0xff000n) |
                    ((inst >> 11n) & 0x100000n);
                imm = this.signExtend(imm, 21);
                break;
            default:
                break;
        }

        return {
            opcode,
            funct3,
            funct7,
            rd: Number(rd),
            rs1: Number(rs1),
            rs2: Number(rs2),
            imm,
        };
    }

    static signExtend(value, bits) {
        const mask = 1n << (BigInt(bits) - 1n);
        return (value ^ mask) - mask;
    }

    static processEscapes(str) {
        return str.replace(/\\(.)/g, (_, c) => {
            switch (c) {
                case "n":
                    return "\n";
                case "t":
                    return "\t";
                case "r":
                    return "\r";
                case "\\":
                    return "\\";
                case "'":
                    return "'";
                default:
                    throw new Error(`Invalid escape: \\${c}`);
            }
        });
    }

    handleECALL() {
        const syscall = this.registers.get(17n) || 0n; // a7
        switch (syscall) {
            case 64n: // write
                const addr = this.registers.get(11n) || 0n; // a1 (string address)
                const length = this.registers.get(12n) || 0n; // a2 (length)
                this.printString(addr, Number(length));
                break;
            default:
                console.error(`Unhandled syscall: ${syscall}`);
        }
        this.pc += 4n;
    }
}

// Example usage: "Hello, World!" program
const cpu = new rv32ima();

// Human-readable program
const program = [
    //"li x11 1", // x11 = 0x1000 (base address)
    //"li x12 100",
    //"sb x11 x12 0",
    ///*
    `.string 'Hello, World!'`, // Store string
    "li x17 64", // Syscall 64 (write)
    //"li x10 1", // File descriptor (stdout)
    //"li x12 14", // Length of string
    "ecall", // Print the string
    //*/
];

// Translate the program into a decimal array
const translatedProgram = rv32ima.translate(program);

// Load the program into memory
cpu.loadProgram(translatedProgram);

// Execute the program
cpu.execute();

function x(objString) {
    // Safely evaluate the string to get the object
    const obj = Function(`"use strict"; return (${objString});`)();

    // Convert BigInt values to strings
    const convertedObj = {};

    for (const key in obj) {
        if (obj.hasOwnProperty(key)) {
            const innerObj = obj[key];
            const convertedInnerObj = {};

            for (const innerKey in innerObj) {
                if (innerObj.hasOwnProperty(innerKey)) {
                    // Convert BigInt to string
                    convertedInnerObj[innerKey] =
                        "0x" + innerObj[innerKey].toString(16) + "n";
                }
            }

            convertedObj[key] = convertedInnerObj;
        }
    }

    // Serialize the converted object to a JSON string
    return JSON.stringify(convertedObj);
}

let debug = false;

if (debug) {
    console.log("Registers after execution:");
    console.log(cpu.registers); // Check the register values

    console.log("Memory after execution:");
    console.log(Object.fromEntries(cpu.memory)); // Check the memory values

    console.log("translatedProgram: ");
    console.log(
        JSON.stringify(translatedProgram.map((i) => String(Number(i)) + "n"))
    );
    let stdin = require("fs").readFileSync(0, "utf8").toString();
    //console.log(x(require("fs").readFileSync(0, "utf8").toString()));
    console.log(rv32ima.untranslate(JSON.parse(stdin).map((i) => parseInt(i))));
    console.log(rv32ima.untranslate(translatedProgram.map((i) => parseInt(i))));
    console.log(
        JSON.parse(stdin)
            .map((i) => parseInt(i))
            .toString() === translatedProgram.map((i) => parseInt(i)).toString()
    );
}
