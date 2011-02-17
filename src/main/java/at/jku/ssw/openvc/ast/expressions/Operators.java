package at.jku.ssw.openvc.ast.expressions;

public class Operators {

    public static enum Term {
        MUL, DIV, MOD, REM
    }

    public static enum Relation {
        EQ, NEQ, LT, LEQ, GT, GEQ
    }

    public static enum Shift {
        SLL, SRL, SLA, SRA, ROL, ROR
    }

    public static enum Factor {
        POW, ABS, NOT
    }

    public static enum Logical {
        AND, NAND, OR, NOR, XOR, XNOR
    }

    public static enum Add {
        PLUS, MINUS, AMPERSAND
    }

    public static enum Sign {
        PLUS, MINUS
    }
}
