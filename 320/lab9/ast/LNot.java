package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for logical not expressions (!).
 */
public class LNot extends UnExpr {

    /** Default constructor.
     */
    public LNot(Position pos, Expr exp) {
        super(pos, exp);
    }

    /** Return a string that provides a simple description of this
     *  particular type of operator node.
     */
    String label() { return "LNot"; }

    /** Generate a pretty-printed description of this expression
     *  using the concrete syntax of the mini programming language.
     */
    public void print(TextOutput out) { unary(out, "!"); }

    /** Run type checking analysis on this expression.  The typing
     *  parameter provides access to the type analysis phase (in
     *  particular, to the associated error handler).
     */
    public Type analyze(TypeAnalysis typing) {
        return type = exp.require(typing, Type.BOOLEAN);
    }

    /** Generate assembly language code for this expression that will
     *  evaluate the expression when it is executed and leave the result
     *  in the specified free register, preserving any lower numbered
     *  registers in the process.
     */
    public void compileExpr(IA32Output a, int pushed, int free) {
        exp.compileExpr(a, pushed, free);
        a.emit("xorl", a.immed(1), a.reg(free));
    }
}
