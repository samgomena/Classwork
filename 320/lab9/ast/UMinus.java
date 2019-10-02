package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for unary minus expressions.
 */
public class UMinus extends UnArithExpr {

    /** Default constructor.
     */
    public UMinus(Position pos, Expr exp) {
        super(pos, exp);
    }

    /** Return a string that provides a simple description of this
     *  particular type of operator node.
     */
    String label() { return "UMinus"; }

    /** Generate a pretty-printed description of this expression
     *  using the concrete syntax of the mini programming language.
     */
    public void print(TextOutput out) { unary(out, "-"); }

    /** Constant folding for unary operators with a known integer
     *  argument.
     */
    Expr fold(int n) { return new IntLit(pos, -n); }

    /** Generate assembly language code for this expression that will
     *  evaluate the expression when it is executed and leave the result
     *  in the specified free register, preserving any lower numbered
     *  registers in the process.
     */
    public void compileExpr(IA32Output a, int pushed, int free) {
        exp.compileExpr(a, pushed, free);
        a.emit("negl", a.reg(free));
    }
}
