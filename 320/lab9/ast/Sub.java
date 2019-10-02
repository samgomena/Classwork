package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for subtract expressions.
 */
public class Sub extends BinArithExpr {

    /** Default constructor.
     */
    public Sub(Position pos, Expr left, Expr right) {
        super(pos, left, right);
    }

    /** Return a string that provides a simple description of this
     *  particular type of operator node.
     */
    String label() { return "Sub"; }

    /** Generate a pretty-printed description of this expression
     *  using the concrete syntax of the mini programming language.
     */
    public void print(TextOutput out) { binary(out, "-"); }

    /** Constant folding for binary operators with two known integer
     *  arguments.
     */
    Expr fold(int n, int m) { return new IntLit(pos, n-m); }

    /** Generate assembly language code for this expression that will
     *  evaluate the expression when it is executed and leave the result
     *  in the specified free register, preserving any lower numbered
     *  registers in the process.
     */
    public void compileExpr(IA32Output a, int pushed, int free) {
        compileOp(a, "subl", pushed, free);
    }
}
