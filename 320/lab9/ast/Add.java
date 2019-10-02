package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for add expressions.
 */
public class Add extends BinArithExpr {

    /** Default constructor.
     */
    public Add(Position pos, Expr left, Expr right) {
        super(pos, left, right);
    }

    /** Return a string that provides a simple description of this
     *  particular type of operator node.
     */
    String label() { return "Add"; }

    /** Generate a pretty-printed description of this expression
     *  using the concrete syntax of the mini programming language.
     */
    public void print(TextOutput out) { binary(out, "+"); }

    /** Constant folding for binary operators with two known integer
     *  arguments.
     */
    Expr fold(int n, int m) { return new IntLit(pos, n+m); }

    /** Simplification of a binary expression when the left operand
     *  (but not the right) is a known integer constant.
     */
    Expr simpL(int m) {
        // Commutative operator: swap operands and then use simpR:
        Expr temp = left; left = right; right = temp;
        return simpR(m);
    }

    /** Simplification of a binary expression when the right operand
     *  (but not the left) is a known integer constant.
     */
    Expr simpR(int m) { return left.simpAdd(this, m);  }

    /** Simplify an addition with a known integer as the right argument.
     */
    Expr simpAdd(Add orig, int m) {
        IntLit rightInt = right.isIntLit();
        return (rightInt==null)
                ? orig
                : left.newAdd(orig.pos, m + rightInt.getNum());
    }

    /** Generate assembly language code for this expression that will
     *  evaluate the expression when it is executed and leave the result
     *  in the specified free register, preserving any lower numbered
     *  registers in the process.
     */
    public void compileExpr(IA32Output a, int pushed, int free) {
        compileCommutativeOp(a, "addl", pushed, free);
    }
}
