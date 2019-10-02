package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for less than or equal expressions.
 */
public class Lte extends BinCompExpr {

    /** Default constructor.
     */
    public Lte(Position pos, Expr left, Expr right) {
        super(pos, left, right);
    }

    /** Return a string that provides a simple description of this
     *  particular type of operator node.
     */
    String label() { return "Lte"; }

    /** Generate a pretty-printed description of this expression
     *  using the concrete syntax of the mini programming language.
     */
    public void print(TextOutput out) { binary(out, "<="); }

    /** Constant folding for binary operators with two known integer
     *  arguments.
     */
    Expr fold(int n, int m) { return new BoolLit(pos, n<=m); }

    /** Generate assembly language code for this expression that will
     *  evaluate the expression when it is executed and leave the result
     *  in the specified free register, preserving any lower numbered
     *  registers in the process.
     */
    public void compileExpr(IA32Output a, int pushed, int free) {
        compileCondValue(a, "jle", pushed, free);
    }

    /** Generate code that will evaluate this (boolean-valued) expression
     *  and jump to the specified label if the result is true.
     */
    void branchTrue(IA32Output a, int pushed, int free, String lab) {
        compileCond(a, pushed, free);
        a.emit("jle", lab);
    }

    /** Generate code that will evaluate this (boolean-valued) expression
     *  and jump to the specified label if the result is false.
     */
    void branchFalse(IA32Output a, int pushed, int free, String lab) {
        compileCond(a, pushed, free);
        a.emit("jg", lab);
    }
}
