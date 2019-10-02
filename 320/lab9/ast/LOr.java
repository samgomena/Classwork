package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for logical or expressions (||).
 */
public class LOr extends BinLogicExpr {

    /** Default constructor.
     */
    public LOr(Position pos, Expr left, Expr right) {
        super(pos, left, right);
    }

    /** Return a string that provides a simple description of this
     *  particular type of operator node.
     */
    String label() { return "LOr"; }

    /** Generate a pretty-printed description of this expression
     *  using the concrete syntax of the mini programming language.
     */
    public void print(TextOutput out) { binary(out, "||"); }

    /** Generate assembly language code for this expression that will
     *  evaluate the expression when it is executed and leave the result
     *  in the specified free register, preserving any lower numbered
     *  registers in the process.
     */
    public void compileExpr(IA32Output a, int pushed, int free) {
        String lab = a.newLabel();
        left.compileExpr(a, pushed, free);
        a.emit("orl", a.reg(free), a.reg(free));
        a.emit("jnz",  lab);
        right.compileExpr(a, pushed, free);
        a.emitLabel(lab);
      }

    /** Generate code that will evaluate this (boolean-valued) expression
     *  and jump to the specified label if the result is true.
     */
    void branchTrue(IA32Output a, int pushed, int free, String lab) {
        left.branchTrue(a, pushed, free, lab);
        right.branchTrue(a, pushed, free, lab);
    }

    /** Generate code that will evaluate this (boolean-valued) expression
     *  and jump to the specified label if the result is false.
     */
    void branchFalse(IA32Output a, int pushed, int free, String lab) {
        String lab1 = a.newLabel();
        left.branchTrue(a, pushed, free, lab1);
        right.branchFalse(a, pushed, free, lab);
        a.emitLabel(lab1);
    }
}
