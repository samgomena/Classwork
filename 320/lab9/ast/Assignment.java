package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for assignment expressions.
 */
public class Assignment extends Expr {

    /** The variable where the result will be saved.
     */
    private Id lhs;

    /** The expression whose value will be saved.
     */
    private Expr rhs;

    /** Default constructor.
     */
    public Assignment(Position pos, Id lhs, Expr rhs) {
        super(pos);
        this.lhs = lhs;
        this.rhs = rhs;
        depth    = 1 + rhs.getDepth(); // Compute the depth of this expression
    }

    /** Print an indented description of this abstract syntax node,
     *  including a name for the node itself at the specified level
     *  of indentation, plus more deeply indented descriptions of
     *  any child nodes.
     */
    public void indent(IndentOutput out, int n) {
        out.indent(n, "Assignment");
        lhs.indent(out, n+1);
        rhs.indent(out, n+1);
    }

    /** Generate a pretty-printed description of this abstract syntax
     *  node using the concrete syntax of the mini programming language.
     */
    public void print(TextOutput out) {
        lhs.print(out);
        out.print(" = ");
        rhs.parenPrint(out);
    }

    /** Output a description of this node (with id n) in dot format,
     *  adding an extra node for each subtree.
     */
    public int toDot(DotOutput dot, int n) {
        return rhs.toDot(dot, n, "rhs",
               lhs.toDot(dot, n, "lhs",
               node(dot, "Assignment", n)));
    }

    /** Run scope analysis on this expression.  The scoping parameter
     *  provides access to the scope analysis phase (in particular,
     *  to the associated error handler), and the env parameter
     *  reflects the environment in which the expression is evaluated.
     *  Unlike scope analysis for statements, there is no return
     *  result here: an expression cannot introduce new variables in
     *  to a program, so the final environment will always be the same
     *  as the initial environment.
     */
    public void analyze(ScopeAnalysis scoping, Env env) {
        lhs.analyze(scoping, env);
        rhs.analyze(scoping, env);
    }

    /** Run type checking analysis on this expression.  The typing
     *  parameter provides access to the type analysis phase (in
     *  particular, to the associated error handler).
     */
    public Type analyze(TypeAnalysis typing) {
        Type lt = lhs.analyze(typing);
        Type rt = rhs.analyze(typing);
        if (lt!=rt) {
          typing.report(new Failure(pos, "Attempt to assign " + rt +
                                         " value to variable of type " + lt));
        }
        return type = rt;
    }

    /** Run initialization analysis on this expression.  The init parameter
     *  provides access to the initialization analysis phase (in particular,
     *  to the associated error handler), and the initialized parameter
     *  reflects the set of variables that are known to have been initialized
     *  before this expression is evaluated.  The return result is the set of
     *  variables that are known to be initialized after the expression has
     *  been evaluated.
     */
    public VarSet analyze(InitAnalysis init, VarSet initialized) {
        return lhs.addTo(rhs.analyze(init, initialized));
    }

    /** Rewrite this expression using algebraic identities to reduce
     *  the amount of computation that is required at runtime.
     */
    Expr simplify() {
        rhs = rhs.simplify();
        return this;
    }

    /** Return the depth of this expression as a measure of how complicated
     *  the expression is / how many registers will be needed to evaluate it.
     */
    int getDepth() {
        // Return the depth value that was computed by the constructor
        return depth;
    }

    /** Records the depth of this expression; this value is computed
     *  at the time the constructor is called and then saved here so
     *  that it can be accessed without further computation later on.
     */
    protected int depth;

    /** Generate assembly language code for this expression that will
     *  evaluate the expression when it is executed and leave the result
     *  in the specified free register, preserving any lower numbered
     *  registers in the process.
     */
    public void compileExpr(IA32Output a, int pushed, int free) {
        rhs.compileExpr(a, pushed, free);
        a.emit("movl", a.reg(free), lhs.fromStackFrame(a, pushed));
    }
}
