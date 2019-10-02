package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for binary logical expressions.
 */
public abstract class BinLogicExpr extends BinExpr {

    /** Default constructor.
     */
    public BinLogicExpr(Position pos, Expr left, Expr right) {
        super(pos, left, right);
    }

    /** Run type checking analysis on this expression.  The typing
     *  parameter provides access to the type analysis phase (in
     *  particular, to the associated error handler).
     */
    public Type analyze(TypeAnalysis typing) {  // LAnd, LOr
        left.require(typing, Type.BOOLEAN);
        right.require(typing, Type.BOOLEAN);
        return type = Type.BOOLEAN;
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
        // return right.analyze(init, left.analyze(init, initialized));
        VarSet li = left.analyze(init, initialized);
        right.analyze(init, li);
        return li;
    }
}
