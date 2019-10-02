package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for binary equality test expressions.
 */
public abstract class BinEqualityExpr extends BinExpr {

    /** Default constructor.
     */
    public BinEqualityExpr(Position pos, Expr left, Expr right) {
        super(pos, left, right);
    }

    /** Run type checking analysis on this expression.  The typing
     *  parameter provides access to the type analysis phase (in
     *  particular, to the associated error handler).
     */
    public Type analyze(TypeAnalysis typing) {   // Eql, Neq
        Type lt = left.require(typing, Type.INT, Type.BOOLEAN);
        Type rt = right.require(typing, Type.INT, Type.BOOLEAN);
        requireSame(typing, lt, rt);
        return type = Type.BOOLEAN;
    }
}
