package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for binary arithmetic expressions.
 */
public abstract class BinArithExpr extends BinExpr {

    /** Default constructor.
     */
    public BinArithExpr(Position pos, Expr left, Expr right) {
        super(pos, left, right);
    }

    /** Run type checking analysis on this expression.  The typing
     *  parameter provides access to the type analysis phase (in
     *  particular, to the associated error handler).
     */
    public Type analyze(TypeAnalysis typing) {  // Add, Sub, Mul, Div
        left.require(typing, Type.INT);
        right.require(typing, Type.INT);
        return type = Type.INT;
    }
}
