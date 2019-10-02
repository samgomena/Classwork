package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for unary expressions that operate on
 *  numeric arguments.
 */
public abstract class UnArithExpr extends UnExpr {

    /** Default constructor.
     */
    public UnArithExpr(Position pos, Expr exp) {
        super(pos, exp);
    }

    /** Run type checking analysis on this expression.  The typing
     *  parameter provides access to the type analysis phase (in
     *  particular, to the associated error handler).
     */
    public Type analyze(TypeAnalysis typing) {
        // return type = exp.analyze(typing);
        return type = exp.require(typing, Type.INT);
    }
}
