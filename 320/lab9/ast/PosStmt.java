package ast;
import compiler.Failure;
import compiler.Position;

/** Statements that are annotated with positions.
 */
public abstract class PosStmt extends Stmt {

    protected Position pos;

    /** Default constructor.
     */
    public PosStmt(Position pos) {
        this.pos = pos;
    }

    /** Return a string describing the position/coordinates
     *  of this abstract syntax tree node.
     */
    String coordString() { return pos.coordString(); }

    /** Output a dot description of this abstract syntax node
     *  using the specified label and id number.
     */
    protected int node(DotOutput dot, String lab, int n) {
        return dot.node(lab + "\\n" + pos.coordString(), n);
    }
}
