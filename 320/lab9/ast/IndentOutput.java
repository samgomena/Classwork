package ast;
import compiler.Failure;
import compiler.Position;

/** Represents an output phase for producing textual output of
 *  abstract syntax trees using indentation.
 */
public class IndentOutput {

    private java.io.PrintStream out;

    /** Default constructor.
     */
    public IndentOutput(java.io.PrintStream out) {
        this.out = out;
    }

    /** Output an indented description of the abstract syntax
     *  tree for the given statement.
     */
    public void indent(Stmt stmt) {
        stmt.indent(this, 0);
    }

    /** Print a given String message indented some number of
     *  spaces (currently two times the given nesting level, n).
     */
    public void indent(int n, String msg) {
        for (int i=0; i<n; i++) {
            out.print("  ");
        }
        out.println(msg);
    }
}
