package ast;
import compiler.Failure;
import compiler.Position;
import compiler.Handler;
import compiler.Phase;

/** Represents a static analysis phase that performs type checking, assuming
 *  a previous (and successful) use of scope analysis.
 */
public class TypeAnalysis extends Phase {

    /** Default constructor.
     */
    public TypeAnalysis(Handler handler) {
        super(handler);
    }

    /** Run type analysis/checking on the specified statement, assuming an
     *  empty initial environment.
     */
    public void analyze(Stmt stmt)
      throws Failure {
        stmt.analyze(this);
        if (getHandler().hasFailures()) {
            throw new Failure("Aborting: errors detected during type checking");
        }
    }
}
