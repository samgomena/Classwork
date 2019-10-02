package ast;
import compiler.Failure;
import compiler.Position;
import compiler.Handler;
import compiler.Phase;

/** Represents a static analysis phase that associates each identifier
 *  in a program with a corresponding environment entry, and reports
 *  an error for any variable that is used without a preceding declaration.
 */
public class ScopeAnalysis extends Phase {

    /** Default constructor.
     */
    public ScopeAnalysis(Handler handler) {
        super(handler);
    }

    /** Run scope analysis on the specified statement, assuming an
     *  empty initial environment.
     */
    public void analyze(Stmt stmt)
      throws Failure {
        stmt.analyze(this, null);
        if (getHandler().hasFailures()) {
            throw new Failure("Aborting: errors detected during scope analysis");
        }
    }
}
