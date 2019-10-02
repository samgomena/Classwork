package ast;
import compiler.Failure;
import compiler.Position;
import compiler.Handler;
import compiler.Phase;

/** Represents a static analysis phase that performs initialization
 *  analysis, assuming a previous (and successful) use of scope analysis.
 */
public class InitAnalysis extends Phase {

    /** Default constructor.
     */
    public InitAnalysis(Handler handler) {
        super(handler);
    }

    /** Run initialization analysis on the specified statement, assuming
     *  an empty set of already initialized variables.
     */
    public void analyze(Stmt stmt)
      throws Failure {
        stmt.analyze(this, null);
        if (getHandler().hasFailures()) {
            throw new Failure("Aborting: errors detected during initialization analysis");
        }
    }
}
