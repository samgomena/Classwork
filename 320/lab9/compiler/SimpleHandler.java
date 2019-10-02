package compiler;

/** A simple implementation of the Handler interface that prints the
 *  position and description of each diagnostic on System.err, and
 *  then returns to the caller.
 */
public class SimpleHandler extends Handler {
    /** Respond to a diagnostic by displaying it on the error output
     *  stream.
     */
    protected void respondTo(Diagnostic diagnostic) {
        if (diagnostic instanceof Warning) {
            System.err.print("WARNING: ");
        } else {
            System.err.print("ERROR: ");
        }
        Position pos = diagnostic.getPos();
        if (pos!=null) {
            System.err.println(pos.describe());
        }
        System.err.println(diagnostic.getText());
    }
}
