package ast;
import compiler.Failure;
import compiler.Position;

/** Abstract syntax for  basic types.
 */
public class Type {

    String typename;

    /** Default constructor.
     */
    private Type(String typename) {
        this.typename = typename;
    }

    /** Represents the type of integers.
     */
    public static final Type INT = new Type("int");

    /** Represents the type of booleans.
     */
    public static final Type BOOLEAN = new Type("boolean");

    /** Generate a printable name for this type.
     */
    public String toString() {
        return typename;
    }

    /** Output a description of this node (with id n), including a
     *  link to its parent node (with id p) and returning the next
     *  available node id.
     */
    public int toDot(DotOutput dot, int p, String attr, int n) {
        dot.join(p, n, attr);
        return toDot(dot, n);
    }

    /** Output a description of this node (with id n) in dot format,
     *  adding an extra node for each subtree.
     */
    public int toDot(DotOutput dot, int n) {
        return dot.node(typename,n);
    }

    /** Return a string that assigns a dot color for the type of this node.
     */
    public static String color(Type type) {
        return (type==Type.INT)     ? "lemonchiffon"
             : (type==Type.BOOLEAN) ? "plum"
             :                        "lightsalmon";
    }
}
