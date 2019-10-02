package ast;
import compiler.Failure;
import compiler.Position;

/** Represents a linked list of identifiers.
 */
class IdList {

    /** The first identifier in this list.
     */
    Id head;

    /** The list of all other identifiers in this list.
     */
    IdList rest;

    /** Default constructor.
     */
    IdList(Id head, IdList rest) {
        this.head = head;
        this.rest = rest;
    }
}
