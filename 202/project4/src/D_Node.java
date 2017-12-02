/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

import java.util.ArrayList;
import java.util.HashMap;

/**
 * @brief Subclass of our linear linked list class which is contained in our doubly linked list class.
 */
public class D_Node extends LLL {
    private D_Node next;
    private D_Node prev;
    private String nameLiteral;

    public D_Node() {
        super();
        next = null;
        prev = null;
    }

    public D_Node(ArrayList<HashMap<String, String>> orderStuff, String restaurantOrderedFrom, String orderName, float price) {
        super();
        next = null;
        prev = null;
        nameLiteral = restaurantOrderedFrom;
        add(orderStuff, orderName, price);
    }

    public D_Node next() {
        return next;
    }
    public void next(D_Node new_next) {
        next = new_next;
    }
    public D_Node prev() {
        return prev;
    }
    public void prev(D_Node new_prev) {
        prev = new_prev;
    }

    /**
     * @brief Wrapper function for calling our parents display function.
     *
     * Note: I realize this is very poor design.
     * @return int the number of items in our parents list.
     */
    public int display() {
        return super.display();
    }
}
