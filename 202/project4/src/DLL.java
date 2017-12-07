/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

public class DLL {
    private D_Node head;
    private D_Node tail;
    private int orderNums;

    public DLL() {
        head = null;
        tail = null;
        orderNums = 0;
    }

    /**
     * The interface to main, takes in the order details and delegates their addition to the list.
     *
     * @param restaurantOrderedFrom: String the name of the restaurant ordered from.
     * @param orderedItem: String the item that the user ordered.
     * @param price: float the price of the item the user ordered.
     * @return: boolean Whether or not `addFront(...)` was able to insert the node.
     */
    public boolean addOrder(String restaurantOrderedFrom, String orderedItem, float price) {
        return addFront(new D_Node(restaurantOrderedFrom, orderedItem, price));
    }

    /**
     * Wrapper function for private display function.
     * @return int the number of items in the list.
     */
    public int display() {
        if(head == null) {
            return 0;
        }
        return display(head);
    }

    public boolean removeAll() {
        head = null;
        tail = null;
        return true;
    }

    public int getOrderNums() {
        return orderNums;
    }

    public boolean hasOrders() { return orderNums > 0; }

    public float orderTotal() {
        return orderTotal(head);
    }

    private float orderTotal(D_Node curr) {
        if(curr == null) {
            return 0;
        }
        return curr.orderPrice() + orderTotal(curr.next());
    }

    /**
     * Adds a node to the front of the list.
     *
     * Note: Increments `order_num` for every node added.
     * Note: This function will always return true.
     * @param new_node: D_Node the node to insert to the front of the list.
     * @return boolean: Whether or not we were able to insert a node.
     */
    private boolean addFront(D_Node new_node) {
        if(head == null) {
            head = new_node;
            tail = new_node;
            ++orderNums;
            return true;
        }
        new_node.next(head);
        head.prev(new_node);
        head = new_node;
        ++orderNums;
        return true;
    }

    protected int display(D_Node curr) {
        if(curr == null) {
            return 0;
        }
        curr.display(); // Call D_Node classes display.
        return display(curr.next()) + 1;
    }
}
