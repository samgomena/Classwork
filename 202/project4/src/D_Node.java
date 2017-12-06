/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;

/**
 * @brief Subclass of our linear linked list class which is contained in our doubly linked list class.
 */
public class D_Node {
    private D_Node next;
    private D_Node prev;
    private String nameLiteral;
    private OrderComponent oc;

    public D_Node() {
        next = null;
        prev = null;
    }

    public D_Node(ArrayList<HashMap<String, String>> orderStuff, String restaurantOrderedFrom, String orderName, float price) {
        next = null;
        prev = null;
        nameLiteral = restaurantOrderedFrom;
        oc = getBinding(orderName, price);
    }

    public D_Node next() { return next; }
    public void next(D_Node new_next) { next = new_next; }
    public D_Node prev() { return prev; }
    public void prev(D_Node new_prev) { prev = new_prev; }

    /**
     * @brief Wrapper function for calling our parents display function.
     *
     * Note: I realize this is very poor design.
     * @return int the number of items in our parents list.
     */
    public boolean display() {
        return oc.display();
    }

    private static OrderComponent getBinding(String name, float price) {
        Calendar cal = Calendar.getInstance();
        int hour = cal.get(Calendar.HOUR_OF_DAY);
//        Random rnd = new Random(); // TODO: retrieve tf rid of this
//        hour = rnd.nextInt(23);
        switch(hour) {
            case 5:
            case 6:
            case 7:     // 5AM - 10AM
            case 8:
            case 9:
            case 10:
                return new Breakfast(name, price);
            case 11:
            case 12:
            case 13:    // 11AM - 3PM
            case 14:
            case 15:
                return new Lunch(name, price);
            case 16:
            case 17:
            case 18:
            case 19:    // 4PM - 10PM
            case 20:
            case 21:
            case 22:
                return new Dinner(name, price);
            default:
                return new Dinner(name, price); // Why not?
        }
    }
}
