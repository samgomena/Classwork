/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

import java.util.TreeSet;
import java.util.Random;


public class Node {
    protected Node left, right, parent;
    private boolean color;
    private static final boolean RED = true, BLACK = false;
    private String restaurantName, driverDistances;
    protected DLL restaurantOrders;
    private TreeSet<Integer> driversAvailable;

    /**
     * This initializes the nodes references to null. As well it sets up the tree of drivers randomly.
     *
     * Note: this contains a built in self balancing tree, namely, `TreeSet` this is due to the fact that containing
     * the tree I built would have resulted in an infinite loop of Tree's containing Node's containing Tree's
     * containing Node's containing etc.
     * @param restName: String the name of the restaurant we want to start tracking orders for.
     */
    Node(String restName) {
        Random rnd = new Random();
        left = right = parent = null;
        color = BLACK;
        restaurantName = restName;
        driverDistances = Menu.parseRestaurantName(restName)[1];
        restaurantOrders = new DLL();
        driversAvailable = new TreeSet<>();
        // Setup outside of initialization
        String[] minMaxDistance = driverDistances.split("-");
        final int MIN = Integer.parseInt(minMaxDistance[0]);
        final int MAX = Integer.parseInt(minMaxDistance[1]);
        int numDrivers = rnd.nextInt(25) + 5; // Set random number of drivers around driverDistances where 5 < drivers < 25
        for(int i = 0; i < numDrivers; ++i) {
            driversAvailable.add(rnd.nextInt((MAX - MIN) + 1) + MIN); // Add a driver that is n minutes away where min < n < max
        }
    }

    public Node parent() { return this.parent; }
    public Node grandparent() { return this.parent == null ? null : this.parent.parent; }

    public String name() { return this.restaurantName; }

    public boolean color(boolean _color) { return color = _color; }
    public boolean isRed() { return this.color == RED; }
    public boolean isBlack() { return this.color == BLACK; }

    public void display() {
        if(restaurantOrders.hasOrders()) {
            Menu.print("Orders from " + Menu.parseRestaurantName(restaurantName)[0] + ":\n");
            restaurantOrders.display();
            Menu.print("\n\tThere are " + driversAvailable.size() + " available drivers right now.\n\tThe closest one is " + driversAvailable.first() + " minutes away.\n");
        }
    }

    public boolean addOrder(String restaurantOrderedFrom, String orderedItem, float price) {
        return restaurantOrders.addOrder(restaurantOrderedFrom, orderedItem, price);
    }

    public int getOrderSize() {
        return restaurantOrders.getOrderNums();
    }

    public float getOrderTotal() { return restaurantOrders.orderTotal(); }

    public boolean removeAll() {
        driversAvailable.clear();
        return restaurantOrders.removeAll();
    }
}