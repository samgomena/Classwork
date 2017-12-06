import java.util.TreeSet;
import java.util.Random;

public class Node {
    protected Node left;
    protected Node right;
    protected Node parent;
    private static final boolean RED = true;
    private static final boolean BLACK = false;
    private String restaurantName;
    private String driverDistances;
    private TreeSet ts;
    private boolean color;

    Node(String restName) {
        Random rnd = new Random();
        left = null;
        right = null;
        parent = null;
        restaurantName = Menu.parseRestaurantName(restName)[0];
        driverDistances = Menu.parseRestaurantName(restName)[1];
        color = BLACK;
        ts = new TreeSet();
        // Setup outside of initialization
        String[] minMaxDistance = driverDistances.split("-");
        final int MIN = Integer.parseInt(minMaxDistance[0]);
        final int MAX = Integer.parseInt(minMaxDistance[1]);
        int numDrivers = rnd.nextInt(25) + 5; // Set random number of drivers around driverDistances for 5 < drivers < 25
        for(int i = 0; i < numDrivers; ++i) {
            ts.add(rnd.nextInt((MAX - MIN) + 1) + MIN); // Add a driver that is n minutes away for min < n < max
        }
    }

    public Node parent() {
        return this.parent;
    }

    public Node grandparent() {
        if(this.parent == null) {
            return null;
        }
        return this.parent.parent;
    }

    public String data() {
        return this.restaurantName;
    }

    public void data(String newName) {
        this.restaurantName = newName;
    }

    public boolean color(boolean _color) {
        return color = _color;
    }

    public boolean isRed() {
        return this.color == RED;
    }

    public boolean isBlack() {
        return this.color == BLACK;
    }

    public void display() {
        System.out.println("Drivers are " + ts.toString() + " minutes away from " + restaurantName + ".");
    }
}