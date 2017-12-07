import java.util.ArrayList;
import java.util.HashMap;

public class Tree {

    private Node root;
    private static final boolean RED = true;
    private static final boolean BLACK = false;

    public Tree() {
        root = null;
    }

    public Tree(HashMap<String, ArrayList<HashMap<String, String>>> initData) {
        root = null;
        boolean error = false;
        for(String restName: initData.keySet()) {
            error |= !insert(restName); // If insert ever fails `error` is set to true [error = false || !insert(...)]
        }
        // If we encountered an error entering data, lets quit and try again.
        if(error) {
            System.out.println("There was an error parsing the restaurant name.\nPlease try running again.");
            System.exit(0);
        }
    }

    public boolean insert(String restaurantName) {
        Node new_node = new Node(restaurantName);
        root = insert(root, new_node);
        return refactorAfterInsertion(new_node);
    }

    public Node retrieve(String data) { return retrieve(root, data); }

    public String retrieveAll(String data) { return retrieveAll(root, data); }

    public float getPriceOfAllOrders() { return getPriceOfAllOrders(root); }
    public int getNumberOfAllOrders() { return getNumberOfAllOrders(root); }

    public int height() { return root == null ? 0 : height(root); }

    public int display() { return root == null ? 0 : display(root); }

    public boolean removeAll() {
        boolean removed = removeAll(root);
        root = null;
        return removed;
    }

    protected Node insert(Node root, Node new_node) {
        if(root == null) {
            return new_node;
        } else if (new_node.name().compareTo(root.name()) < 1) {
            Node leftChild = insert(root.left, new_node);
            root.left = leftChild;
            leftChild.parent = root;
        } else {
            Node rightChild = insert(root.right, new_node);
            root.right = rightChild;
            rightChild.parent = root;
        }
        return root;
    }

    protected Node retrieve(Node root, String key) {
        if(root == null) {
            return root;
        }
        else if(key.equals(root.name())) {
            return root;
        }
        else if(key.compareTo(root.name()) < 0) {
            return retrieve(root.left, key);
        } else if(key.compareTo(root.name()) >= 0) {
            return retrieve(root.right, key);
        }
        return root;
    }

    /**
     * This is a poorly named function that really just returns the total price of all the orders. : /
     * @param root
     * @param key
     * @return
     */
    protected String retrieveAll(Node root, String key) {
        if(root == null) {
            return "";
        } else if(key.equals(root.name())) {
            return root.name();
        } else if(key.compareTo(root.name()) < 0) {
            return retrieveAll(root.left, key);
        } else if (key.compareTo(root.name()) >= 0) {
            return retrieveAll(root.right, key);
        }
        return root.name();
    }

    protected float getPriceOfAllOrders(Node root) {
        if(root == null) {
            return 0;
        }
        return root.getOrderTotal() + getPriceOfAllOrders(root.left) + getPriceOfAllOrders(root.right);
    }

    protected int getNumberOfAllOrders(Node root) {
        return root == null ? 0 : root.getOrderSize() + getNumberOfAllOrders(root.left) + getNumberOfAllOrders(root.right);
    }

    protected int height(Node root) {
        if(root == null) {
            return 0;
        }
        int left = height(root.left) + 1;
        int right = height(root.right) + 1;
        return left > right ? left : right;
    }

    protected int display(Node root) {
        if(root == null) {
            return 0;
        }
        int count = display(root.left) + 1;
//        System.out.print(root.data() + ": "); TODO: Maybe remove this.
        root.display(); // Call display on node in tree.
        return count + display(root.right);
    }

    private boolean refactorAfterInsertion(Node newNode) {
        boolean error = false;
        Node temp;
        newNode.color(RED);
        while(newNode.parent() != null && newNode.parent().isRed()) {
            if(newNode.parent() == newNode.grandparent().left) {
                temp = newNode.grandparent().right; // Uncle of node added on left sub-tree
                if(temp != null && temp.isRed()) { // If Uncle is red -> recolor and keep refactoring
                    newNode.parent().color(BLACK);
                    temp.color(BLACK); // Color parent and uncle black
                    newNode.grandparent().color(RED); // Make grandparent red
                    newNode = newNode.grandparent(); // Move up the tree (incremental step)
                } else { // If uncle is black
                    if(newNode == newNode.parent().right) { // We added on a right sub-tree
                        newNode = newNode.parent(); // Move up a level to perform rotation
                        error |= !rotateLeft(newNode); // Leaning right so rotate left
                    }
                    newNode.parent().color(BLACK);
                    newNode.grandparent().color(RED);
                    error |= !rotateRight(newNode.grandparent()); // Rotate right to adjust height. If rotation fails update error
                }
            } else { // Same operations performed as above but adjust operations for use on right sub-tree
                temp = newNode.grandparent().left; // Uncle of node added on right sub-tree
                if(temp != null && temp.isRed()) { // Again if uncle is red -> recolor and keep refactoring
                    newNode.parent().color(BLACK);
                    temp.color(BLACK); // Color parent and uncle black
                    newNode.grandparent().color(RED); // Make grandparent red
                    newNode = newNode.grandparent(); // Move up the tree (incremental step)
                } else {
                    if(newNode == newNode.parent().left) { // We added on a left sub-tree
                        newNode = newNode.parent(); // Move up a level to perform rotation
                        error |= !rotateRight(newNode); // Leaning left so rotate right
                    }
                    newNode.parent().color(BLACK);
                    newNode.grandparent().color(RED);
                    error |= !rotateLeft(newNode.grandparent()); // Rotate left to adjust height
                }
            }
        }
        root.color(BLACK); // Root is always black. (Essentially our base case)
        return !error;
    }

    private boolean rotateRight(Node rotateAbout) {
        int subTreeHeight = height(rotateAbout); // Used for veracity check
        Node subTree = rotateAbout.left; // Grab left sub-tree
        rotateAbout.left = subTree.right; // Update left sub-tree of rotating node to inorder predecessor
        if(rotateAbout.left != null) {
            subTree.right.parent = rotateAbout; // Update parent of inorder predecessor
        }
        subTree.parent = rotateAbout.parent();  // ---------
        subTree.right = rotateAbout;            // Move left leaning sub-tree up i.e. rotate
        rotateAbout.parent = subTree;           // ---------
        if(root == rotateAbout) { // Handle rotations around root
            root = subTree;
        } else { // We're in a sub-tree
            if(subTree.parent().left == rotateAbout) { // Update parents left/right links
                subTree.parent().left = subTree;
            } else {
                subTree.parent().right = subTree;
            }
        }
        return subTreeHeight - height(subTree) <= 1; // True if difference in height is at most 1; false otherwise.
    }

    private boolean rotateLeft(Node rotateAbout) {
        int subTreeHeight = height(rotateAbout);
        Node subTree = rotateAbout.right;
        rotateAbout.right = subTree.left;
        if(subTree.left != null) {
            subTree.left.parent = rotateAbout; // Update parent of inorder predecessor
        }
        subTree.parent = rotateAbout.parent();  // ---------
        subTree.left = rotateAbout;             // Move right leaning sub-tree up i.e. rotate
        rotateAbout.parent = subTree;           // ---------
        if(root == rotateAbout) { // Handle rotations around root
            root = subTree;
        } else { // We're in a sub-tree
            if(subTree.parent().left == rotateAbout) { // Update parents left/right link
                subTree.parent().left = subTree;
            } else {
                subTree.parent().right = subTree;
            }
        }
        return subTreeHeight - height(subTree) <= 1; // True if difference in height is at most 1; false otherwise.
    }

    private boolean removeAll(Node root) {
        if(root == null) {
            return true;
        }
        removeAll(root.left);
        removeAll(root.right);
        return root.removeAll();
    }
}