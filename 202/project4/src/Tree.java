/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

import java.util.ArrayList;
import java.util.HashMap;

public class Tree {

    private Node root;
    private static final boolean RED = true;
    private static final boolean BLACK = false;

    public Tree() {
        root = null;
    }

    /**
     * This constructor consumes data returned by the `readData()` function in the `Menu` class.
     *
     * Note: Construction of the tree will fail and the program will exit if there is an error encountered while
     * inserting data into the tree.
     * @param initData: HashMap<String, ArrayList<HashMap<String, String>>>> pseudo-serialized data from the data file.
     */
    public Tree(HashMap<String, ArrayList<HashMap<String, String>>> initData) {
        root = null;
        boolean error = false;
        for(String restName: initData.keySet()) {
            error |= !insert(restName); // If insert ever fails `error` is set to true [error = false || !insert(...)]
        }
        // If we encountered an error entering data, lets quit and try again.
        if(error) {
            System.out.println("There was an error parsing the restaurant data.\nPlease try running again.");
            System.exit(0);
        }
    }

    /**
     * Public wrapper function that calls the private insert function and fixes up the tree after insertion.
     *
     * @param restaurantName: String The raw name of the restaurant we are adding to the tree
     * @return boolean Value returned by `refactorAfterInsertion(...)` on whether the tree retained its balanced tree properties.
     */
    public boolean insert(String restaurantName) {
        Node new_node = new Node(restaurantName);
        root = insert(root, new_node);
        return refactorAfterInsertion(new_node);
    }

    /**
     * Public wrapper function that retrieves an item from the tree.
     *
     * Note: this method will return null if the item is not found.
     * @param data: String the item we want to search for. A restaurant name is the only useful value here.
     * @return Node the node that contains data matching `data`.
     */
    public Node retrieve(String data) { return retrieve(root, data); }

    /**
     * Public wrapper function that retrieves similar items from the tree.
     *
     * Note: this method will return null if the item is not found.
     * @param data: String the item we want to search for. A restaurant name is the only useful value here.
     * @return Node the node that contains data matching `data`.
     */
    public Node retrieveAll(String data) { return retrieveAll(root, data); }

    public float getPriceOfAllOrders() { return getPriceOfAllOrders(root); }
    public int getNumberOfAllOrders() { return getNumberOfAllOrders(root); }

    public int height() { return root == null ? 0 : height(root); }

    public int display() { return root == null ? 0 : display(root); }

    /**
     * Public wrapper function for call to recursive `removeAll(...)` function.
     *
     * @return boolean Whether we removed all the nodes from the tree.
     */
    public boolean removeAll() {
        boolean removed = removeAll(root);
        root = null;
        return removed;
    }

    /**
     * Protected insert function which traverses the tree inserting data inorder. This function differs from a regular
     * binary search tree insertion method by adding support of parents i.e. every node keeps track of its parent in the
     * tree. This is required to be able to balance the tree.
     *
     * @param root: Node the root of the tree.
     * @param new_node: Node the node we want to add to the tree.
     * @return Node a reference to the updated tree with new_node added to it.
     */
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

    /**
     * Protected retrieve functions which searches the tree looking for a node that contains data matching `key`.
     * If no node contains `key` null is returned.
     *
     * @param root: Node the root of the tree.
     * @param key: String a string of data for which to search for.
     * @return Node the node with the matching data or null.
     */
    protected Node retrieve(Node root, String key) {
        if(root == null) {
            return root;
        } else if(key.equals(root.name())) {
            return root;
        } else if(key.compareTo(root.name()) < 0) {
            return retrieve(root.left, key);
        } else if(key.compareTo(root.name()) >= 0) {
            return retrieve(root.right, key);
        }
        return root;
    }

    /**
     * This is a poorly named function that really just returns the total price of all the orders. : /
     * @param root: Node the root of the tree.
     * @param key: String a string of data for which to search for.
     * @return Node the node if we were able to find it.
     */
    protected Node retrieveAll(Node root, String key) {
        if(root == null) {
            return null;
        } else if(key.equals(root.name())) {
            return root;
        } else if(key.compareTo(root.name()) < 0) {
            return retrieveAll(root.left, key);
        } else if (key.compareTo(root.name()) >= 0) {
            return retrieveAll(root.right, key);
        }
        return root;
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
        root.display(); // Call display on node in tree.
        return count + display(root.right);
    }

    /**
     * Refactor's the tree after insertion of a new node to maintain its balanced properties. It does so by traversing
     * backwards up the tree from the newly inserted node, adjusting colors and/or rotating as necessary.
     *
     * Note: This algorithm was taken from Introduction to Algorithms cited below.
     *
     *      Cormen, Thomas H.; Leiserson, Charles E.; Rivest, Ronald L.; Stein, Clifford (2001). "Red–Black Trees". 
     *      Introduction to Algorithms (second ed.). MIT Press. pp. 273–301
     *
     * @param newNode: Node the node that has just been added to the tree.
     * @return boolean Whether or not the recoloring/repositioning of nodes in the tree maintained self balanced properties.
     */
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

    /**
     * Private helper function used by `refactorAfterInsertion(...)` to performs rotations moving right about a node.
     *
     * @param rotateAbout: the parent of node which we have determined to be in violation of the trees balanced state.
     * @return: boolean Value calculated by measuring the difference of the trees high before and after rotation determining
     * that the difference in heights does not exceed 1.
     */
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

    /**
     * Private helper function used by `refactorAfterInsertion(...)` to performs rotations moving left about a node.
     *
     * @param rotateAbout: the parent of node which we have determined to be in violation of the trees balanced state.
     * @return: boolean Value calculated by measuring the difference of the trees high before and after rotation determining
     * that the difference in heights does not exceed 1.
     */
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

    /**
     * Private function which recursvely traverses the tree calling each nodes `removeAll()` function to delete it's data.
     *
     * @param root: Node the root of the tree.
     * @return boolean if we successfully removed all the node. (this will always be true)
     */
    private boolean removeAll(Node root) {
        if(root == null) {
            return true;
        }
        removeAll(root.left);
        removeAll(root.right);
        return root.removeAll();
    }
}