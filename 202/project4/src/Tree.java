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
            System.out.println("There was an error parsing the restaurant data.\nPlease try running again.");
            System.exit(0);
        }
    }

    public boolean insert(String restaurantName) {
        Node new_node = new Node(restaurantName);
        root = insert(root, new_node);
        return refactorAfterInsertion(new_node);
    }

    public String retrieve(String data) {
        return retrieve(root, data);
    }

    public String retrieveAll(String data) {
        return retrieveAll(root, data);
    }

    public int height() {
        return root == null ? 0 : height(root);
    }

    public int display() {
        return root == null ? 0 : display(root);
    }

    public boolean removeAll() {
        root = null;
        return true;
    }

    protected Node insert(Node root, Node new_node) {
        if(root == null) {
            return new_node;
        } else if (new_node.data().equals(root.data())) {
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

    protected String retrieve(Node root, String key) {
        if(root == null) {
            return "";
        } else if(key.equals(root.data())) {
            return root.data();
        } else if(key.compareTo(root.data()) < 0) {
            return retrieve(root.left, key);
        } else if (key.compareTo(root.data()) >= 0) {
            return retrieve(root.right, key);
        }
        return root.data();
    }

    protected String retrieveAll(Node root, String key) {
        if(root == null) {
            return "";
        } else if(key.equals(root.data())) {
            return root.data();
        } else if(key.compareTo(root.data()) < 0) {
            return retrieve(root.left, key);
        } else if (key.compareTo(root.data()) >= 0) {
            return retrieve(root.right, key);
        }
        return root.data();
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
        System.out.print(root.data() + ": ");
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

    public void printLevelOrder() {
        int h = height(root);
        for (int i = 1; i <= h; ++i) {
            printGivenLevel(root, i);
            System.out.println();
        }
    }

    private void printGivenLevel (Node root, int level) {
        if (root == null)
            return;
        if (level == 1) {
            System.out.print(root.data() + " ");
        } else if (level > 1) {
            printGivenLevel(root.left, level-1);
            printGivenLevel(root.right, level-1);
        }
    }
}