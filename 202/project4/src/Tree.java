public class Tree {

    private Node root;
    private static final boolean RED = true;
    private static final boolean BLACK = false;

    public Tree() {
        root = null;
    }

    public boolean put(int data) {
        Node new_node = new Node(data);
        root = add(root, new_node);
        refactorAfterInsertion(new_node);
        return true;
    }

    public int get() {
        return root.data();
    }

    public int get(int data) {
        return get(root, data);
    }

    public int height() {
        if(root == null) {
            return 0;
        }
        return height(root);
    }

    public int display() {
        if(root == null) {
            return 0;
        }
        return display(root);
    }

    private Node add(Node root, Node new_node) {
        if(root == null) {
            return new_node;
        } else if (new_node.data() < root.data()) {
            Node leftChild = add(root.left, new_node);
            root.left = leftChild;
            leftChild.parent = root;
        } else {
            Node rightChild = add(root.right, new_node);
            root.right = rightChild;
            rightChild.parent = root;
        }
        return root;
    }

    private int get(Node root, int key) {
        if(root == null) {
            return -1;
        } else if(key < root.data()) {
            get(root.left, key);
        }
        else if (key > root.data()) {
            get(root.right, key);
        } else if(key == root.data()) {
            return root.data();
        }
        return root.data();
    }

    private int height(Node root) {
        if(root == null) {
            return 0;
        }
        int left = height(root.left) + 1;
        int right = height(root.right) + 1;
        return left > right ? left : right;
    }

    private int display(Node root) {
        if(root == null) {
            return 0;
        }
        int count = display(root.left) + 1;
        System.out.print(root.data() + ", ");
        return count + display(root.right);
    }

    private void refactorAfterInsertion(Node newNode) {
        Node temp;
        newNode.color(RED);
        while (newNode.parent() != null && newNode.parent().isRed()) {
            if (newNode.parent() == newNode.grandparent().left) {
                temp = newNode.grandparent().right; // Uncle
                if (temp != null && temp.isRed()) {
                    newNode.parent().color(BLACK);
                    temp.color(BLACK);
                    newNode.grandparent().color(RED);
                    newNode = newNode.grandparent();
                } else {
                    if (newNode == newNode.parent().right) {
                        newNode = newNode.parent();
                        rotateLeft(newNode);
                    }
                    newNode.parent().color(BLACK);
                    newNode.grandparent().color(RED);
                    rotateRight(newNode.grandparent());
                }
            } else {
                temp = newNode.grandparent().left;
                if (temp != null && temp.isRed()) {
                     newNode.parent().color(BLACK);
                    temp.color(BLACK);
                    newNode.grandparent().color(RED);
                    newNode = newNode.grandparent();
                } else {
                    if (newNode == newNode.parent().left) {
                        newNode = newNode.parent();
                        rotateRight(newNode);
                    }
                    newNode.parent().color(BLACK);
                    newNode.grandparent().color(RED);
                    rotateLeft(newNode.grandparent());
                }
            }
        }
        root.color(BLACK);
    }

    private void rotateRight(Node x) {
        Node y = x.left;

        x.left = y.right;
        if (x.left != null) {
            y.right.parent = x;
        }
        y.parent = x.parent;
        y.right = x;
        x.parent = y;
        if (root == x) {
            root = y;
        } else {
            if (y.parent.left == x) {
                y.parent.left = y;
            } else {
                y.parent.right = y;
            }
        }
    }

    private void rotateLeft(Node x) {
        Node y = x.right;
        x.right = y.left;
        if (y.left != null) {
            y.left.parent = x;
        }

        y.parent = x.parent;
        if (x.parent == null) {
            root = y;
        } else {
            if (x == x.parent.left) {
                x.parent.left = y;
            } else {
                x.parent.right = y;
            }
        }
        y.left = x;
        x.parent = y;
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