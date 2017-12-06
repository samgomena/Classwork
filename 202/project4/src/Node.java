class Node {
    protected Node left;
    protected Node right;
    protected Node parent;
    private static final boolean RED = true;
    private static final boolean BLACK = false;
    private int data;
    private boolean color;

    Node(int _data) {
        left = null;
        right = null;
        parent = null;
        data = _data;
        color = BLACK;
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

    public int data() {
        return this.data;
    }

    public void data(int new_data) {
        this.data = new_data;
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
}