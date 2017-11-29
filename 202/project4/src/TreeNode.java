public class TreeNode {

    private TreeNode left;
    private TreeNode right;
    private int data;

    public TreeNode() {
        left = null;
        right = null;
    }

    public TreeNode goLeft() {
        return left;
    }

    public TreeNode goRight() {
        return right;
    }

    public int data() {
        return data;
    }

}