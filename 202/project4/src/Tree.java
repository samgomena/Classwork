public class Tree {

    public Tree() {}

    private TreeNode root;

    public int display() {
        System.out.println("Tree Display");
        return display(root);
    }
    private int display(TreeNode root) {
        if(root == null) {
            return 0;
        }
        System.out.println(root.data());
        return display(root.goLeft()) + display(root.goRight()) + 1;
    }

}