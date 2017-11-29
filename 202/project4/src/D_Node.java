public class D_Node {
    private D_Node next;
    private D_Node prev;
    protected LLL list;

    public D_Node() {
        next = null;
        prev = null;
        list = new LLL();
    }

    public D_Node(String order_name) {
        next = null;
        prev = null;
        list = new LLL(order_name);
    }


    public D_Node next() {
        return next;
    }

    public void next(D_Node new_next) {
        next = new_next;
    }

    public D_Node prev() {
        return prev;
    }

    public void prev(D_Node new_prev) {
        prev = new_prev;
    }
}
