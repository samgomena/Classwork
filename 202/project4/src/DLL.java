public class DLL {
    private D_Node head;
    private D_Node tail;
    private int order_nums;

    public DLL() {
        head = null;
        tail = null;
        order_nums = 0;
    }

    public boolean add() {
        addFront(new D_Node());
        addBack(new D_Node());
        return true;
    }

    public void display() {
        if(head == null) {
            return;
        }
        display(head);
    }

    public boolean removeAll() {
        head = null;
        tail = null;
        return true;
    }

    private D_Node addFront(D_Node new_node) {
        if(head == null) {
            head = new_node;
            tail = new_node;
            return head;
        }
        new_node.next(head);
        head.prev(new_node);
        head = new_node;
        ++order_nums;
        return head;
    }

    private D_Node addBack(D_Node new_node) {
        if(head == null) {
            head = new_node;
            tail = new_node;
            return head;
        }
        new_node.prev(tail);
        tail.next(new_node);
        tail = new_node;
        ++order_nums;
        return tail;
    }

    protected int display(D_Node curr) {
        if(curr == null) {
            return 0;
        }
//        curr.list.display();
        int i = 1;
        System.out.print("Order " + i + " ");
        return display(curr.next()) + (++i);
//        return 0;
    }
}
