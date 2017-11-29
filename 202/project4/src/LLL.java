import java.util.Random;

public class LLL {
    private Node head;
    private Node tail;
    private int components;

    private class Node {
        private Node next;
        private OrderComponent oc;

        public Node() {
            next = null;
            oc = new OrderComponent();
        }

        public Node next() {
            return next;
        }

        public void next(Node new_next) {
            next = new_next;
        }
    }

    public LLL() {
        head = new Node();
        tail = null;
        components = 0;
    }

    public LLL(String order_name) {
        Random rnd = new Random();
        head = new Node();
        tail = null;
        components = 0;
    }

    public boolean add() {
        if(head == null) {
            return false;
        }
        return add(new Node());
    }

    public boolean removeAll() {
        head = null;
        tail = null;
        return true;
    }

    public int display() {
        if(head == null) {
            return 0;
        }
        return display(head);
    }

    protected boolean add(Node new_node) {
        if(head == null) {
            head = new_node;
            tail = new_node;
            return true;
        }
        new_node.next(head);
        head = new_node;
        ++components;
        return true;
    }

    protected int display(Node curr) {
        if(curr == null) {
            return 0;
        }
        if(curr.next() == null) {
            System.out.print("_");
        } else {
            System.out.print("_" + ", ");
        }
        return display(curr.next()) + 1;
    }
}
