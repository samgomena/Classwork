///**
// * Author: Sam Gomena
// * Class: CS202 Fall 2017
// * Instructor: Karla Fant
// */
//
//import java.util.ArrayList;
//import java.util.Calendar;
//import java.util.HashMap;
//import java.util.Random;
//
//public class LLL {
//    private Node head;
//    private Node tail;
//    private int components;
//
//    private class Node {
//        private Node next;
//        private OrderComponent oc;
//
//        public Node(String name, float price) {
//            next = null;
//            oc = getBinding(name, price);
//        }
//
//        public Node next() {
//            return next;
//        }
//        public boolean next(Node new_next) {
//            next = new_next;
//            return true;
//        }
//        public void display() {
//            oc.display();
//        }
//    }
//
//    public LLL() {
//        head = null;
//        tail = null;
//        components = 0;
//    }
//
//    public boolean add(ArrayList<HashMap<String, String>> orderStuff, String orderName, float price) {
//        return add(new Node(orderName, price));
//    }
//
//    public boolean removeAll() {
//        head = null;
//        tail = null;
//        return true;
//    }
//
//    public int display() {
//        if(head == null) {
//            return 0;
//        }
//        return display(head);
//    }
//
//    protected boolean add(Node new_node) {
//        if(head == null) {
//            head = new_node;
//            tail = new_node;
//            ++components;
//            return true;
//        }
//        new_node.next(head);
//        head = new_node;
//        ++components;
//        return true;
//    }
//
//    protected int display(Node curr) {
//        if(curr == null) {
//            return 0;
//        }
//        if(curr.next() == null) {
//            curr.display();
//        } else {
//            curr.display();
//            Menu.print("");
//        }
//        return display(curr.next()) + 1;
//    }
//
//    /**
//     * @brief Determines the type of object we are going to dynamically bind to based on the time of day.
//     *
//     * Note: Time of day outlined below
//     * @param name: the name of the ordered item. Passed to the new class instance.
//     * @param price: the price of the ordered item. Passed to the new class instance.
//     * @return OrderComponent object given back to the calling routine which is them dynamically bound.
//     */
//    private static OrderComponent getBinding(String name, float price) {
//        Calendar cal = Calendar.getInstance();
//        int hour = cal.get(Calendar.HOUR_OF_DAY);
////        Random rnd = new Random(); // TODO: retrieve tf rid of this
////        hour = rnd.nextInt(23);
//        switch(hour) {
//            case 5:
//            case 6:
//            case 7:     // 5AM - 10AM
//            case 8:
//            case 9:
//            case 10:
//                return new Breakfast(name, price);
//            case 11:
//            case 12:
//            case 13:    // 11AM - 3PM
//            case 14:
//            case 15:
//                return new Lunch(name, price);
//            case 16:
//            case 17:
//            case 18:
//            case 19:    // 4PM - 10PM
//            case 20:
//            case 21:
//            case 22:
//                return new Dinner(name, price);
//            default:
//                return new Dinner(name, price); // Why not?
//        }
//    }
//}
