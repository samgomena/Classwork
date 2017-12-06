/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {

        Tree tree = new Tree();

//        boolean added_all = tree.put(10) && tree.put(13) && tree.put(5) && tree.put(69) && tree.put(2) && tree.put(7) && /*tree.put(23) && */ tree.put(11) && tree.put(12) && tree.put(68) && tree.put(69) && tree.put(11);
        boolean added_all = tree.put(7) && tree.put(4) && tree.put(5) && tree.put(6) && tree.put(20) && tree.put(19) && tree.put(18) && tree.put(1) && tree.put(2) && tree.put(17) && tree.put(16) && tree.put(8);
        int count = tree.display();
        System.out.println("Added all: " + added_all + "\nTherefore we have " + count + " nodes.");

        System.out.println("Root: " + tree.get());
        System.out.println("get(5) = " + tree.get(5));
        System.out.println("Height: " + tree.height());
        System.out.println("Level Order:");
        tree.printLevelOrder();
        System.out.println();
        tree.display();


        DLL dll = new DLL(); // Create new list to house our orders.
        Menu.print("Welcome to CS202_Eatz.", "\n\n");
//
//        // If we were able to open and read the data.
//        if (Menu.readData()) {
//            do {
//                switch (Menu.showOptions(Menu.stdOps, "What would you like to do")) {
//                    case -1:
//                        Menu.print("Please select a valid menu option and try again");
//                        break;
//                    case 1:
//                        Menu.print("Starting New Order...");
//                        String chosenRestaurant =
//                                Menu.showAndGetOptions(
//                                        Menu.restaurants(false),
//                                "Here are the available restaurants with distances:",
//                                "mins");
//                        if(chosenRestaurant == null) {
//                            Menu.print("Sorry, we didn't get that.\nPlease try again.");
//                            break;
//                        }
//                        String[] itemWithPrice =
//                                Menu.showAndGetOptions(
//                                        Menu.menuItems(chosenRestaurant),
//                                        ("What do you want to order from " + chosenRestaurant.split(" ")[0] + "?"),
//                                "").split("\\$");
//                        if(itemWithPrice.length == 1) {
//                            Menu.print("Sorry, we didn't get that.\nPlease try again.");
//                            break;
//                        }
//                        String item = itemWithPrice[0];
//                        String price = itemWithPrice[1];
//                        if(dll.addOrder(Menu.getRestaurant(chosenRestaurant), chosenRestaurant, item, Float.parseFloat(price))) {
//                            Menu.print("Your order of " + item + "has been added.\nTotal orders: " + dll.getOrderNums());
//                        } else {
//                            Menu.print("Sorry, we were not able to add your order, please try again.");
//                        }
//                        break;
//                    case 2:
//                        int orders = dll.display();
//                        if(orders == 0) {
//                            Menu.print("It looks like you don't have any orders yet.\nPress '1' to change that.");
//                        } else {
//                            Menu.print("Total orders: " + orders);
//                        }
//                        break;
//                    case 3:
//                        // TODO: Add functionality for project 5.
//                        break;
//                    case 4:
//                        Menu.print("Note: We only take whole bitcoins.");
//                        // TODO: Add functionality for project 5.
//                        Menu.done();
//                        break;
//                    default:
//                        Menu.print("Sorry, we didn't get that.\nPlease try again.");
//                }
//            } while (Menu.notDone());
//            dll.removeAll();
//        } else {
//            System.exit(0); // We done goofed.
//        }
    }
}