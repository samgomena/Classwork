
/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {

        Tree restaurantsAvailable = new Tree(Menu.readData());
        restaurantsAvailable.display();

        Menu.print("Welcome to CS202_Eatz.", "\n\n");

        do {
            switch (Menu.showOptions(Menu.stdOps, "What would you like to do")) {
                case -1:
                    Menu.print("Please select a valid menu option and try again");
                    break;
                case 1:
                    Menu.print("Starting New Order...");
                    String chosenRestaurant = Menu.showAndGetOptions(Menu.restaurants(false),
                            "Here are the available restaurants:",
                            "minutes away.");
                    if(chosenRestaurant == null) {
                        Menu.print("Sorry, we didn't retrieve that.\nPlease try again.");
                        break;
                    }
                    String[] itemWithPrice = Menu.showAndGetOptions(Menu.menuItems(chosenRestaurant),
                                    ("What do you want to order from " + chosenRestaurant.split(" ")[0] + "?"),
                            "").split("\\$");
                    if(itemWithPrice.length == 1) {
                        Menu.print("Sorry, we didn't retrieve that.\nPlease try again.");
                        break;
                    }
                    String item = itemWithPrice[0], price = itemWithPrice[1];

                    Node restaurant = restaurantsAvailable.retrieve(chosenRestaurant);
                    if(restaurant != null && restaurant.addOrder(chosenRestaurant, item, Float.parseFloat(price))) {
                        Menu.print("Your order of " + item + "has been added.");
                        Menu.print("Total price: " + restaurant.getOrderTotal());
                        Menu.print("Total orders from " + restaurant.name() + " is " + restaurant.getOrderSize());
                    } else {
                        Menu.print("Sorry, we were not able to add your order, please try again.");
                    }
                    break;
                case 2:
                    int orders = restaurantsAvailable.getNumberOfAllOrders();
                    restaurantsAvailable.display();
                    if(orders == 0) {
                        Menu.print("It looks like you don't have any orders yet.\nPress '1' to change that.");
                    } else {
                        Menu.print("Total orders: " + orders);
                    }
                    break;
                case 3:
                    System.out.println("Sorry this action is not supported yet.");
                    break;
                case 4:
                    final float BTC_PRICE = 14032.00f;
                    float ordersTotalPrice = restaurantsAvailable.getPriceOfAllOrders();
                    if(ordersTotalPrice > 0) {
                        Menu.print("Your order total is $" + ordersTotalPrice);
                        Menu.print("\tNote: We only take whole bitcoins.");
                        Menu.print("Your order is " + (ordersTotalPrice / BTC_PRICE) * 100 + "% the price of one bitcoin.");
                        Menu.print("Due to this we were unable to process your order.\nHave a good one though!");
                    }
                    restaurantsAvailable.removeAll();
                    Menu.done();
                    break;
                default:
                    Menu.print("Sorry, we didn't retrieve that.\nPlease try again.");
            }
        } while (Menu.notDone());
    }
}