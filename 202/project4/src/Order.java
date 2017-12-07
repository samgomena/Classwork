/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

public class Order extends OrderComponent {

    // TODO: Ran out of time to implement this functionality but all that would need to be done is add a call to
    // TODO: Menu and have it return the notes value. This would obviously be done at object instantiation.
    private String notes = "";

    Order() {
        super();
    }

    Order(String OrderName, float _price) {
        super(OrderName, _price);
    }

    Order(String _order_name, float _price, String _note) {
        super(_order_name, _price);
        notes += ", " + sanitize(_note);
    }

    /**
     * method to be used for project 5.
     * @param _note
     * @return boolean whether or not we were able to insert the note.
     */
    public boolean addNotes(String _note) {
        // Check if empty note, or note has already been added.
        if(_note.equals("") || notes.toLowerCase().contains(_note.toLowerCase())) {
            return false;
        } else {
            notes += ", " + sanitize(_note); // Comma separated note list
            return true;
        }
    }
    public boolean display() {
        Menu.print("You shouldn't have an order of type 'Order' yo.");
        return true;
    }
    private String sanitize(String str) {
        if(str.equals("")) {
            return str;
        }
        String removeWithoutSpace = "[./\\!:;?*]"; // Replace all of these with nothing i.e. ""
        String removeWithSpace = "[_]"; // Replace all of these with a space i.e. " "
        str.replaceAll(removeWithoutSpace, "").replaceAll(removeWithSpace, " ");
        return str;
    }
}

// --------------- Sub Classes of Order --------------- //
class Breakfast extends Order {
    Breakfast(String mealName, float _price) {
        super(mealName, _price);
    }

    public boolean display() {
//        Menu.print("Breakfast: ");
        Menu.print("\t" + mealName + " $" + price);
        return true;
    }

    public float getTotalPrice() { return totalPrice; }
}

class Lunch extends Order {
    Lunch(String mealName, float _price) {
        super(mealName, _price);
    }

    public boolean display() {
//        Menu.print("Lunch: ");
        Menu.print("\t" + mealName + " $" + price);
        return true;
    }

    public float getTotalPrice() { return totalPrice; }
}

class Dinner extends Order {
    Dinner(String mealName, float _price) {
        super(mealName, _price);
    }

    public boolean display() {
//        Menu.print("Dinner: ");
        Menu.print("\t" + mealName + " $" + price);
        return true;
    }

    public float getTotalPrice() { return totalPrice; }
}