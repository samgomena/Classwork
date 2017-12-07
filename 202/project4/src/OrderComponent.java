/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

public class OrderComponent {
    protected final float price;
    protected float totalPrice = 0;
    protected String mealName;

    public OrderComponent() {
        price = 0f;
        mealName = "";
    }

    public OrderComponent(String _meal_name, float _price) {
        price = _price;
        totalPrice += _price;
        mealName = _meal_name;
    }

    public boolean display() {
        return true;
    }
    public float getTotalPrice() { return totalPrice; }
}



