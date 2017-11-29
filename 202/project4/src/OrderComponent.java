public class OrderComponent {

    private final float price;
    private final float prep_time;
    private String meal_name;


    public OrderComponent() {
        price = 0f;
        prep_time = 0f;
        meal_name = "";
    }

    public OrderComponent(float _price, float _prep_time, String _meal_name) {
        price = _price;
        prep_time = _prep_time;
        meal_name = _meal_name;
    }
}

