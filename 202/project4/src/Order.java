public class Order extends OrderComponent {

    private String order_type;
    private String notes = "";

    Order() {
        super();
        order_type = "any";
    }

    Order(float _price, float _prep_time, String _meal_name) {
        super(_price, _prep_time, _meal_name);
        order_type = "any";
    }

    Order(float _price, float _prep_time, String _order_name, String _order_type, String _note) {
        super(_price, _prep_time, _order_name);
        order_type = _order_type;
        notes += ", " + sanitize(_note);
    }

    public boolean addNotes(String _note) {
        // Check if empty note, or note has already been added.
        if(_note.equals("") || notes.toLowerCase().contains(_note.toLowerCase())) {
            return false;
        } else {
            notes += ", " + sanitize(_note); // Comma separated note list
            return true;
        }
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
