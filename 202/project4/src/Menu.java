import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Scanner;
import java.io.*;
import java.nio.file.Path;
import java.nio.file.Files;
import java.nio.file.FileSystems;
import java.nio.charset.Charset;



public class Menu {
    private static final Path file = FileSystems.getDefault().getPath("/Users/sgomena/Documents/repos/Classwork/202/project4/src/data.txt");
    private static final int OPTIONS_MIN = 1;
    private static final int OPTIONS_MAX = 4;
    private static boolean notDone = true;
    private static HashMap<String, ArrayList<HashMap<String, String>>> restaurantData = new HashMap<>(); // Implicit call to ArrayList constructor using newer java 7(?) syntax: '<>'
//    Menu() {}

    public static boolean notDone() {
        return notDone;
    }

    public static void done() {
        notDone = false;
    }

    public static void print(String s) {
        System.out.println(s);
    }

    public static void print(String s, String delim) {
        if(!delim.equals("")) {
            System.out.print(s + delim);
        } else {
            System.out.print(s);
        }
    }

    public static void options() {
        print("What would you like to do?");
        print("\t1) Start a new order." +
                "\n\t2) Review your orders." +
                "\n\t3) Remove an order." +
                "\n\t4) Pay for an order.");
    }

    public static void newOrderOptions() {
        print("Where would you like to order food from?");
//        restaurants.listAll();
        print("<list_of_restaurants>");
    }

    public static int getOptions() {
        Scanner sc = new Scanner(System.in);
        print("> ");
        String hopefully_int = sc.nextLine(); // Read potentially false input
        if(hopefully_int.length() > Integer.toString(OPTIONS_MAX - OPTIONS_MIN).length()) {
            return -1;
        }
        int should_be_int = Integer.parseInt(hopefully_int); // Convert to int
        if(should_be_int < OPTIONS_MIN || should_be_int > OPTIONS_MAX) {
            return -1;
        }
        return should_be_int;
    }

    public static void showRestaurants(boolean withMenu) {
        if(restaurantData == null) {
            return;
        }
        for (String restaurant: restaurantData.keySet()) {
            String[] rstrnt = restaurant.split(" ");
            String formattedRstrnt = String.join(" ", Arrays.copyOfRange(rstrnt, 0, rstrnt.length-1)) + " " + rstrnt[rstrnt.length-1].replaceAll(",", "-") + " mins";
            String value = restaurantData.get(restaurant).toString();
            if(withMenu) {
                print(formattedRstrnt + ": " + value, "\n\n");
            } else {
                print(formattedRstrnt);
            }

        }
    }

    public static void readData() throws IOException {
        try {
            BufferedReader reader = Files.newBufferedReader(file, Charset.forName("UTF-8"));
            String line;
            String restaurant_and_distance = ""; // I'll explain later.
            while ((line = reader.readLine()) != null) {
                // Allow simple comments in this pseudo language
                while(line.startsWith("//") || line.trim().isEmpty()) {
                    line = reader.readLine();
                }
                HashMap<String, String> items_and_prices = new HashMap<>();
                if(line.startsWith("- ")) {
                    while((line != null) && line.startsWith("- ")) {
//                             && (line.startsWith("//") && (line = reader.readLine()) != null)) { // Handle comments in item listing
                        String[] temp = line.split(" "); // Create array of the menu items name and price
                        String price = temp[temp.length - 1]; // Price is always listed last in pseudo language
                        String item = String.join(" ", Arrays.copyOfRange(temp, 1, temp.length-1)); // The menu item name is all but the first and last words in the list
                        items_and_prices.put(item, price); // Add them to the map. (key = item, value = price)
                        line = reader.readLine();
                    }
                } else {
                    restaurant_and_distance = line; // Save for later. This is here exclusively to take care of the first line.
                }
                restaurantData.putIfAbsent(restaurant_and_distance, new ArrayList<HashMap<String, String>>()); // If the key doesn't exist yet, add it with a new arraylist as its value
                // If we found items on the restaurants menu (items_and_prices has more than 0 values), add them.
                if(items_and_prices.size() > 0) {
                    restaurantData.get(restaurant_and_distance).add(items_and_prices);
                }
                // Update restaurant name already read, new restaurant name.
                if(!restaurant_and_distance.equalsIgnoreCase(line)) {
                    restaurant_and_distance = line;
                }
            }
        } catch (IOException io_err) {
            print("Couldn't open file: ", "\n\t");
            print(io_err.getMessage());
        }
    }
}
