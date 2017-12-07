/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

import java.util.*;
import java.io.*;
import java.nio.file.*;
import java.nio.charset.Charset;

/**
 * @brief Static class of helper functions for use throughout the program.
 *
 * @desc Using such complicated file I/O, heavy builtin data structure dependency and a static utility class were perhaps the
 * worst decision I have made all term. To be honest, I'm not sure how I managed to retrieve it working, but am assured that
 * this will be the last time I use a non-standard data format.
 *
 * As well, forgive the complexity of this class. Fortunately, there isn't really any code in here that we were required to
 * write (in terms of data structures, hierarchies, etc.) so hopefully you aren't obligated to read through this monstrosity.
 *
 */
public class Menu {
    private static final Path file = Paths.get("src/data.txt"); // Use src b/c it looks for file at program entry point. I.e. 'project4/'
    private static boolean notDone = true; // Bool to control our menu driven program.
    public static String[] stdOps = {"Start a new order.", "Review your orders.", "[Beta] Remove an order.", "Pay and quit."}; // List of the four menu options allowed
    private static HashMap<String, ArrayList<HashMap<String, String>>> restaurantData = new HashMap<>(); // Implicit call to ArrayList constructor using newer java 7(?) syntax: '<>'

    public static boolean notDone() {
        return notDone;
    }
    public static void done() {
        notDone = false;
    }

    /**
     * @brief Wrapper function for System.out.println(...)
     *
     * Hint: pass in "" (empty string) to print a new line
     * @param s: String the string to print.
     */
    public static void print(String s) {
        if(!s.equals("")) {
            System.out.println(s);
        } else {
            System.out.println();
        }
    }

    /**
     * @brief Similar to `print(...)` this will append a delimiter of your choice.
     *
     * Hint: pass in "" (empty string) as the second arg for no delimiter.
     * @param s: String the string to print.
     * @param delim: String a delimiter to append to the end of the string. I.e. "\n\t"
     */
    public static void print(String s, String delim) {
        if(!delim.equals("")) {
            System.out.print(s + delim);
        } else {
            System.out.print(s);
        }
    }

    /**
     * @brief Returns a restaurants list of items and prices given a restaurants name.
     *
     * Note: if the restaurant parameter is not found this function returns null
     * @param restaurant: String the restaurant who's data we want to retrieve.
     * @return ArrayList<HashMap<String, String>> the list of restaurant items and prices
     */
    public static ArrayList<HashMap<String, String>> getRestaurant(String restaurant) {
        return restaurantData.get(restaurant);
    }

    /**
     * @brief Displays options in a format they can easily select from.
     *
     * Note: This will return -1 on `getOptions(...)` behalf if an invalid option is selected.
     * @param opArr: String[] a list of options to display.
     * @param message: String a message to give the user context as to what's being displayed.
     * @return int an integer corresponding to the option the user selected.
     */
    public static int showOptions(String[] opArr, String message) {
        print(message);
        print("", "\t");
        for(int i = 0; i < opArr.length; ++i) {
            print(i+1 + ") " + opArr[i], "\n\t"); // Pretty print
        }
        return getOptions(1, opArr.length);
    }

    /**
     * @brief Similar to `showOptions(...)` this will return the literal value selected.
     *
     * Note: This will return "" (empty string) if the user selects an invalid option from those displayed.
     * @param opArr: String[] a list of options to display.
     * @param message: String a message to give the user context as to what's being displayed.
     * @param addToOutput: String an identifier to give context to each item displayed.
     * @return String the option selected by the user if the selected input was a valid option, else "" (empty string)
     */
    public static String showAndGetOptions(String[] opArr, String message, String addToOutput) {
        print(message);
        print("", "\t");
        for(int i = 0; i < opArr.length; ++i) {
            print(i+1 + ") " + opArr[i] + " " + addToOutput.trim(), "\n\t"); // Pretty print
        }
        int op = getOptions(1, opArr.length) - 1;
        if(op >= 0) {
            return opArr[op];
        }
        return null;
    }

    /**
     * @brief Parse and return an option selected by the user.
     *
     * @param OPTIONS_MIN: final int the minimum value a user may select.
     * @param OPTIONS_MAX: final int the maximum value a user may select.
     * @return int the integer corresponding to the option selected by the user.
     */
    public static int getOptions(final int OPTIONS_MIN, final int OPTIONS_MAX) {
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

    public static String[] parseRestaurantName(String restaurant) {
        String[] rstrnt = restaurant.split(" +");
        return new String[] {String.join(" ", Arrays.copyOfRange(rstrnt, 0, rstrnt.length-1)), rstrnt[rstrnt.length-1]};
    }

    /**
     * @brief Returns the list of available restaurants.
     *
     * @param withMenu: boolean prints the available restaurants as well as returns them.
     * @return String[] the list of available restaurants.
     */
    public static String[] restaurants(boolean withMenu) {
        if(restaurantData == null) {
            return null;
        }
        String[] rstrntArr = new String[restaurantData.size()];
        int i = 0;
        for (String restaurant: restaurantData.keySet()) {
            String[] rstrnt = parseRestaurantName(restaurant);
            String formattedRstrnt = rstrnt[0] + " " + rstrnt[1];
            String value = restaurantData.get(restaurant).toString();
            if(withMenu) {
                print(formattedRstrnt + ": " + value, "\n\n");
            }
            rstrntArr[i] = formattedRstrnt;
            ++i;
        }
        return rstrntArr;
    }

    /**
     * @brief Returns a list of the items a restaurant has available.
     *
     * Note: This will return null if the restaurant has no data.
     * @param restaurant: String the restaurant for which we want to retrieve data from.
     * @return String[] the list of items available from the restaurant.
     */
    public static String[] menuItems(String restaurant) {
        if(restaurantData == null) {
            return null;
        }
        String[] itemArr = new String[restaurantData.get(restaurant).get(0).size()];
        int i = 0;
        ArrayList<HashMap<String, String>> placeholder = restaurantData.get(restaurant);
        for (HashMap<String, String> place: placeholder) {
            for(String item: place.keySet()) {
                itemArr[i] = item + " $" + place.get(item);
                ++i;
            }
        }
        return itemArr;
    }

    /**
     * @brief Reads in data from a data file. (Though you wouldn't realize by looking at it)
     *
     * @return HashMap<String, ArrayList<HashMap<String, String>>>: The "database" of restaurants which is then consumed
     * by the Tree class's constructor.
     * @throws IOException if the file cannot be found or cannot be opened.
     */
    public static HashMap<String, ArrayList<HashMap<String, String>>> readData() throws IOException {
        try {
            BufferedReader reader; // Keep compiler happy
            try {
                reader = Files.newBufferedReader(file, Charset.forName("UTF-8")); // data file should be in '<entry_point>/src'. I.e. platform agnostic
            } catch(Error err) {
                Path last_try = Paths.get(System.getProperty("user.dir") + "/src/name.txt"); // Ask system for programs entry point and insert know data location.
                print("Couldn't find file: '" + file + "' in the working directory."); // Let 'em know
                print("Trying again with full path: '" + last_try + "'", "\n\n");
                reader = Files.newBufferedReader(last_try, Charset.forName("UTF-8")); // If it's not here, well...
            }

            String line, restaurant_and_distance = ""; // For reading lines from data file.
            while ((line = reader.readLine()) != null) {
                while(line.startsWith("//") || line.trim().isEmpty()) { // Allow simple comments in this pseudo language
                    line = reader.readLine();
                }
                HashMap<String, String> items_and_prices = new HashMap<>(); // Map that will hold the item and its price.
                if(line.startsWith("- ")) {
                    while((line != null) && line.startsWith("- ")) {
//                             && (line.startsWith("//") && (line = reader.readLine()) != null)) { // Handle comments in item listing
                        String[] temp = line.split(" +"); // Create array of the menu items' name and price
                        String price = temp[temp.length - 1]; // Price is always listed last in pseudo language
                        String item = String.join(" ", Arrays.copyOfRange(temp, 1, temp.length-1)); // The menu item name is all but the first and last words in the list
                        items_and_prices.put(item, price); // Add them to the map. (key = item, value = price)
                        line = reader.readLine();
                    }
                } else {
                    restaurant_and_distance = line; // Save for later. This is here exclusively to take care of the first line.
                }
                restaurantData.putIfAbsent(restaurant_and_distance, new ArrayList<>()); // If the key doesn't exist yet, insert it with a new arraylist as its value
                // If we found items on the restaurants menu (items_and_prices has more than 0 values), insert them.
                if(items_and_prices.size() > 0) {
                    restaurantData.get(restaurant_and_distance.trim()).add(items_and_prices);
                }
                // If we found a new restaurant, update our placeholder.
                if(!restaurant_and_distance.equalsIgnoreCase(line)) {
                    restaurant_and_distance = line;
                }
            }
        } catch (IOException io_err) {
            print("Sorry, we couldn't open the file: ", "\n\t");
            print("'" + file + "' due to it being: " + io_err.getCause());
        }
        return restaurantData;
    }
}