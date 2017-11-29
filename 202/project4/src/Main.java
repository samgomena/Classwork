import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {
//        Tree new_tree;
//        new_tree = new Tree();
//        int num_nodes = new_tree.display();
//        System.out.println("Number of nodes: " + num_nodes);

//        Menu $ = new Menu(); // Used as quick-access utility identifier
        DLL dll  = new DLL();
        Menu.print("Welcome to CS202_Eatz.", "\n\n");

        Menu.readData();
        Menu.showRestaurants(false);
//        Menu.showRestaurants(true);


        do {
            Menu.options();
            switch(Menu.getOptions()) {
                case -1:
                    Menu.print("Please select a valid menu option and try again");
                    break;
                case 1:
                    Menu.print("Starting New Order");
                    dll.add();
                    break;
                case 2:
                    Menu.print("These are your orders so far");
                    dll.display();
                    break;
                case 3:
                    Menu.print("Are you sure you want to remove this order?");
                    break;
                case 4:
                    Menu.print("We only take whole bitcoins'.\nAre you sure you want to continue?");
                    Menu.done();
                    break;
                default:
                    Menu.print("Sorry, we didn't understand that.\nPlease try again.");
            }
        } while(Menu.notDone());



        dll.add();
        dll.add();
        dll.add();
        dll.display();
        dll.removeAll();
        dll.display();
    }
}