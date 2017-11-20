/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "binary_tree.h"

Node::Node() : left(NULL), right(NULL), race_history(new Sol()) {}

Node::Node(int days_to_race, int train_type, float workouts_per_week, int race_type)
        : left(NULL), right(NULL), race_history(NULL), days_to_race(days_to_race),
          train_type(train_type), workouts_per_week(workouts_per_week), race_type(race_type) {
    race_history = new Sol();
}

Node::~Node() {
    if(race_history) {
        delete race_history;
    }
}

Node::Node(const Node & copy)
        : left(NULL), right(NULL), days_to_race(copy.days_to_race), train_type(copy.train_type),
          workouts_per_week(copy.workouts_per_week), race_type(copy.race_type), race_history(copy.race_history) {}

Node &Node::operator=(const Node & equal) {
    if(this == &equal) {
        return *this;
    }
    if(race_history) {
        delete race_history;
    }
    this->days_to_race = equal.days_to_race;
    this->workouts_per_week = equal.workouts_per_week;
    this->train_type = equal.train_type;
    this->race_type = equal.race_type;
    this->race_history = equal.race_history;
}

/**
 * @brief Adds a data point to this nodes historical race data
 * @param data_point The data to be added to the nodes race history list
 */
void Node::add_data(float data_point) {
    race_history->add(data_point);
}

/**
 * @brief Prints the node's data to the console
 * @param out The console output object
 * @param node The node in question to print
 * @return ostream& The console output object
 */
ostream& operator << (ostream& out, const Node& node) {
    out << "Days to race: " << node.days_to_race
        << "\nWorkouts per week: " << node.workouts_per_week
        << "\nTraining: "; node.determine_train_type(node.train_type);
        out << "\nRace type: "; node.determine_race_type(node.race_type);
        out << "\nPrevious race times: ";
    int num_races = node.race_history->display(); // Use external list print function and get number of race history items
    out << "Total races completed: " << num_races << endl;
    return out;
}

const void Node::determine_race_type(int type) const {
    switch(type) {
        case 1:
            cout << "Short race";
            break;
        case 2:
            cout << "Medium race";
            break;
        case 3:
            cout << "Long race";
            break;
        default:
            cout << "Medium race";
            break;
    }
}

const void Node::determine_train_type(int type) const {
    switch(type) {
        case 1:
            cout << "Cardio";
            break;
        case 2:
            cout << "Walking or Running";
            break;
        case 3:
            cout << "Weight training";
            break;
        case 4:
            cout << "Cardio and weight training";
            break;
        case 5:
            cout << "Weight training and walking or running";
            break;
        case 6:
            cout << "Cardio and walking or running";
            break;
        case 7:
            cout << "Cardio, weight training, and walking or running";
            break;
        default:
            cout << "Cardio, weight training, and walking or running";
            break;
    }
}

BST::BST() : root(NULL), DATA_FILE("./data.txt") {
    import(); // Load data from file `data.txt` into bst
}

BST::BST(const char * INPUT): root(NULL), DATA_FILE(INPUT) {
    import(); // Load data from file into bst
}

BST::~BST() {
    destroy(root);
}

BST::BST(const BST & copy_me) : root(NULL) {
    copy(root, copy_me.root);
}

BST &BST::operator=(const BST & equal) {
    if(this == &equal) {
        return *this;
    }
    if(root) {
        destroy(root);
    }
    copy(root, equal.root);
    return *this;
}

/**
 * @brief Public Wrapper for protected add function.
 *
 * Creates a new node to be added to the tree with the provided parameters from the data file.
 * Still using the parameters from the data file it adds the race history data to the nodes race history list.
 * It then calls the protected add function with the fully initialized node.
 *
 * @param days_to_race The number of days until this race happens
 * @param train_type The suggested type of training for this race
 * @param workouts_per_week The suggested number of workouts per week to prepare for this race
 * @param race_type The type of race
 * @param race_history A list of historical race times for similar races
 * @return bool Whether we were able to add the node, this will always be true.
 */
bool BST::add(int days_to_race, int train_type, float workouts_per_week, int race_type, char * race_history) {
    Node *node = new Node(days_to_race, train_type, workouts_per_week, race_type);
    char *str_char;
    str_char = strtok(race_history, ", ");  // Split list on ',' character
    while(str_char) {
        node->add_data(atof(str_char)); // Convert to float and add to race history list function
        str_char = strtok(NULL, ", ");
    }
    return add(this->root, node, days_to_race);
}

void BST::display() {
    display(root);
}

// Protected Functions
void BST::destroy(Node *&root) {
    if(!root) {
        return;
    }
    destroy(root->left);
    destroy(root->right);
    delete root;
    root = NULL;
    return;
}

void BST::copy(Node *& dest, Node * src) {
    if(src == NULL) {
        dest = NULL;
        return;
    }
    dest = new Node(*src);
    copy(dest->left, src->left);
    copy(dest->right, src->right);
    return;
}
/**
 * @brief Protected add functions that adds a node to the bst.
 *
 * This insertion algorithm places the soonest races on the left subtree and the farthest races on the right.
 * Duplicate races on the same day are supported (but not advised) and added as the in order successor to the first race node of the duplicate race nodes.
 *
 * @param root The root of the bst, or more generally, the node to start traversal on.
 * @param new_node The node we are adding to the list.
 * @param key The value used for determining where to place the node in the bst.
 * Internally, this is always the days_to_race value of the node.
 * @return bool Whether we were able to add the node, this will always be true.
 */
bool BST::add(Node *& root, Node *& new_node, int key) {
    if(!root) {
        root = new_node;
        return true;
    } else if(key < root->days_to_race) {
        add(root->left, new_node, key);
    } else {
        add(root->right, new_node, key);
    }
    return true;
}
/**
 * @brief Display tree in order
 *
 * Uses the overloaded Node stream insertion operator to display the contents of each node in the tree.
 * Traverses as far left as possible, prints the node and attempts to go right. I.e. follows the in order traversal algorithm.
 *
 * @param root The root of the bst
 */
void BST::display(Node * root) {
    if(!root) {
        return;
    }
    display(root->left);
    cout << *root << endl;
    display(root->right);
    return;
}

// Private Functions
void BST::import() {
    ifstream in(DATA_FILE); // Open file
    char *str = new char[256]; // Buffer to hold race history data in. 256 used arbitrarily.
    int days_to_race, train_type, race_type;
    float workouts_per_week;
    if(in.is_open()) {
        while(!in.eof()) { // Read in data one line at a time until at end of file
            in >> days_to_race;
            in >> train_type;
            in >> workouts_per_week;
            in >> race_type;
            in.getline(str, 256);

            // Make sure we're not on the last line (with no data)
            if(!in.eof()) {
                add(days_to_race, train_type, workouts_per_week, race_type, str);
            }
        }
    } else { // Data file was not found, likely spelled incorrectly...
        cout << "Could not find data file."
                "\nPlease try again, adding the data file as an argument."
                "\n\tNote: data file provided is `data.txt`" << endl;
    }
    delete str;
}



