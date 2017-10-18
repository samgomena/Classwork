/**
 * @file main.cpp
 * @brief The driver of the application.
 *
 * The main function is responsible for kicking off the initialization of our city.
 * It does this by representing a graph with a 2D array of numbers where each number corresponds to a different city.
 * The nodes in the list are the cities that the city (the head of the linked-list) is connected to.
 *
 * @author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "list.h"
#include "fire_suppression.h"

using namespace std;

int main() {
    const int ARRAY_LENGTH = 9;
    // Map initialization
    const int city_1[] = {1, 7, 6, 2, 0};
    const int city_2[] = {2, 1, 3, 0};
    const int city_3[] = {3, 8, 9, 2, 0};
    const int city_4[] = {4, 9, 5, 0};
    const int city_5[] = {5, 9, 6, 4, 0};
    const int city_6[] = {6, 1, 9, 0};
    const int city_7[] = {7, 1, 8, 9, 0};
    const int city_8[] = {8, 3, 9, 7, 0};
    const int city_9[] = {9, 3, 8, 7, 4, 5, 6, 0};
    const int * city_data[ARRAY_LENGTH];
    city_data[0] = city_1;
    city_data[1] = city_2;
    city_data[2] = city_3;
    city_data[3] = city_4;
    city_data[4] = city_5;
    city_data[5] = city_6;
    city_data[6] = city_7;
    city_data[7] = city_8;
    city_data[8] = city_9;

    srand(time(NULL));
    // int dlist_length = 5;//generate_random(1, 20);
    // int llist_length = generate_random(1, 20);
    // cout << "List length: " << dlist_length << endl;

    // Test copy ctor    
    // D_Node *node = new D_Node(69);
    // D_Node *other_node = new D_Node(*node);
    // cout << "Node: " << node->get_data() << " " << node->prev() << "\nOther node: " << other_node->get_data() << " " << other_node->prev() << endl;
    
    // DLL *list = new DLL();
    // list->build(dlist_length);
    // list->display();
    // cout << endl << endl;
    // // delete list;
    // // list = NULL;
    //
    // // Test copy ctor
    // DLL *nu_list = new DLL(*list);
    // list->add(69);
    // list->display();
    // cout << endl << endl;
    // nu_list->display();
    // delete nu_list;
    // nu_list = NULL;
    //
    // LLL *l_list = new LLL();
    // l_list->build(llist_length);
    // l_list->display();
    // delete l_list;
    // list = NULL;
    
    // Test copy ctor
    // LLL *nu_list = new LLL(*l_list);
    // nu_list->display();
    // delete nu_list;
    // nu_list = NULL;
    
    int i, j;
    LLL **arr;
    arr = new LLL*[ARRAY_LENGTH];
    for(i = 0; i < ARRAY_LENGTH; ++i) {
        arr[i] = new LLL();
        for(j = 0; city_data[i][j] != 0; j++) {
            arr[i]->add(city_data[i][j]);
        }
    }
    
    for(i = 0; i < ARRAY_LENGTH; ++i) {
        // cout << "LLL " << i << ": ";
        arr[i]->display();
        cout << "-----------------------------------------";
        delete arr[i];
        arr[i] = NULL;
    }
    delete arr;
    arr = NULL;

    FireSuppression *areal = new UrbanSupport();
    FireSuppression *structural = new SuburbanSupport();
    FireSuppression *wild = new WildSupport();
    
    cout << endl;
    areal->report();
    structural->report();
    wild->report();
    
    delete areal;
    delete structural;
    delete wild;
}





