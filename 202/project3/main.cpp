/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "binary_tree.h"

int main(int argc, char **argv) {
    BST *bst;
    // Use command line argument if we cannot find the data file.
    if(argc > 1) {
        bst = new BST(argv[1]);
    } else {
        bst = new BST();
    }

    bst->display();

// Tests:
//    Sol *ll = new Sol();
//    BST *new_bst = new BST(*bst);
//    new_bst->display();
//
//    ll->add(10.0);
//    ll->add(20.0);
//    ll->add(30.0);
//    ll->add(40.0);
//
//    ll->display();
//
//    cout << ll->operator[](1) << endl;
//    ll->operator[](1) = 90;
//    ll->display();
//
//    Sol *ll1 = new Sol(*ll);
//    ll1->add(60.0);
//    ll1->display();
//
//    delete ll1;
//    delete ll;
//    delete new_bst;

    delete bst;
    return 0;
}
