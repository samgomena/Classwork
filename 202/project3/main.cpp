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
    delete bst;

//    Sol *ll = new Sol();
//    ll->add(10);
//    ll->add(20);
//    ll->add(30);
//    ll->add(40);
//    ll->add(50);
//
//    ll->display();
//
//    ll->operator[](2).data(60);
//
//    ll->display();
//    delete ll;
    return 0;
}