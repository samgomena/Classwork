/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */
 
#include <iostream>
#include "utils.h"

using namespace std;
 
int *prompt_user() {
    // Initialized to values from example in assignment.
    int num_textbooks = 3, num_slides = 2, num_questions = 10;
    // Array to pass back number of review assignments.
    int *review_array = new int[3];
    // Welcome user to program.
    static bool welcomed = false;
    if(!welcomed) {
        cout << "--------- Welcome to the CS202 Test Preparation Generator ---------" << endl << endl;
        welcomed = false;
    }
    cout << "Please enter how many textbooks you would like to study.\n> ";
    cin >> num_textbooks;
    cout << "Please enter how many class slides you would like to review.\n> ";
    cin >> num_slides;
    cout << "Please enter how many programming questions you would like to review.\n> ";
    cin >> num_questions;
    cout << "You have chosen to study " << num_textbooks << " textbooks, " << num_slides << " class slides, and " << num_questions << " programming questions.\nNote: all review material will be selected for you at random!\n\n";
    
    review_array[0] = num_textbooks;
    review_array[1] = num_slides;
    review_array[2] = num_questions;
    return review_array;
}
 
