/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include <iostream>
#include "utils.h"

using namespace std;

bool CUSTOM_INPUT = true; // For testing purposes.

int generate_random(int lower, int upper) {
    return rand() % upper + lower;
}

int *prompt_user(const int &size) {
    // Initialized to values from example in assignment.
    int num_textbooks = 3, num_slides = 2, num_questions = 3;
    // Array to pass back number of review assignments.
    int *review_array = new int[size];
    if(CUSTOM_INPUT) {
        // Welcome user to program.
        static bool welcomed = false;
        if(!welcomed) {
            cout << "--------- Welcome to the CS202 Test Preparation Generator ---------" << endl << endl;
            welcomed = false;
        }
        cout << "Please enter how many textbooks you would like to study.\n> ";
        cin >> num_textbooks;
        cout << "Please enter how many programming questions you would like to review.\n> ";
        cin >> num_questions;
        cout << "Please enter how many class slides you would like to review.\n> ";
        cin >> num_slides;
        cout << "You have chosen to study " << num_textbooks << " textbooks, " << num_slides << " class slides, and " << num_questions << " programming questions.\nNote: all review material will be selected for you at random!\n\n";
    }
    review_array[0] = num_textbooks;
    review_array[1] = num_questions;
    review_array[2] = num_slides;
    return review_array;
}

const char * KR = "The C Programming Language by Kernighan & Ritchie";
const char * PRATA = "C++ Primer Plus by Stephen Prata";
const char * STROUSTROUPS = "The C++ Programming Language by Bjarne Stroustroup";

const char *TEXTBOOK_ARRAY[] = {
        KR,
        PRATA,
        STROUSTROUPS
};

const char * Q1 = "Q1";
const char * A1 = "A1";
const char * Q2 = "Q2";
const char * A2 = "A2";
const char * Q3 = "Q3";
const char * A3 = "A3";
const char * Q4 = "Q4";
const char * A4 = "A4";
const char * Q5 = "Q5";
const char * A5 = "A5";
const char * Q6 = "Q6";
const char * A6 = "A6";

const char *QUESTION_ARRAY[] = {
        Q1,
        Q2,
        Q3,
        Q4,
        Q5,
        Q6
};
const char *ANSWER_ARRAY[] = {
        A1,
        A2,
        A3,
        A4,
        A5,
        A6
};

const char * SLIDE_TOPIC_GENERAL = "https://d2l.pdx.edu/d2l/le/content/653818/Home";
const char * SLIDE_TOPIC_1 = "https://d2l.pdx.edu/d2l/le/content/653818/viewContent/3093666/View";
const char * SLIDE_TOPIC_2 = "https://d2l.pdx.edu/d2l/le/content/653818/viewContent/3093669/View";
const char * SLIDE_TOPIC_3 = "https://d2l.pdx.edu/d2l/le/content/653818/viewContent/3093670/View";
const char * SLIDE_TOPIC_4 = "https://d2l.pdx.edu/d2l/le/content/653818/viewContent/3093673/View";
const char * SLIDE_TOPIC_5 = "https://d2l.pdx.edu/d2l/le/content/653818/viewContent/3093676/View";
const char * SLIDE_TOPIC_6 = "https://d2l.pdx.edu/d2l/le/content/653818/viewContent/3093678/View";
const char * SLIDE_TOPIC_7 = "https://d2l.pdx.edu/d2l/le/content/653818/viewContent/3093685/View";
const char * SLIDE_TOPIC_8 = "https://d2l.pdx.edu/d2l/le/content/653818/viewContent/3093687/View";

const char *SLIDE_ARRAY[] = {
        SLIDE_TOPIC_1,
        SLIDE_TOPIC_2,
        SLIDE_TOPIC_3,
        SLIDE_TOPIC_4,
        SLIDE_TOPIC_5,
        SLIDE_TOPIC_6,
        SLIDE_TOPIC_7,
        SLIDE_TOPIC_8,
        SLIDE_TOPIC_GENERAL
};