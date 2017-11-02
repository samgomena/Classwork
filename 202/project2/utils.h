/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef UTILS_H
#define UTILS_H


#include <iostream>
#include <cstring>
#include <stdlib.h>     /* srand, rand */
#include <time.h>

using namespace std;

//#define arr_size(x) (sizeof(x) / sizeof((x)[0]))

int generate_random(int lower, int upper);
int *prompt_user(const int &);

// Textbooks to review
extern const char * KR;
extern const char * PRATA;
extern const char * STROUSTROUPS;

extern const char *TEXTBOOK_ARRAY[];

// Programming questions
extern const char * Q1;
extern const char * A1;
extern const char * Q2;
extern const char * A2;
extern const char * Q3;
extern const char * A3;
extern const char * Q4;
extern const char * A4;
extern const char * Q5;
extern const char * A5;
extern const char * Q6;
extern const char * A6;

extern const char *QUESTION_ARRAY[];
extern const char *ANSWER_ARRAY[];

// Topics for slides
extern const char * SLIDE_TOPIC_GENERAL;
extern const char * SLIDE_TOPIC_1;
extern const char * SLIDE_TOPIC_2;
extern const char * SLIDE_TOPIC_3;
extern const char * SLIDE_TOPIC_4;
extern const char * SLIDE_TOPIC_5;
extern const char * SLIDE_TOPIC_6;
extern const char * SLIDE_TOPIC_7;
extern const char * SLIDE_TOPIC_8;

extern const char *SLIDE_ARRAY[];


#endif