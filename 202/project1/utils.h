/**
 * @file utils.h
 * @brief Header file for function definitions of project wide utility functions.
 *
 * @author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef UTILS_H
#define UTILS_H

#include <iostream>
#include <stdlib.h>     /* srand, rand */
#include <time.h>


using namespace std;

const bool DEBUG = false;

int generate_random(int lower, int upper);
const void NOTICE();
const void WARN();

#endif