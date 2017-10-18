/**
 * @file utils.cpp
 * @brief Implementation of project wide utility functions.
 *
 * @author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "utils.h"

/**
 * @brief Generates a random number in the range [lower, upper].
 * 
 * @param upper The upper bound of the randomly generated number.
 * @param lower The lower bound of a randomly generated number.
 * 
 * @return A randomly generated number between `upper` and `lower`.
*/
int generate_random(int lower, int upper) {
    // if(lower > 0 && upper > lower) {}
    return rand() % upper + lower;
}

/**
 * @brief Notify the user textually.
 *
 * @return void
 */
const void NOTICE() {
    cout << "NOTICE: ";
}

/**
 * @brief Warn the user textually.
 *
 * @return void
 */
const void WARN() {
    cout << "WARNING: ";
}