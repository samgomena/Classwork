/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "utils.h"

/**
 * Helper function to generate a random number in the range [lower, upper].
 * 
 * upper: int
 *     The upper bound of the randomly generated number.
 * lower: int
 *     The lower bound of a randomly generated number.
 * 
 * Return: int
 *     A randomly generated number between `upper` and `lower`.
*/
int generate_random(int lower, int upper) {
    // if(lower > 0 && upper > lower) {}
    return rand() % upper + lower;
}

const void NOTICE() {
    cout << "NOTICE: ";
}

const void WARN() {
    cout << "WARNING: ";
}