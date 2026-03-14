/*
math.h : Basic math support.

This file contains some functions that is not implemented by op instruction.
*/

#ifndef _MATH_H
#define _MATH_H

double distance(double x1, double y1, double x2, double y2) {
    return sqrt(pow(x2-x1,2)+pow(y2-y1,2));
}

#endif