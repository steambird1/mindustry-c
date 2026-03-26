/*
time.h : Compatible with C time.h time() function.
*/

#ifndef _TIME_H
#define _TIME_H

long time() {
    return floor((double)@time / 1000);
}

// You must guarantee that the time is positive number.
void printTime(long second) {
    if (second >= 60) {
        const long minute = floor(second / 60);
        if (minute < 10) {
            print("0");
        }
        print(minute,":");
        if ((second % 60) < 10) {
            print("0");
        }
        print(second % 60);
    } else if (second < 10) {
        print("00:0", second);
    } else {
        print("00:", second);
    }
}

#endif