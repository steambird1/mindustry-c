#include "mindc.h"

#define TOTAL_ITEMS 24

auto device display1;
auto device foundation1;

const device displayOut = display1, nucleus = foundation1;

near int lastCorr10s[TOTAL_ITEMS], lastCorr20s[TOTAL_ITEMS];
double lastData10s, lastData20s;
int topMessage = 0;

void updateData(near int *target) {
    int i;
    for (i = 0; i < @itemCount; ++i) {
        target[i] = (int)sensor(nucleus, (item_t)i);
    }
}

void printValues(int atY, int current, int diff5s, int diff20s) {
    printflush(null);
    print(current);
    draw("print", 20, atY, @bottomLeft);
    if (diff5s >= 0) {
        draw("color", 0, 255, 0, 255);
        print("+");
    }
    else draw("color", 255, 0, 0, 255);
    print(diff5s);
    draw("print", 40, atY, @bottomLeft);
    if (diff20s >= 0) {
        draw("color", 0, 255, 0, 255);
        print("+");
    }
    else draw("color", 255, 0, 0, 255);
    print(diff20s);
    draw("print", 55, atY, @bottomLeft);
    draw("color", 255, 255, 255, 255);
}

void main() {
    while (true) {
        if (@time > lastData10s + 10) {
            updateData(lastCorr10s);
            lastData10s = @time;
        }
        if (@time > lastData20s + 20) {
            updateData(lastCorr20s);
            lastData20s = @time;
        }
        draw("clear", 0, 0, 0);
        draw("color", 255, 255, 255, 255);
        int latestTop = (topMessage + 1) % @itemCount, currY = 20;
        bool labeling = true;
        while (topMessage != latestTop) {
            const content_t currentItem = (item_t)latestTop;
            const int current = (int)sensor(nucleus, currentItem);
            const int last10s = lastCorr10s[latestTop], last20s = lastCorr20s[latestTop];
            const int diff10s = current - last10s, diff20s = current - last20s;
            if (current > 0 && diff10s >= 0 && diff20s >= 0) {
                if (labeling) {
                    draw("image", 5, 5, currentItem, 15, 0);
                    printValues(5, current, diff10s, diff20s);
                    topMessage = latestTop;
                    labeling = false;
                }
            } else if (diff10s < 0 || diff20s < 0) {
                draw("image", 5, currY, currentItem, 15, 0);
                printValues(currY, current, diff10s, diff20s);
                currY += 15;
            }
            latestTop = (latestTop + 1) % @itemCount;
        }
        drawflush(displayOut);
    }
}