#include "mindc.h"

auto device display1;
auto device reconstructor1;

const device dispOut = display1;
const device factoryIn = reconstructor1;
int factoryX, factoryY;

const double timeoutInterval = 45000.0; // If not arriving after 45 sec.

void outputState(int status, content_t unitType) {
    draw("clear", 0, 0, 0);
    draw("color", (status <= 1) ? 255 : 0, (status >= 1) ? 255 : 0, (status >= 3) ? 255 : 0, 255);
    draw("poly", 15, 15, 50, 15, 0);
    draw("color", 255, 255, 255, 255);
    if (unitType != null) {
        draw("image", 15, 45, unitType, 30, 0);
    }
    drawflush(dispOut);
}

bool checkDemand() {
    if ((int)sensor(factoryIn, @silicon) < 130) return false;
    if ((int)sensor(factoryIn, @titanium) < 80) return false;
    if ((int)sensor(factoryIn, @metaglass) < 40) return false;
    return true;
}

bool attemptUnit(content_t unitType) {
    bool isApproaching;
    ubind(unitType);
    const double startingTime = @time;
    outputState(2, unitType);
    while (true) {
        if ((bool)sensor(@unit, @dead)) return false;
        ucontrol("within", factoryX, factoryY, 2, (volatile bool*)isApproaching);
        if (isApproaching) break;
        ucontrol("pathfind", factoryX, factoryY);
        if ((@time - startingTime) > timeoutInterval) return false;
    }
    ucontrol("payEnter");
    outputState(3, unitType);
    wait(4);
    return true;
}

void main() {
    factoryX = (int)sensor(factoryIn, @x);
    factoryY = (int)sensor(factoryIn, @y);
    while (true) {
        const int pCount = (int)sensor(factoryIn, @payloadCount);
        if (pCount > 0) {
            outputState(0, (content_t)sensor(factoryIn, @payloadType));
        } else if (checkDemand()) {
            if (attemptUnit(@horizon)) continue;
            if (attemptUnit(@poly)) continue;
            outputState(2, null);
        } else {
            outputState(1, null);
        }
    }
}