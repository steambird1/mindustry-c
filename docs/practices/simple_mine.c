// Example 2: Let poly tirelessly collect copper or lead (depends on which you lacks)
auto device container1;

const long mySymbol = ((long)@thisx * 1000) + (long)@thisy;
const content_t ctrlUnit = @poly;

int coreX, coreY;
const device coreDevice = container1;

void ensureAlive() {
    if ((bool)sensor((device)@unit, @dead)) {
        while (true) {
            ubind(ctrlUnit);
            if ((bool)sensor((device)@unit, @dead)) continue;
            const long curFlag = (long)sensor((device)@unit, @flag);
            if (curFlag != 0 && curFlag != mySymbol) continue;
            ucontrol("flag", mySymbol);
            break;
        }
    }
}

void movingTo(int x, int y) {
    while (true) {
        ensureAlive();
        ucontrol("move", x, y);
        const int curX = floor(sensor(@unit, @x));
        const int curY = floor(sensor(@unit, @y));
        //print(curX,",",curY,"->",x,",",y);
        // output("Waiting poly to move to destination...");
        if (abs(curX - x) < 3 && abs(curY - y) < 3) break;
    }
}

void mining(content_t type, int storage, float deadline) {
    int oreX, oreY;
    bool oreFound;
    device oreDevice;
    ulocate("ore", type, 
        (volatile int*)oreX, 
        (volatile int*)oreY, 
        (volatile bool*)oreFound,
        (volatile device*)oreDevice);  // Use volatile pointer to let data fed into it
    movingTo(coreX, coreY);
    while ((int)sensor(@unit, @totalItems) > 0) {
        ucontrol("itemDrop", coreDevice, storage);
    }
    if (oreFound) {
        // output("Instructing poly to move...");
        movingTo(oreX, oreY);
        int mined = 0;
        const float startTime = (float)@time;
        while (mined < storage && ((float)@time - startTime) <= deadline) {
            ensureAlive();
            ucontrol("mine", oreX, oreY);
            mined = sensor(@unit, type);
        }
        movingTo(coreX, coreY);
        while ((int)sensor(@unit, @totalItems) > 0) {
            ucontrol("itemDrop", coreDevice, storage);
        }
        
    } else {
        // output("No! there's no such mineral in this graph.");
    }
}

void main() {
    /*
    output("Binding poly...");
    ubind(@poly);
    wait(3);
    ulocate("building", "core", false,
        (volatile int*)coreX,
        (volatile int*)coreY,
        (volatile int*)coreFound,
        (volatile device*)coreDevice);
        */
    coreX = (int)sensor(coreDevice, @x);
    coreY = (int)sensor(coreDevice, @y);
    const int maxCapacity = 30;
    const float maxTimeout = 10 * 1000;
    while (true) {
        ensureAlive();
        mining(@coal, maxCapacity, maxTimeout);
    }
}