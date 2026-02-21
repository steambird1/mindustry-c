// Example 2: Let poly tirelessly collect copper or lead (depends on which you lacks)
auto device container1;
auto device container2;

const long mySymbol = ((long)@thisx * 1000) + (long)@thisy;
const content_t ctrlUnit = @mono;

int containerX, containerY;
const device containerDevice = container2;

const content_t carrying = @silicon;
const device vault = container1;

int coreX, coreY, vaultX, vaultY;
bool coreFound = false;
device coreDevice;

void ensureAlive() {
    // const float curTime = (float)@time;
    // if (curTime - lastAlive > timeout) {
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
        const int curX = floor(sensor(@unit, @x));
        const int curY = floor(sensor(@unit, @y));
        ucontrol("move", x, y);
        //print(curX,",",curY,"->",x,",",y);
        // output("Waiting poly to move to destination...");
        if (abs(curX - x) < 3 && abs(curY - y) < 3) break;
    }
}

void mining(content_t type, int storage, float deadline) {
    int oreX, oreY;
    bool oreFound;
    device oreDevice;
    ensureAlive();
    ulocate("ore", type, 
        (volatile int*)oreX, 
        (volatile int*)oreY, 
        (volatile bool*)oreFound,
        (volatile device*)oreDevice);  // Use volatile pointer to let data fed into it
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
        movingTo(containerX, containerY);
        while ((int)sensor(@unit, type) > 0) {
            ucontrol("itemDrop", containerDevice, storage);
        }
        
    }
}

void main() {
    containerX = (int)sensor(containerDevice, @x);
    containerY = (int)sensor(containerDevice, @y);
    vaultX = (int)sensor(vault, @x);
    vaultY = (int)sensor(vault, @y);
    ensureAlive();
    ulocate("building", "core", false,
            (volatile int*)coreX,
            (volatile int*)coreY,
            (volatile int*)coreFound,
            (volatile device*)coreDevice);
    const int maxCapacity = 20;
    const float maxTimeout = 12 * 1000;
    while (true) {
        while ((int)sensor(vault, carrying) < maxCapacity * 2) {
            mining(@scrap, maxCapacity, maxTimeout);
        }
        movingTo(vaultX, vaultY);
        ucontrol("itemTake", vault, carrying, maxCapacity);
        wait(0.2);
        movingTo(coreX, coreY);
        ucontrol("itemDrop", coreDevice, maxCapacity);
        wait(0.2);
    }
}