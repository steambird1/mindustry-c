/*
Simple carrier. Only control single unit for each time (to simplify operations.)
*/
auto device container1;

const long mySymbol = ((long)@thisx * 1000) + (long)@thisy;
const int threshold = 2;
const int maxCarry = 20;
const float timeout = 15;
const content_t ctrlUnit = @mono;
const device vault = container1;

float lastAlive = 0;

int coreX, coreY, vaultX, vaultY;
bool coreFound = false;
device coreDevice;

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

void moveTo(int x, int y) {
    while (true) {
        ensureAlive();
        const int cx = sensor((device)@unit, @x);
        const int cy = sensor((device)@unit, @y);
        ucontrol("move", x, y);
        if (abs(cx-x) + abs(cy-y) < threshold) break;
    }
}

void dropAll() {
    while ((int)sensor(@unit, @totalItems) > 0) {
        ucontrol("itemDrop", coreDevice, maxCarry);
    }
}

void doCarry(content_t carrying) {
    ensureAlive();
    if ((content_t)sensor(@unit, @firstItem) != null) {
        moveTo(coreX, coreY);
        dropAll();
    }
    moveTo(vaultX, vaultY);
    ucontrol("itemTake", vault, carrying, maxCarry);
    wait(0.2);
    moveTo(coreX, coreY);
    dropAll();
}

void main() {
    vaultX = (int)sensor(vault, @x);
    vaultY = (int)sensor(vault, @y);
    ensureAlive();
    ulocate("building", "core", false,
            (volatile int*)coreX,
            (volatile int*)coreY,
            (volatile int*)coreFound,
            (volatile device*)coreDevice);
    while (true) {
        doCarry(@graphite);
        doCarry(@silicon);
    }
}