#include "mindc.h"

#define MAX_UNITS 24

auto device vault1, display1;

const near item_t *mineTargets = {@titanium};
const unsigned mineTargetLen = 1;
// 1 - mono-ok, 2 - poly-ok, 4 - mega-ok
const near unsigned *mineRequire = {4};
const near unsigned *mineMax = {2};

const near item_t *carryTargets = {@titanium};
const unsigned carryTargetLen = 0, carryCat = 2;
const near unsigned *carryMax = {2};

const device vaultDevice = vault1;
const device displayDevice = display1;
device coreDevice;
int coreX, coreY, vaultX, vaultY;
// Carrying in order: mega -> poly -> mono

int currentFlag;
int currentAffair[MAX_UNITS];
pdlist_t currentBound = pdcreate(MAX_UNITS);

void reportSystemState(int currentId, char description, device unitImage, content_t itemImage) {
    draw("clear", 0, 0, 0);
    draw("color", 255, 255, 255, 255);
    printflush(null);
    print(currentId);
    draw("print", 5, 5, @bottomLeft);
    print(description);
    draw("print", 5, 15, @bottomLeft);
    if (unitImage != null) {
        draw("image", 5, 35, (content_t)sensor(unitImage, @type), 15, 0);
        print(ceil((double)sensor(unitImage, @health) / 10.0));
        draw("print", 5, 50, @bottomLeft);
        print(floor((double)sensor(unitImage, @x)),",",floor((double)sensor(unitImage, @y)));
        draw("print", 5, 61, @bottomLeft);
    }
    if (itemImage != null) {
        draw("image", 35, 35, itemImage, 15, 0);
        if (unitImage != null) {
            print((int)sensor(unitImage, itemImage));
            draw("print", 35, 50, @bottomLeft);
        }
    }
    drawflush(displayDevice);
}

bool suitRequire(device currentDevice, unsigned requirement) {
    if ((bool)sensor(currentDevice, @dead)) return false;
    if ((int)sensor(currentDevice, @flag) != currentFlag) return false;
    content_t category = (content_t)sensor(currentDevice, @type);
    if (category == @mono && (requirement & 1)) return true;
    if (category == @poly && (requirement & 2)) return true;
    if (category == @mega && (requirement & 4)) return true;
    return false;
}

device tryBinding(content_t category) {
    const int maxAttempt = 15;
    int i;
    for (i = 0; i < maxAttempt; ++i) {
        ubind(category);
        if (((bool)sensor(@unit, @dead))) continue;
        const int curFlag = (int)sensor(@unit, @flag);
        if (curFlag != 0 && curFlag != currentFlag) continue;
        ucontrol("flag", currentFlag);
        return @unit;
    }
    return (device)null;
}

device tryBindingFor(unsigned requirement) {
    device result;
    if (requirement & 1) {
        if ((result = tryBinding(@mono)) != null) return result;
    }
    if (requirement & 2) {
        if ((result = tryBinding(@poly)) != null) return result;
    }
    if (requirement & 4) {
        if ((result = tryBinding(@mega)) != null) return result;
    }
    return null;
}

void updateCore() {
    bool coreFound;
    if ((bool)sensor(coreDevice, @dead)) {
        ulocate("building", "core", false, (volatile int*)coreX, (volatile int*)coreY, (volatile bool*)coreFound, (volatile device*)coreDevice);
    }
}

bool getReady(unsigned req, int currentId) {
    device currDev = (device)pdread(currentBound, currentId);
    if (!suitRequire(currDev, req)) {
        currDev = tryBindingFor(req);
        if (currDev == null) return false;
        pdwrite(currentBound, currentId, currDev);
    }
    ubind(currDev);
    updateCore();
    return true;
}

void deployToVault(int currentId, content_t unitItem) {
    bool coreWithin;
    const int unitItemCapacity = (int)sensor(@unit, @itemCapacity);
    reportSystemState(currentId, "Storing", @unit, unitItem);
    ucontrol("pathfind", vaultX, vaultY);
    ucontrol("within", vaultX, vaultY, 6.5, (volatile bool*)coreWithin);
    if (coreWithin) {
        ucontrol("itemDrop", vaultDevice, unitItemCapacity);
    }
}

bool checkStorage(int currentId, content_t mineType) {
    const int unitItemCapacity = (int)sensor(@unit, @itemCapacity);
    const int unitItemCurr = (int)sensor(@unit, @totalItems);
    const content_t unitItemFirst = (content_t)sensor(@unit, @firstItem);
    bool coreWithin;
    if (unitItemFirst != null) {
        if (unitItemFirst != mineType) {
            reportSystemState(currentId, "Cleaning", @unit, unitItemFirst);
            ucontrol("pathfind", coreX, coreY);
            ucontrol("within", coreX, coreY, 5.5, (volatile bool*)coreWithin);
            if (coreWithin) {
                ucontrol("itemDrop", coreDevice, unitItemCapacity);
            }
            return true;
        } else if (unitItemCurr >= unitItemCapacity) {
            deployToVault(currentId, mineType);
            return true;
        }
    }
    return false;
}

void main() {
    vaultX = (int)sensor(vaultDevice, @x);
    vaultY = (int)sensor(vaultDevice, @y);
    currentFlag = ((int)sensor(@this, @x) * @maph) + (int)sensor(@this, @y);

    int i, j, currentId;
    while (true) {
        currentId = 0;
        for (i = 0; i < mineTargetLen; ++i) {
            const content_t mineType = mineTargets[i];
            const unsigned req = mineRequire[i];
            for (j = 0; j < mineMax[i]; ++j, ++currentId) {
                if (getReady(req, currentId)) {
                    // Check capacity
                    if (checkStorage(currentId, mineType)) continue;
                    int oreX, oreY;
                    bool oreFound, oreWithin;
                    char state = "Mining: no ore";
                    ulocate("ore", mineType, (volatile int*)oreX, (volatile int*)oreY, (volatile bool*)oreFound, null);
                    if (oreFound) {
                        ucontrol("pathfind", oreX, oreY);
                        state = "Mining: moving";
                        ucontrol("within", oreX, oreY, 3.5, (volatile bool*)oreWithin);
                        if (oreWithin) {
                            ucontrol("mine", oreX, oreY);
                            state = "Mining: started";
                        }
                    }
                    reportSystemState(currentId, state, @unit, mineType);
                } else {
                    reportSystemState(currentId, "Binding failed", null, null);
                }
            }
        }
        for (i = 0; i < carryTargetLen; ++i) {
            const int unitItemCapacity = (int)sensor(@unit, @itemCapacity);
            const content_t carryType = carryTargets[i];
            for (j = 0; j < carryMax[i]; ++j, ++currentId) {
                if (getReady(carryCat, currentId)) {
                    if (checkStorage(currentId, carryType)) continue;
                    char state;
                    bool coreWithin;
                    ucontrol("pathfind", coreX, coreY);
                    state = "Fetching: moving";
                    ucontrol("within", coreX, coreY, 3.5, (volatile bool*)coreWithin);
                    if (coreWithin) {
                        ucontrol("itemTake", coreDevice, carryType, unitItemCapacity);
                        state = "Fetching: taking";
                    }
                    reportSystemState(currentId, state, @unit, carryType);
                } else {
                    reportSystemState(currentId, "Binding failed", null, null);
                }
            }
        }
    }
}