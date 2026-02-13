/**
 * @requires pslist
*/
// I don't really have doc support right away.

// Example 7b: Partial "group controller"
// This is also a object delivery.

// The function is a little bit similar but the logic is different from
// example 5 as it must be "passive" (you can't always control an object).

// --- Begin of common part ---
auto device switch1;    // Whether enabling this controller
auto device switch2;    // Whether using "balanced mode"
auto device switch3;    // Turned on - move away; Turned off - move in
auto device vault1;
auto device message1;   // Information display

const near unit_t *allowUnits = {(unit_t)@poly};
const near int *allowMaxBind = {3};
const int allowUnitsSize = 1;
const near item_t *deployObjects = {(item_t)@thorium};
const int deployObjectsSize = 1;

// Must simultaneously modify them:
const pdlist_t unitList = pdcreate(25);
near int phase[25];
const int maxUnitBinding = 25;

const device vaultDevice = vault1; // You may replace it by sth else
const int defaultCarry = 50;

const float coreRadius = 4;

const int inf = 1000000000, maxDrop = 999;
const float timeout = 20000.0;

const int serial = (long)@thisx * 1000 + (long)@thisy;

// --- End of configuration --

int bindHead = -1;

bool preFetchWork = true;
float beginning;
int vaultX, vaultY;
int coreX, coreY, coreFound;
device coreDevice;
near int *phaseAccess;

void output(char info) {
    print(info);
    printflush(message1);
}

bool autoStop() {
    if (!((volatile bool)sensor(switch1, @enabled))) {
        output("System is shut down!");
        end();
    }
}

bool sein(device target) {
    return !((volatile bool)sensor(target, @dead));
}

bool controllable(device target) {
    return sein(target) && ((int)sensor(target, @flag) == serial);
}
// --- End of common part ---


// Must have unit bound
void preFetch() {
    if (preFetchWork) {
        ulocate("building", "core", false,
            (volatile int*)coreX,
            (volatile int*)coreY,
            (volatile int*)coreFound,
            (volatile device*)coreDevice);
        preFetchWork = false;
    }
}

// To keep the unit control alive, this will send control instruction!
bool isApproaching(int x, int y, float r) {
    bool state;
    ucontrol("within", x, y, r, (volatile bool*)state);
    return state;
}

// Returning of direction: 1 - vault to core, 0 - core to vault
void considerOperation(item_t *selection, int *direction) {
    const bool balancing = (volatile bool)sensor(switch2, @enabled);
    near item_t *it = deployObjects;
    int i;
    // Small number means that core has more
    int minImbal = inf, maxImbal = -inf;
    item_t minImbalItem, maxImbalItem;
    for (i = 0; i < deployObjectsSize; i++, it++) {
        const near item_t currentObj = *it;
        const int sensoredCount = (int)sensor(vaultDevice, currentObj) 
                - (balancing ? (int)sensor(coreDevice, currentObj) : 0);
        const bool minCond = sensoredCount < minImbal, maxCond = sensoredCount > maxImbal;
        // Using '?:' for higher versions helps with speeding up calculations
        minImbal = minCond ? sensoredCount : minImbal;
        minImbalItem = minCond ? currentObj : minImbalItem;
        maxImbal = maxCond ? sensoredCount : maxImbal;
        maxImbalItem = maxCond ? currentObj : maxImbalItem;
        // debug
        //print("Sensoring: ",(content_t)currentObj,", got ",sensoredCount);
        //printflush(message1);
        //wait(0.25);
    }
    if (balancing) {
        const bool maxImbalGreater = maxImbal > 0;
        *selection = maxImbalGreater > 0 ? maxImbalItem : minImbalItem;
        *direction = maxImbalGreater > 0 ? 1 : 0;
    } else {
        const bool isMovingAway = (bool)sensor(switch3, @enabled);
        *selection = isMovingAway ? maxImbalItem : minImbalItem;
        *direction = isMovingAway ? 1 : 0;
    }
}

int runPhase(int currentPhase, item_t currentSelection, int currentDirection) {
    const bool apprCore = isApproaching(coreX, coreY, coreRadius);
    const bool apprVault = isApproaching(vaultX, vaultY, coreRadius);
    //print("Execution begun, phase: ",currentPhase, "approachState: ",apprCore,";",apprVault);
    //printflush(message1);

    // State machine
    /*
    State -1: Initial binding
    State 0: Idle (default entry)
    States are on-the-way progress
    State 1: Waiting for core unload
    State 2: Waiting for core load
    State 3: Waiting for vault unload
    State 4: Waiting for vault load
    */
    if (currentPhase < 0) {
        if ((int)sensor(@unit, @totalItems) > 0) {
            ucontrol("move", coreX, coreY);
            currentPhase = 1;
        } else {
            currentPhase = 0;
        }
    }

    if (currentPhase == 1) {
        // Core unload
        if (apprCore) {
            ucontrol("itemDrop", coreDevice, maxDrop);
            currentPhase = 0;
        } else {
            ucontrol("move", coreX, coreY);
        }
    }

    if (currentPhase == 2) {
        // Core load = vault unload (but it is possible to give up current operation)
        if (apprCore) {
            ucontrol("itemTake", coreDevice, currentSelection, defaultCarry);
            wait(0.25);
            ucontrol("move", vaultX, vaultY);
            currentPhase = 3;
        } else {
            ucontrol("move", coreX, coreY);
        }
    }

    if (currentPhase == 3) {
        // Vault unload
        if (apprVault) {
            ucontrol("itemDrop", vaultDevice, maxDrop);
            currentPhase = 0;
        } else {
            ucontrol("move", vaultX, vaultY);
        }
    }

    if (currentPhase == 4) {
        // Vault load = Core unload (but it is possible to give up current operation)
        if (apprVault) {
            ucontrol("itemTake", vaultDevice, currentSelection, defaultCarry);
            wait(0.25);
            ucontrol("move", coreX, coreY);
            currentPhase = 1;
        } else {
            ucontrol("move", vaultX, vaultY);
        }
    }

    if (currentPhase == 0) {
        if (currentDirection == 1) {
            // Vault-to-core, to vault first
            ucontrol("move", vaultX, vaultY);
            currentPhase = 4;
        } else {
            // Core-to-vault, to core first
            ucontrol("move", coreX, coreY);
            currentPhase = 2;
        }
    }
    return currentPhase;
}

// Execution of unit
// TODO: Re-design the state machine!!!!!!!
// (The binding system is just working fine)
void execute(int id) {
    autoStop();
    if (!controllable((device)@unit)) return;
    preFetch();

    // Determine current operation mode
    // (This is just an example. For actual execution, it is recommended to
    // do it before all binding.)
    item_t currentSelection;
    int currentDirection;
    considerOperation(&currentSelection, &currentDirection);
    // debug
    //print("Decision: ",(content_t)currentSelection,", direction: ",currentDirection);
    //wait(0.25);

    int currentPhase = *phaseAccess;
    const int startPhase = currentPhase;

    currentPhase = runPhase(currentPhase, currentSelection, currentDirection);
    wait(0.2);
    currentPhase = runPhase(currentPhase, currentSelection, currentDirection);
    print("#",id,": Execution completed, phase: ",startPhase," -> ",currentPhase);
    printflush(message1);
    (*phaseAccess) = currentPhase;
}

void main() {
    int i, j, maxBind = 0;
    autoStop();
    output("Restarting...");
    vaultX = (int)sensor(vaultDevice, @x);
    vaultY = (int)sensor(vaultDevice, @y);
    for (i = 0; i < allowUnitsSize; i++) {
        for (j = 0; j < allowMaxBind[i]; j++) {
            phase[maxBind] = -1;    // Unknown state
            maxBind++;
        }
    }
    while (true) {
        phaseAccess = phase;
        int currentCategory = 0, currentRef = 0;
        for (i = 0; i < maxBind; i++, phaseAccess++, currentRef++) {
            autoStop();
            if (currentRef >= allowMaxBind[currentCategory]) {
                currentRef = 0;
                currentCategory++;
            }
            device currentDevice = (device)pdread(unitList, i);
            const unit_t currentUnitType = allowUnits[currentCategory];
            if (!controllable(currentDevice)) {
                print("Device at ",i," of ",currentUnitType," is uncontrollable. Trying to bind...");
                printflush(message1);
                // Try rebinding, unless...
                ubind(currentUnitType);
                print("Currently bound:",@unit);
                printflush(message1);
                bool ok = true;
                for (j = 0; j < maxBind; j++) {
                    if ((device)@unit == (device)pdread(unitList, j)) {
                        output("Already bound!");
                        ok = false; break;
                    }
                }
                if (!ok) continue;
                ucontrol("flag", serial);
                pdwrite(unitList, i, @unit);
                currentDevice = (device)@unit;
                if (!controllable(currentDevice)) {
                    (*phaseAccess) = -1;
                    output("Binding unsuccessful!");
                    continue;
                }
            } else {
                print("Reconnecting...");
                printflush(message1);
                ubind(currentDevice);
            }
            print("Bound ", currentUnitType, ", executing...");
            printflush(message1);
            execute(i);
        }
    }
}