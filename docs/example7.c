// Example 7:
// Specified army will automatically catch the combat
// (If the combat are gone, their first occurrence will be recorded)
auto device message1;
auto device message2;
auto device switch1;

const float hazardRadius = 50.0;
const float combatRadius = 5.0;

// Type of units to bind
const unit_t* bindings = {@flare};
const int* bindLimit = {-1};
const int bindingsCount = 1;    // Maximum 10

const int serial = ((int)@thisX) * 1000 + ((int)@thisY);
int currentBinds[10];           // The number of current binding (might be displayed)

bool preFetch = true;
bool coreFound, combatFound;
int coreX, coreY, combatX, combatY;
device coreDevice, combatDevice;

/*
Control flags:
1 - Force to execute enabled policy
2 - Enable 'readyForEnemy' policy
4 - Enable 'policyDirectToCombat' policy
8 - Enable 'policyCoreProtection' policy
*/
int controlFlags() {
    return 14;  // 2+4+8
}

bool terminate() {
    return !sensor(switch1, @enabled);
}

void output(device target, char info) {
    print(info);
    printflush(target);
}

// Policy: Ready for the enemy
void policyReadyForEnemy() {
    
}

// Policy: Just move to the enemy
void policyDirectToCombat() {

}

// Policy: Protect the core!
void policyCoreProtection() {

}

// Preprocess before executing instruction on every unit
void preprocess() {
    // If there are combats...
    

    device combat = uradar("enemy", "any", "any", "health", 1);

}

// Every time it is called, it processes something.
void execute() {
    if (preFetch) {
        // Prefetch core and combat information
        ulocate("building", "core", false, (volatile int*)coreX, (volatile int*)coreY, (volatile bool*)coreFound, (volatile device*)coreDevice);
        preFetch = false;
    }
    // Pre-locate must be executed
    ulocate("spawn", (volatile int*)combatX, (volatile int*)combatY, (volatile bool*)combatFound, (volatile device*)combatDevice);
}

// This function will return when and only when bindings are completed
void bindUnits() {
    int i;
    bool working = true;
    for (i = 0; i < bindingsCount; i++) {
        // Record the number of binding (if bindLimit == -1 then no limit)
        const int bindRestriction = bindLimit[i];
        if (bindRestriction < 0) continue;
        int count = 0;
        device firstUnit = null;
        while (count < bindRestriction && ubind(bindings[i]) != firstUnit) {
            if (terminate()) break;
            if (sensor(firstUnit, @dead)) {
                firstUnit = null; count = 0; continue;
            }
            const device current = (device)@unit;
            if (sensor(current, @controlled) && (sensor(current, @flag) != serial)) continue;
            ucontrol("flag", serial);
            execute();
            count++;
        }
        currentBinds[i] = count;
        if (terminate()) break;
    }
}

void main() {
    // It is recommended to use such loop to skip initialization
    if (bindingsCount > 10) {
        output(message1, "Too many bindings!");
        return;
    }
    while (true) {
        preprocess();
        bindUnits();
    }
}