// Example 7a: Simplified version
// (with only unit binding)
// Automatically gather specified units around the player.
// Note: this is actually a group control (although what it does is randomly)
// choose a number of members each time

auto device message1;
//auto device message2;
auto device switch1;

const float gatherRadius = 5.0;
// Type of units to bind
const unit_t* bindings = {@flare};
const int* bindLimit = {3};
const int bindingsCount = 1;    // Maximum 10

const int serial = ((int)@thisX) * 1000 + ((int)@thisY);
int currentBinds[10];           // The number of current binding (might be displayed)

bool terminate() {
    return !sensor(switch1, @enabled);
}

void output(device target, char info) {
    print(info);
    printflush(target);
}

void autoStop() {
    if (terminate()) {
        output(message1, "The system is shutting down!");
        end();
    }
}

// Every time it is called, it processes something.
void execute() {
    if (terminate()) {
        ucontrol("unbind");
        return;
    }
    const device player = uradar("player", "ally", "any", "distance", 0, 1);
    const int playerX = sensor(player, @x);
    const int playerY = sensor(player, @y);
    ucontrol("approach", playerX, playerY, gatherRadius);
}

// This function will return when and only when bindings are completed
void bindUnits() {
    int i;
    bool working = true;
    for (i = 0; i < bindingsCount; i++) {
        // Record the number of binding (if bindLimit == -1 then no limit)
        const int bindRestriction = bindLimit[i];
        const content_t currentBind = bindings[i];
        if (bindRestriction < 0) continue;
        print("Binding:",currentBind);
        printflush(message1);
        int count = 0;
        device firstUnit = null;
        while (count < bindRestriction && (count <= 1 || (device)@unit != firstUnit)) {
            autoStop();
            ubind(currentBind);
            if (@unit == null) {
                output(message1, "Binding failed!");
                break;
            }
            const bool dead_state = (bool)sensor(firstUnit, @dead);
            if (dead_state) {
                output(message1, "Lost contact with the first unit! rebinding...");
                firstUnit = (device)@unit; count = 1; continue;
            }
            const device current = (device)@unit;
            if ((bool)sensor(current, @controlled) && ((int)sensor(current, @flag) != serial)) {
                output(message1, "Resolved conflict with another controller's unit!");
                ucontrol("unbind");
                continue;
            }
            ucontrol("flag", serial);
            print("Bound ",count," units of:",currentBind);
            printflush(message1);
            execute();
            count++;
        }
        currentBinds[i] = count;
        autoStop();
    }
}

void main() {
    // It is recommended to use such loop to skip initialization
    if (bindingsCount > 10) {
        output(message1, "Too many bindings!");
        return;
    }
    while (true) {
        bindUnits();
    }
}