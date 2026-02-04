// Example 5:
// Control units to carry particular object (with timeout detector)

auto device switch1;    // Whether enabling this controller
auto device switch2;    // Whether using "balanced mode"
auto device switch3;    // Turned on - move away; Turned off - move in
auto device vault1;
auto device message1;   // Information display

const int *allow_units = {(int)(@poly)};
const int allow_units_size = 1;
const int *deploy_objects = {(int)(@thorium), (int)(@graphite)};
const int deploy_objects_size = 2;

const device vault_device = vault1; // You may replace it by sth else
const int default_carry = 50;

const int inf = 1000000000, max_drop = 999;
const float timeout = 20000.0;

float beginning;
int vaultX, vaultY;
int coreX, coreY, core_found;
device core_device;

void output(char info) {
    print(info);
    printflush(message1);
}

bool find_avail() {
    static int searched = 0;
    content_t unit = lookup("unit", allow_units[searched]);
    print("Attempt to bind:");
    print(unit);
    printflush(message1);
    searched = (searched + 1) % allow_units_size;
    ubind(unit);
    return (@unit != null);
    //if (@unit == null) return false;
    //return !((bool)sensor((device)@unit, @controlled));
}

void dead_detect() {
    if (@unit == null) {
        output("Connection lost!");
        wait(0.5);
        end();
    }
    if ((float)@time - beginning > timeout) {
        output("Time out!");
        wait(0.5);
        end();
    }
    if ((float)sensor(@unit, @health) <= 0) {
        end();
    }
}

void moving_to(int x, int y) {
    beginning = (float)@time;
    ucontrol("move", x, y);
    while (true) {
        dead_detect();
        const int curX = floor(sensor(@unit, @x));
        const int curY = floor(sensor(@unit, @y));
        output("Waiting the unit to move to destination...");
        if (abs(curX - x) < 3 && abs(curY - y) < 3) break;
    }
}

// Ensure binding before calling this
void carry(device from, device to, content_t type, int count) {
    const int fromX = (int)sensor(from, @x), fromY = (int)sensor(from, @y);
    const int toX = (int)sensor(to, @x), toY = (int)sensor(to, @y);
    output("Moving to source...");
    moving_to(fromX, fromY);
    output("Trying to take item...");
    ucontrol("itemTake", from, type, count);
    output("Moving to target...");
    moving_to(toX, toY);
    output("Trying to drop item...");
    ucontrol("itemDrop", to, count);
}

bool carry_in(bool balanced) {
    int mins = inf, i, minc = -1;
    for (i = 0; i < deploy_objects_size; i++) {
        const int deps = deploy_objects[i];
        content_t cont = lookup("item", deps);
        int val = (int)sensor(vault_device, cont) - (int)sensor(core_device, cont);
        if (balanced && val >= 0) continue;
        if (val < mins) {
            mins = val; minc = deps;
        }
    }
    if (minc == -1) return false;
    carry(core_device, vault_device, lookup("item", minc), balanced ? -mins : default_carry);
    return true;
}

bool carry_away(bool balanced) {
    int mins = -inf, i, minc = -1;
    for (i = 0; i < deploy_objects_size; i++) {
        const int deps = deploy_objects[i];
        content_t cont = lookup("item", deps);
        int val = (int)sensor(vault_device, cont) - (int)sensor(core_device, cont);
        if (balanced && val <= 0) continue;
        if (val > mins) {
            mins = val; minc = deps;
        }
    }
    if (minc == -1) return false;
    carry(vault_device, core_device, lookup("item", minc), balanced ? mins : default_carry);
    return true;
}

void main() {
    if (!((bool)sensor(switch1, @enabled))) {
        if (@unit != null) {
            ucontrol("unbind");
        }
        output("The system is shut down!");
        end();
    }
    if ((!allow_units_size) || (!deploy_objects_size)) {
        output("Please specify at least one allowed unit and size!");
        end();
    }
    vaultX = sensor(vault_device, @x);
    vaultY = sensor(vault_device, @y);
    output("Bind possible unit...");
    while (!find_avail()) {
        wait(0.5);
        output("Binding possible unit...");
    }
    beginning = (float)@time;
    ulocate("building", "core", false,
        (volatile int*)coreX,
        (volatile int*)coreY,
        (volatile int*)core_found,
        (volatile device*)core_device);
    while ((int)sensor(@unit, @totalItems) > 0) {
        moving_to(coreX, coreY);
        output("Prepare to drop items for further delivery...");
        ucontrol("itemDrop", core_device, max_drop);
    }
    if (!core_found) {
        output("No! there's no core found by the logical processor.");
        return;
    }
    const bool sw2 = sensor(switch2, @enabled);
    if (sw2) {
        output("Starting to balance...");
        if (carry_in(true)) return;
        carry_away(true);
    } else {
        if ((bool)sensor(switch3, @enabled)) carry_away(false);
        else carry_in(false);
    }
}