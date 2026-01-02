# Examples for Mindustry-C

If you have already learnt C or C++, it will be easy for you to get started with Mindustry-C.

The only differences are:

- **String literals are of type `char` instead of `char *`**;
- Pointers take up 2 bytes of space;
- There's no dynamic memory allocator (you must implement one if you need, so far);
- There are special grammar features to declare connected buildings.

Consider studying two examples below (I can't spare time to write detailed explanations right away.)

## Example 1: Scatter's Shooting

```c++
// Example 1: Scatter shooting any air target

auto device scatter1;

void main() {
    int order = 1;
    device former = null;
    while (true) {
        device target = radar("flying", "any", "any", "distance", scatter1, order++);
        control("shootp", scatter1, target, 1);
        if (target == former) break;
    }
}
```

## Example 2: Poly collecting the material you lack (either copper or lead)

```c++
// Example 2: Let poly tirelessly collect copper or lead (depends on which you lacks)

auto device message1;

int coreX, coreY;
bool coreFound;
device coreDevice;

// Notice that pure strings are of 'char' type.
void output(char info) {
    print(info);
    printflush(message1);
}

void movingTo(int x, int y) {
    ucontrol("move", x, y);
    while (true) {
        const int curX = floor(sensor(@unit, @x));
        const int curY = floor(sensor(@unit, @y));
        //print(curX,",",curY,"->",x,",",y);
        output("Waiting poly to move to destination...");
        if (abs(curX - x) < 3 && abs(curY - y) < 3) break;
    }
}

void mining(int type, int storage, float deadline) {
    int oreX, oreY;
    bool oreFound;
    device oreDevice;
    ulocate("ore", type, 
        (volatile int*)oreX, 
        (volatile int*)oreY, 
        (volatile bool*)oreFound,
        (volatile device*)oreDevice);  // Use volatile pointer to let data fed into it
    if (oreFound) {
        output("Instructing poly to move...");
        movingTo(oreX, oreY);
        output("Now mining...");
        ucontrol("mine", oreX, oreY);
        wait(0.1);
        int mined = 0;
        const float startTime = @time;
        while (mined < storage && (@time - startTime) <= deadline) {
            mined = sensor(@unit, type);
            print("Now mined:", mined);
            printflush(message1);
            ucontrol("idle");
        }
        movingTo(coreX, coreY);
        ucontrol("itemDrop", coreDevice, storage);
    } else {
        output("No! there's no such mineral in this graph.");
    }
}

void main() {
    output("Binding poly...");
    ubind(@poly);
    wait(3);
    ulocate("building", "core", false,
        (volatile int*)coreX,
        (volatile int*)coreY,
        (volatile int*)coreFound,
        (volatile device*)coreDevice);
    if (!coreFound) {
        output("No! there's no core found by the logical processor.");
        return;
    }
    const int maxCapacity = 30;
    const float maxTimeout = 40 * 1000;
    if (sensor(coreDevice, @copper) < sensor(coreDevice, @lead)) {
        mining(@copper, maxCapacity, maxTimeout);
    } else {
        mining(@lead, maxCapacity, maxTimeout);
    }
    //mining(@copper, 30, 40000.0);
}
```
