auto device switch1;
auto device message1;

const long mySymbol = ((long)@thisx * 1000) + (long)@thisy;

device du;          // you
const content_t ctrlUnit = @mono;
const device aus = message1;

device targetVault;

struct coord {
    int x; int y;
};

coord coordBound, storageBound;

const device bindingSwitch = switch1;


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

bool inBinding() {
    return (bool)sensor(bindingSwitch, @enabled);
}

float mht(coord x, coord y) {
    return abs(x.x-y.x)+abs(x.y-y.y);
}

coord binding() {
    if (inBinding()) {
        du = null;
        print("Binding stage 1: please let processor detect you...");
        printflush(aus);
        while (du == null) {
            if (!inBinding()) break;
            du = radar("player", "ally", "any", "distance", @this, 1);
        }
        print("Binding stage 2: please move to target position and stay for 5s...");
        printflush(aus);
        coord last;
        last.x = last.y = 0;
        float lastSteady = (float)@time + 10.0;
        const float vibrationThreshold = 2;
        while (true) {
            coord jetzt;// = {floor(sensor(du, @x)), floor(sensor(du, @y))};
            jetzt.x = floor(sensor(du, @x)); jetzt.y = floor(sensor(du, @y));
            if (mht(last, jetzt) > vibrationThreshold) {
                print("Binding stage 2: please move to target position and stay for 5s... (you are moving)");
                printflush(aus);
                last = jetzt;
                lastSteady = (float)@time;
            } else {
                const int remainingTime = 5 - floor(((float)@time - lastSteady) / 1000.0);
                print("Binding stage 2: please stay... will use (",jetzt.x,",",jetzt.y,") in ",remainingTime,"s");
                printflush(aus);
                if (remainingTime <= 0) break;
            }
        }
        print("Binding stage 3: prepare for ",ctrlUnit," to bind storage...");
        printflush(aus);
        ensureAlive();
        movingTo(last.x, last.y);
        bool storageFound = false;
        device currentVault;
        while (!storageFound) {
            print("Binding stage 3: letting ",ctrlUnit," bind storage...");
            printflush(aus);
            ulocate("building", "storage", false, (volatile int*)storageBound.x, (volatile int*)storageBound.y, (volatile bool*)storageFound, (volatile device*)targetVault);
        }
        targetVault = currentVault;
        print("Binding successful!");
        printflush(aus);
        return coordBound = last;
    } else {
        return coordBound;
    }
}

void main() {
    coordBound.x = (long)@thisx; coordBound.y = (long)@thisy;
    while (true) {
        binding();
        print("Bound: (",coordBound.x,",",coordBound.y,"); storage: (",storageBound.x,",",storageBound.y,")");
        printflush(aus);
    }
}