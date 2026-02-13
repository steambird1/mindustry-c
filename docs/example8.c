// Example 8: Start RTG generator group in accordance with current
// This will use getlink() to control as many RTG generators as possible
// so there's no need to use pseudo list

auto device node1;
auto device display1;

const int enableThreshold = 2, disableThreshold = 4;
const int powerThreshold = 500;
const int closureThreshold = 1000;
const content_t target = @rtg_generator;
const int singleProvide = 270;  // Not really used
const int screenX = 80;
const int screenY = 80;
const int singleOperationClosure = 4;

near int displayState[20];  // 0 - no display, 1 - red, 2 - yellow, 4 - green, 8 - flashing
near int displayBuffer[20]; // 1 - flashing black

int currentDuration = 0, currentControlState = 0, targetControlState = 0;
int drawX = 0, drawY = 0;

long getPower() {
    return (long)sensor(node1, @powerNetIn) - (long)sensor(node1, @powerNetOut);
}

void main() {
    bool await;
    float awaitStart;
    while (true) {
        long power;
        power = getPower();
        await = false;
        currentDuration++;
        if (currentControlState == 0 && power <= powerThreshold) {
            if (targetControlState != 1) {
                currentDuration = 0;
            }
            targetControlState = 1;
            if (currentDuration > enableThreshold) {
                currentControlState = 1;
                currentDuration = 0;
            } else {
                await = true;
                awaitStart = (float)@time;
            }
        } else if (currentControlState == 1 && power > powerThreshold) {
            if (targetControlState != 0) {
                currentDuration = 0;
            }
            targetControlState = 0;
            if (currentDuration > disableThreshold) {
                currentControlState = 0;
                currentDuration = 0;
            } else {
                await = true;
                awaitStart = (float)@time;
            }
        }
        int i, total = 0, curClosure = 0;
        for (i = 0; ; i++) {
            const device building = getlink(i);
            if (building == null) break;
            if ((volatile content_t)sensor(building, @type) == target) {
                const int curPower = getPower();
                if (currentControlState == 1 || curPower < 0) {
                    control("enabled", building, true);
                } else if (((volatile bool)sensor(building, @enabled)) && curPower > closureThreshold && curClosure < singleOperationClosure) {
                    control("enabled", building, false);
                    curClosure++;
                }
                // Report its status (red: not enabled (red flashing: too hot and stopped!); yellow: no output; green: outputting)
                int state = 0;
                if ((volatile bool)sensor(building, @dead) || ((volatile long)sensor(building, @heat) > 0)) {
                    control("enabled", building, false);
                    state = 9;
                } else if (!((volatile bool)sensor(building, @enabled))) {
                    state = 1;
                } else if ((volatile float)sensor(building, @efficiency) < 0.5) {
                    state = 2;
                } else {
                    state = 4;
                }
                displayState[total] = state;
                total++;
            }
        }
        // Draw it
        drawX = 0, drawY = 0;
        draw("clear", 0, 0, 0);
        for (i = 0; i < total; i++) {
            const int state = displayState[i];
            if ((volatile bool)(state & 8)) {
                const int buf = displayBuffer[i];
                displayBuffer[i] = buf ^ 1;
                if ((volatile bool)buf) {
                    continue;
                }
            }
            if (state & 1) {
                draw("color", 255, 0, 0, 255);
            }
            if (state & 2) {
                draw("color", 255, 255, 0, 255);
            }
            if (state & 4) {
                draw("color", 0, 255, 0, 255);
            }
            draw("poly", drawX + 5, drawY + 5, 50, 5, 0);
            drawX += 10;
            if (drawX >= screenX) {
                drawX = 0;
                drawY += 10;
            }
        }
        drawflush(display1);
        if (await) {
            const float awaitEnd = awaitStart + 0.1;
            while ((float)@time < awaitEnd) {
                wait(0.01);
            }
        }
    }
}