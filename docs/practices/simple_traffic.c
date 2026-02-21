auto device unloader1, unloader2;

void controlAs(device dev, bool state) {
    control("enabled", dev, state);
}

void main() {
    while (true) {
        controlAs(unloader1, false);
        controlAs(unloader2, false);
        wait(10);
        controlAs(unloader1, true);
        controlAs(unloader2, true);
        wait(4.5);
    }
}