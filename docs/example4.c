
auto device display1;

const item_t objs[4] = {@lead, @silicon, @graphite, @copper};
const int osize = 4;

void main() {
    int i;
    for (i = 0; i < osize; i++) {
        draw("clear", 0, 0, 0);
        draw("image", 20, 20, objs[i], 32, 0);
        drawflush(display1);
        wait(1);
    }
}