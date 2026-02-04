
auto device display1;

const int objs[4] = {(int)@lead, (int)@silicon, (int)@graphite, (int)@copper};
const int osize = 4;

void main() {
    int i;
    for (i = 0; i < osize; i++) {
        draw("clear", 0, 0, 0);
        draw("image", 20, 20, lookup("item", objs[i]), 32, 0);
        drawflush(display1);
        wait(1);
    }
}