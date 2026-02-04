
auto device unloader1;
auto device vault1;

const int thre[4] = {200, 200, 200, 200};
const item_t objs[4] = {@lead, @silicon, @graphite, @copper};
const int osize = 4;

bool proc(device vault, device unloader, item_t obj, int thr) {
    int sens = (int)(sensor(vault, obj));
    wait(0.5);
    if (sens > thr) {
        control("enabled", unloader, true);
        control("configure", unloader, obj);
        return true;
    } else {
        return false;
    }
}

void main() {
    int i;
    bool done = false;
    for (i = 0; i < osize; i++) {
        if (proc(vault1, unloader1, lookup("item", objs[i]), thre[i])) {
            done = true;
            break;
        }
    }
    if (!done) {
        control("enabled", unloader1, false);
    }
}