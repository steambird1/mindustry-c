#include "mindc.h"

auto device reconstructor1;
auto device router1;
auto device conveyor1;
auto device unloader1;

const device target = reconstructor1, inbound = router1, outbound = conveyor1, installer = unloader1;

void checkFeed(content_t itemType, int required) {
    if ((int)sensor(target, itemType) < required) {
        control("configure", installer, itemType);
        while ((int)sensor(inbound, itemType) != (int)sensor(inbound, @totalItems)) {
            control("enabled", outbound, true);
        }
        control("enabled", outbound, false);
        end();
    }
}

void main() {
    checkFeed(@silicon, 850);
    checkFeed(@titanium, 750);
    checkFeed(@plastanium, 650);
}