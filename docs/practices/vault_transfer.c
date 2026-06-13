#include "mindc.h"

auto device vault1, vault2;

const item_t items[3] = {@coal, @sand, @pyratite};
const unsigned items_count = 3;

const int vault_max = 200;

int flag;

void moveTowards(int x, int y, int radius) {
	bool state;
	while (true) {
		if ((bool)sensor(@unit, @dead)) end();
		ucontrol("within", x, y, radius, (volatile bool*)state);
		if (state) break;
		ucontrol("move", x, y);
	}
}

void main() {
	flag = (int)sensor(@this, @x) * @maph + (int)sensor(@this, @y);
	int v1x = (int)sensor(vault1, @x), v1y = (int)sensor(vault1, @y);
	int v2x = (int)sensor(vault2, @x), v2y = (int)sensor(vault2, @y);
	while (true) {
		ubind(@poly);
		if ((bool)sensor(@unit, @dead)) continue;
		int cflag = (int)sensor(@unit, @flag);
		if (cflag != 0 && cflag != flag) continue;
		ucontrol("flag", flag);
		break;
	}
	int item_capcity = (int)sensor(@unit, @itemCapacity);
	while (true) {
		while ((int)sensor(@unit, @totalItems) > 0) {
			moveTowards(v2x, v2y, 3);
			ucontrol("itemDrop", vault2, sensor(@unit, @totalItems));
		}
		int i;
		for (i = 0; i < items_count; ++i) {
			const content_t current_item = items[i];
			if ((int)sensor(vault2, current_item) >= vault_max) continue;
			if ((int)sensor(vault1, current_item) <= 0) continue;
			moveTowards(v1x, v1y, 3);
			while ((int)sensor(@unit, current_item) < item_capcity) {
				ucontrol("itemTake", vault1, current_item, item_capcity);
			}
			moveTowards(v2x, v2y, 3);
			while ((int)sensor(@unit, current_item) > 0) {
				ucontrol("itemDrop", vault2, item_capcity);
			}
		}
	}
}