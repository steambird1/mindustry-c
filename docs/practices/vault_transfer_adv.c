#include "mindc.h"

auto device display1;
auto device switch1, switch2;

device vault_from, vault_to;
device player;
const device display = display1;

const item_t items[1] = {@blast_compound};
const unsigned items_count = 1;

const int vault_max = 200;

int current_print = 70;
int flag;
static bool inited = false;

void moveTowards(int x, int y, int radius) {
	bool state;
	while (true) {
		if ((bool)sensor(@unit, @dead)) end();
		ucontrol("within", x, y, radius, (volatile bool*)state);
		if (state) break;
		ucontrol("move", x, y);
	}
}

bool bound() {
	int cflag = (int)sensor(@unit, @flag);
	return (!((bool)sensor(@unit, @dead))) && (cflag == 0 || cflag == flag);
}

void wclear() {
	current_print = 70;
	draw("clear", 0, 0, 0);
	drawflush(display);
}

void wwrite(char data) {
	//printflush(null);
	print(data);
	draw("print", 5, current_print, @bottomLeft);
	current_print -= 10;
	drawflush(display);
}

void bindTarget(content_t type) {
	while (!bound()) {
		ubind(type);
		if (!bound()) continue;
		ucontrol("flag", flag);
		break;
	}
	wwrite("Bound unit!");
}

device chooseVault(char bindinfo, device check_switch) {
	char info = "Please move";
	while (true) {
		wclear();
		wwrite(bindinfo);
		wwrite(info);
		while (!((bool)sensor(check_switch, @enabled))) {
			
		}
		moveTowards((int)sensor(player, @x), (int)sensor(player, @y), 2);
		device result;
		int rx, ry;
		bool rf = false;
		ulocate("building", "storage", false, (volatile int*)rx, (volatile int*)ry, (volatile bool*)rf, (volatile device*)result);
		if (!rf) {
			info = "Try again!";
			control("enabled", check_switch, false);
			continue;
		}
		return result;
	}
}

void main() {
	if (!inited) {
		flag = (int)sensor(@this, @x) * @maph + (int)sensor(@this, @y);
		inited = true;
	}
	wclear();
	player = radar("player", "ally", "any", "distance", @this, 1);
	if ((bool)sensor(player, @dead)) {
		wwrite("No player");
		drawflush(display);
		end();
	}
	while (true) {
		bindTarget(@poly);
		if (!((bool)sensor(switch1, @enabled))) {
			vault_from = chooseVault("source", switch1);
		}
		if (!((bool)sensor(switch2, @enabled))) {
			vault_to = chooseVault("target", switch2);
		}
		wclear();
		wwrite("Ready for delivery...");
		const int item_capcity = (int)sensor(@unit, @itemCapacity);
		const int v1x = sensor(vault_from, @x), v1y = sensor(vault_from, @y);
		const int v2x = sensor(vault_to, @x), v2y = sensor(vault_to, @y);
		print(v1x, ",", v1y, " -> ", v2x, ",", v2y);
		wwrite("");
		int i;
		for (i = 0; i < items_count; ++i) {
			const content_t current_item = items[i];
			if ((int)sensor(vault_to, current_item) >= vault_max) continue;
			if ((int)sensor(vault_from, current_item) <= 0) continue;
			moveTowards(v1x, v1y, 3);
			while ((int)sensor(@unit, current_item) < item_capcity) {
				ucontrol("itemTake", vault_from, current_item, item_capcity);
			}
			moveTowards(v2x, v2y, 3);
			while ((int)sensor(@unit, current_item) > 0) {
				ucontrol("itemDrop", vault_to, item_capcity);
			}
		}
	}
}