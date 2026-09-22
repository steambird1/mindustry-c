/**
 * Content ids (item / liquid / unit / block) and the `@`-constant tables.
 *
 * An id is the index of the content inside the `logicids.dat` array of its
 * category -- exactly what `sensor x @id` returns and what `lookup` takes.
 * The tables below are the v152..v159 ordering (see `itemlist.json`).
 *
 * `#pragma id(<category>, @name: <number|null>)` may add or override entries at
 * compile time; those are kept in `ContentIdTable` instead of here.
 */

/** Bare names, in id order. */
export const CONTENT_ORDER = {
	item: [
		'copper', 'lead', 'metaglass', 'graphite', 'sand', 'coal', 'titanium',
		'thorium', 'scrap', 'silicon', 'plastanium', 'phase-fabric', 'surge-alloy',
		'spore-pod', 'blast-compound', 'pyratite', 'beryllium', 'tungsten',
		'oxide', 'carbide'
	],
	liquid: [
		'water', 'slag', 'oil', 'cryofluid', 'neoplasm', 'hydrogen', 'ozone',
		'cyanogen', 'gallium', 'nitrogen', 'arkycite'
	],
	unit: [
		'dagger', 'mace', 'fortress', 'scepter', 'reign', 'nova', 'pulsar',
		'quasar', 'vela', 'corvus', 'crawler', 'atrax', 'spiroct', 'arkyid',
		'toxopid', 'flare', 'horizon', 'zenith', 'antumbra', 'eclipse', 'mono',
		'poly', 'mega', 'quad', 'oct', 'risso', 'minke', 'bryde', 'sei', 'omura',
		'retusa', 'oxynoe', 'cyerce', 'aegires', 'navanax', 'alpha', 'beta',
		'gamma', 'stell', 'locus', 'precept', 'vanquish', 'conquer', 'merui',
		'cleroi', 'anthicus', 'tecta', 'collaris', 'elude', 'avert', 'obviate',
		'quell', 'disrupt', 'evoke', 'incite', 'emanate'
	],
	block: [
		'graphite-press', 'multi-press', 'silicon-smelter', 'silicon-crucible',
		'kiln', 'plastanium-compressor', 'phase-weaver', 'cryofluid-mixer',
		'pyratite-mixer', 'blast-mixer', 'melter', 'separator', 'disassembler',
		'spore-press', 'pulverizer', 'coal-centrifuge', 'incinerator',
		'copper-wall', 'copper-wall-large', 'titanium-wall', 'titanium-wall-large',
		'plastanium-wall', 'plastanium-wall-large', 'thorium-wall',
		'thorium-wall-large', 'phase-wall', 'phase-wall-large', 'surge-wall',
		'surge-wall-large', 'door', 'door-large', 'scrap-wall', 'scrap-wall-large',
		'scrap-wall-huge', 'scrap-wall-gigantic', 'mender', 'mend-projector',
		'overdrive-projector', 'overdrive-dome', 'force-projector', 'shock-mine',
		'conveyor', 'titanium-conveyor', 'plastanium-conveyor', 'armored-conveyor',
		'junction', 'bridge-conveyor', 'phase-conveyor', 'sorter',
		'inverted-sorter', 'router', 'distributor', 'overflow-gate',
		'underflow-gate', 'mass-driver', 'duct', 'duct-router', 'duct-bridge',
		'mechanical-pump', 'rotary-pump', 'conduit', 'pulse-conduit',
		'plated-conduit', 'liquid-router', 'liquid-tank', 'liquid-junction',
		'bridge-conduit', 'phase-conduit', 'power-node', 'power-node-large',
		'surge-tower', 'diode', 'battery', 'battery-large',
		'combustion-generator', 'thermal-generator', 'steam-generator',
		'differential-generator', 'rtg-generator', 'solar-panel',
		'solar-panel-large', 'thorium-reactor', 'impact-reactor',
		'mechanical-drill', 'pneumatic-drill', 'laser-drill', 'blast-drill',
		'water-extractor', 'cultivator', 'oil-extractor', 'core-shard',
		'core-foundation', 'core-nucleus', 'vault', 'container', 'unloader', 'duo',
		'scatter', 'scorch', 'hail', 'wave', 'lancer', 'arc', 'parallax', 'swarmer',
		'salvo', 'segment', 'tsunami', 'fuse', 'ripple', 'cyclone', 'foreshadow',
		'spectre', 'meltdown', 'command-center', 'ground-factory', 'air-factory',
		'naval-factory', 'additive-reconstructor', 'multiplicative-reconstructor',
		'exponential-reconstructor', 'tetrative-reconstructor', 'repair-point',
		'repair-turret', 'payload-conveyor', 'payload-router', 'power-source',
		'power-void', 'item-source', 'item-void', 'liquid-source', 'liquid-void',
		'payload-void', 'payload-source', 'illuminator', 'launch-pad',
		'interplanetary-accelerator', 'message', 'switch', 'micro-processor',
		'logic-processor', 'hyper-processor', 'memory-cell', 'memory-bank',
		'logic-display', 'large-logic-display', 'liquid-container', 'deconstructor',
		'constructor', 'thruster', 'large-constructor', 'payload-loader',
		'payload-unloader', 'silicon-arc-furnace', 'cliff-crusher', 'plasma-bore',
		'reinforced-liquid-junction', 'breach', 'core-bastion', 'turbine-condenser',
		'beam-node', 'beam-tower', 'build-tower', 'impact-drill',
		'carbide-crucible', 'surge-conveyor', 'duct-unloader', 'surge-router',
		'reinforced-conduit', 'reinforced-liquid-router',
		'reinforced-liquid-container', 'reinforced-liquid-tank',
		'reinforced-bridge-conduit', 'core-citadel', 'core-acropolis',
		'heat-reactor', 'impulse-pump', 'reinforced-pump', 'electrolyzer',
		'oxidation-chamber', 'surge-smelter', 'surge-crucible', 'overflow-duct',
		'large-plasma-bore', 'cyanogen-synthesizer', 'slag-centrifuge',
		'electric-heater', 'slag-incinerator', 'phase-synthesizer', 'sublimate',
		'reinforced-container', 'reinforced-vault', 'atmospheric-concentrator',
		'unit-cargo-loader', 'unit-cargo-unload-point',
		'chemical-combustion-chamber', 'pyrolysis-generator', 'regen-projector',
		'titan', 'small-deconstructor', 'vent-condenser', 'phase-heater',
		'heat-redirector', 'tungsten-wall', 'tungsten-wall-large', 'tank-assembler',
		'beryllium-wall', 'beryllium-wall-large', 'eruption-drill',
		'ship-assembler', 'mech-assembler', 'shield-projector', 'beam-link',
		'world-processor', 'reinforced-payload-conveyor', 'reinforced-payload-router',
		'disperse', 'large-shield-projector', 'payload-mass-driver', 'world-cell',
		'carbide-wall', 'carbide-wall-large', 'tank-fabricator', 'mech-fabricator',
		'ship-fabricator', 'reinforced-surge-wall', 'radar', 'blast-door', 'canvas',
		'armored-duct', 'unit-repair-tower', 'diffuse', 'prime-refabricator',
		'basic-assembler-module', 'reinforced-surge-wall-large', 'tank-refabricator',
		'mech-refabricator', 'ship-refabricator', 'slag-heater', 'afflict',
		'shielded-wall', 'lustre', 'scathe', 'smite', 'underflow-duct', 'malign',
		'shockwave-tower', 'heat-source', 'flux-reactor', 'neoplasia-reactor',
		'heat-router', 'large-payload-mass-driver', 'reinforced-message',
		'world-message', 'world-switch', 'small-heat-redirector',
		'large-cliff-crusher', 'advanced-launch-pad', 'landing-pad',
		'tile-logic-display', 'large-canvas'
	]
};

/**
 * `@name` -> id, built from the tables above.
 * @type {Record<string, Map<string, number>>}
 */
export const DEFAULT_CONTENT_IDS = Object.fromEntries(
	Object.entries(CONTENT_ORDER).map(([category, names]) =>
		[category, new Map(names.map((name, id) => [`@${name}`, id]))]));

/** `@name` lists in id order, one per category. */
export const DEFAULT_CONTENT_NAMES = Object.fromEntries(
	Object.entries(CONTENT_ORDER).map(([category, names]) =>
		[category, names.map(name => `@${name}`)]));

/** The categories a `#pragma id(...)` may name, with their usual aliases. */
export const CONTENT_CATEGORIES = ['item', 'liquid', 'unit', 'block'];

/**
 * Normalise the category word of a pragma (`building` and `object` are the
 * names used elsewhere in the compiler).
 *
 * @param {string} word
 * @returns {string?} null when it is not a content category
 */
export function normalizeCategory(word) {
	const w = String(word ?? '').trim().toLowerCase();
	if (w === 'building' || w === 'buildings' || w === 'block') return 'block';
	if (w === 'object' || w === 'objects' || w === 'item') return 'item';
	if (w === 'liquid') return 'liquid';
	if (w === 'unit') return 'unit';
	return null;
}

/**
 * Everything the compiler needs to know about `@` constants for one program.
 *
 * The defaults come from the tables above; `#pragma id(...)` entries are laid
 * on top of them and win.
 */
export class ContentIdTable {

	constructor() {
		/** @type {Map<string, number|null>} per category */
		this.overrides = new Map(CONTENT_CATEGORIES.map(c => [c, new Map()]));
		/** Extra names, in the order they were declared, so ids stay stable */
		this.extra = new Map(CONTENT_CATEGORIES.map(c => [c, []]));
		/** `#pragma var(type, @name)` -- processor variables */
		this.variables = new Map();
	}

	/** Whether any `#pragma id(...)` was seen at all. */
	get hasOverrides() {
		for (const map of this.overrides.values()) if (map.size > 0) return true;
		for (const list of this.extra.values()) if (list.length > 0) return true;
		return false;
	}

	/**
	 * @param {string} category
	 * @param {string} name An `@name`
	 * @param {number|null} id
	 * @returns {void}
	 */
	define(category, name, id) {
		const cat = normalizeCategory(category);
		if (!cat) return;
		const map = this.overrides.get(cat);
		if (DEFAULT_CONTENT_IDS[cat].has(name)) {
			map.set(name, id);
			return;
		}
		// A brand new name: remember it, and keep the declaration order
		const list = this.extra.get(cat);
		const known = list.findIndex(entry => entry.name === name);
		if (known >= 0) list[known].id = id;
		else list.push({ name, id });
	}

	/**
	 * @param {string} category
	 * @param {string} name
	 * @returns {number|null|undefined} undefined = not a content of this kind
	 */
	idOf(category, name) {
		const cat = normalizeCategory(category);
		if (!cat) return undefined;
		if (this.overrides.get(cat).has(name)) return this.overrides.get(cat).get(name);
		if (DEFAULT_CONTENT_IDS[cat].has(name)) return DEFAULT_CONTENT_IDS[cat].get(name);
		const found = this.extra.get(cat).find(entry => entry.name === name);
		return found ? found.id : undefined;
	}

	/**
	 * @param {string} name
	 * @returns {string?} the category the name belongs to
	 */
	categoryOf(name) {
		for (const cat of CONTENT_CATEGORIES) {
			if (this.idOf(cat, name) !== undefined) return cat;
		}
		return null;
	}

	/**
	 * `@name` list of a category in id order, including declared extras.
	 *
	 * @param {string} category
	 * @returns {string[]}
	 */
	namesOf(category) {
		const cat = normalizeCategory(category);
		if (!cat) return [];
		return [...DEFAULT_CONTENT_NAMES[cat], ...this.extra.get(cat).map(e => e.name)];
	}

	/**
	 * @param {string} name
	 * @returns {boolean} true for `@x` nobody declared and no table knows
	 */
	isOpaque(name) {
		if (typeof name !== 'string' || name[0] !== '@') return false;
		return this.variables.has(name) ? false : this.categoryOf(name) === null;
	}

	/**
	 * @param {string} name
	 * @param {string} typeName
	 * @returns {void}
	 */
	defineVariable(name, typeName) {
		this.variables.set(name, typeName);
	}

	/**
	 * @param {string} name
	 * @returns {string?}
	 */
	variableTypeOf(name) {
		return this.variables.get(name) ?? null;
	}
}
