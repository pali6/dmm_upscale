extern crate dreammaker as dm;
extern crate dmmtools;

use std::collections::HashMap;
use std::str;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use ndarray::{self, Array2};

use dm::constants::Constant;
use dm::objtree::ObjectTree;
use dmmtools::dmm;
use dmmtools::dmm::Prefab;

fn get_var<'a>(prefab: &'a Prefab, objtree: &'a ObjectTree, key: &str) -> Option<&'a Constant> {
	if let Some(v) = prefab.vars.get(key) {
			return Some(v);
	}
	let mut current = objtree.find(&prefab.path);
	while let Some(t) = current.take() {
			if let Some(v) = t.get().vars.get(key) {
					return Some(v.value.constant.as_ref().unwrap_or(Constant::null()));
			}
			current = t.parent_type();
	}
	None
}

type WipMap = Array2<Vec<Prefab>>;

enum BigTilePart<'a> {
	FixedPrefab(Prefab),
	Source,
	ModifiedSource(&'a [(&'a str, Constant)]),
}

struct BigTileTemplate<'a> {
	parts: [&'a [&'a BigTilePart<'a>]; 4],
}

macro_rules! big_tile_template {
	($p1:expr, $p2:expr, $p3:expr, $p4:expr) => {
		BigTileTemplate {
			parts: [
				&[&$p1],
				&[&$p2],
				&[&$p3],
				&[&$p4],
			],
		}
	};
}

const BIG_TILE_FILL: BigTileTemplate = big_tile_template!(
	BigTilePart::Source, 	BigTilePart::Source,
	BigTilePart::Source, 	BigTilePart::Source
);

const BIG_TILE_SHIFTS: [(usize, usize); 4] = [
	(0, 1), (1, 1),
	(0, 0), (1, 0),
];

fn apply_big_tile(out_map: &mut WipMap, big_tile: &BigTileTemplate, objtree: &ObjectTree, prefab: &Prefab, coords: (usize, usize)) {
	let (bigx, bigy) = (coords.0 * 2, coords.1 * 2);
	for ((dx, dy), &part_list) in BIG_TILE_SHIFTS.iter().zip(&big_tile.parts) {
		let x = bigx + dx;
		let y = bigy + dy;
		for &part in part_list {
			out_map.get_mut((x, y)).unwrap().push(match part {
				BigTilePart::FixedPrefab(p) => p.clone(),
				BigTilePart::Source => prefab.clone(),
				BigTilePart::ModifiedSource(vars) => {
					let mut p = prefab.clone();
					for (key, value) in vars.iter() {
						p.vars.insert(key.to_string(), value.clone());
					}
					p
				}
			});
		}
	}
}

fn normalize_tile(tile: &mut Vec<Prefab>) {
	for p in tile.iter_mut() {
		p.vars.sort_keys();
	}
	tile.sort_by_key(|p| {
		let mut hasher = DefaultHasher::new();
		p.hash(&mut hasher);
		hasher.finish()
	});
}

fn finish_map(in_map: &mut WipMap) -> Result<dmm::Map, &'static str> {
	let mut out_map = dmm::Map::new(in_map.ncols(), in_map.nrows(), 1, "/turf".to_string(), "/area".to_string());
	let mut key  = out_map.dictionary.keys().next().unwrap().clone(); // sorta hacky way to get the first key
	let mut tile_to_key = HashMap::new();
	for tile in in_map.iter_mut() {
		normalize_tile(tile);
	}
	for tile in in_map.iter() {
		if !tile_to_key.contains_key(tile) {
			tile_to_key.insert(tile, key);
			out_map.dictionary.insert(key, tile.clone());
			key = key.next();
		}
	}
	for ((x, y), tile) in in_map.indexed_iter() {
		let key = tile_to_key[tile];
		out_map.grid[(0, y, x)] = key;
	}
	out_map.adjust_key_length();
	Ok(out_map)
}

fn main() {
	let map = dmm::Map::from_file(r"..\..\goonstation\maps\atlas.dmm".as_ref());
	let map = map.unwrap();
	
	//let objtree = Context::default().parse_environment(r"..\..\goonstation\goonstation.dme".as_ref());
	//let objtree = objtree.unwrap();
	let objtree = ObjectTree::default();

	let (width, height, z_level_count) = map.dim_xyz();
	assert!(z_level_count == 1); // TODO

	let mut out_map : WipMap = ndarray::Array2::from_elem((2 * width, 2 * height), Vec::new());

	for ((_, y, x), key) in map.grid.indexed_iter() {
		let prefabs = &map.dictionary[key];
		let big_tile = BIG_TILE_FILL;
		for prefab in prefabs {
			apply_big_tile(&mut out_map, &big_tile, &objtree, &prefab, (x, y));
		}
	}

	let out_map = finish_map(&mut out_map).unwrap();
	out_map.to_file("atlas_big.dmm".as_ref()).unwrap();
}