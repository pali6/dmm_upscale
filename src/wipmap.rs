use std::{collections::{hash_map::DefaultHasher, HashMap}, hash::{Hash, Hasher}};

use dmmtools::dmm::{Prefab, self, Coord3, Coord2};
use ndarray::Array2;

use crate::coord::{coord_from_raw, coord_to_raw};

pub struct WipMap {
	pub grid: Array2<Vec<Prefab>>,
}

impl WipMap {
	pub fn new(dim_x: usize, dim_y: usize) -> WipMap {
		WipMap {
			grid: Array2::from_elem((dim_x, dim_y), vec![]),
		}
	}

	pub fn add_prefab(&mut self, coord: Coord2, prefab: Prefab) {
		self.grid[(coord.x as usize - 1, coord.y as usize - 1)].push(prefab);
	}

	pub fn width(&self) -> usize {
		self.grid.shape()[0]
	}

	pub fn height(&self) -> usize {
		self.grid.shape()[1]
	}

	pub fn finish(self: &mut WipMap) -> Result<dmm::Map, &'static str> {
		let mut out_map = dmm::Map::new(self.width(), self.height(), 1, "/turf".to_string(), "/area".to_string());
		let mut key  = out_map.dictionary.keys().next().unwrap().clone(); // sorta hacky way to get the first key
		let mut tile_to_key = HashMap::new();
		for tile in self.grid.iter_mut() {
			normalize_tile(tile);
		}
		for tile in self.grid.iter() {
			if !tile_to_key.contains_key(tile) {
				tile_to_key.insert(tile, key);
				out_map.dictionary.insert(key, tile.clone());
				key = key.next();
			}
		}
		for ((x, y), tile) in self.grid.indexed_iter() {
			let key = tile_to_key[tile];
			let coords = coord_to_raw(Coord3::new(x as i32 + 1, y as i32 + 1, 1), out_map.grid.dim());
			out_map.grid[coords] = key;
		}
		out_map.adjust_key_length();
		Ok(out_map)
	}
}

fn normalize_tile(tile: &mut Vec<Prefab>) {
	for p in tile.iter_mut() {
		p.vars.sort_keys();
	}
	tile.sort_by_key(|p| {
		format!("{:?}, {:?}", p.path, p.vars)
	});
	tile.dedup_by_key(|p| {
		format!("{:?}, {:?}", p.path, p.vars)
	});
}