extern crate dreammaker as dm;
extern crate dmmtools;

mod bigtile;
mod dir;
mod wipmap;
mod coord;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::path::Path;
use std::str;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use dm::Context;
use ndarray::{self, Array2};

use dm::constants::{Constant, ConstFn};
use dm::objtree::ObjectTree;
use dmmtools::dmm::{self, Coord3, Map, Coord2};
use dmmtools::dmm::Prefab;

use bigtile::*;
use wipmap::{WipMap};
use dir::Dir;
use coord::*;

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

fn get_pixel_shift(prefab: &Prefab, objtree: &ObjectTree) -> Option<(f32, f32)> {
	let x = get_var(prefab, objtree, "pixel_x").unwrap_or(&Constant::Float(0.));
	let y = get_var(prefab, objtree, "pixel_y").unwrap_or(&Constant::Float(0.));
	match (x, y) {
		(&Constant::Float(x), &Constant::Float(y)) => Some((x, y)),
		_ => None,
	}
}

fn find_neighbor<F>(map: &Map, coord: Coord3, check: F) -> Option<Dir> 
	where F: Fn(&Prefab) -> bool {
	find_neighbor_dir_filter(map, coord, |x, _| check(x))
}

fn find_neighbor_dir_filter<F>(map: &Map, coord: Coord3, check: F) -> Option<Dir> 
	where F: Fn(&Prefab, Dir) -> bool {
	let (width, height, _) = map.dim_xyz();
	for dir in dir::CARDINAL_DIRS {
		let new_coord = dir.shift_coord(coord);
		if new_coord.x < 1 || new_coord.y < 1 || new_coord.x > width as i32 || new_coord.y > height as i32 {
			continue;
		}
		for prefab in map.dictionary.get(&map[new_coord]).unwrap_or(&vec![]) {
			if check(prefab, dir) {
				return Some(dir);
			}
		}
	}
	None
}

fn split_path<'a>(path: &'a String) -> Vec<&'a str> {
	path.split("/").collect::<Vec<_>>()
}

fn build_path(path: &str, subpath: &[&str]) -> String {
	path.to_string() + (if subpath.len() == 0 {
		"".to_string()
	}
	else {
		"/".to_string() + subpath.join("/").as_str()
	}).as_str()
}

fn main() {
	println!("begin");
	
	let objtree = Context::default().parse_environment(r"..\..\goonstation\goonstation.dme".as_ref());
	let objtree = objtree.unwrap();
	println!("environment parsed");

	let path = Path::new(r"..\..\goonstation\maps");
	let paths = if path.is_dir() {
		let mut paths_result = vec![];
		for subpath in std::fs::read_dir(path).unwrap() {
			if !subpath.is_ok() {
				continue;
			}
			let path = subpath.unwrap().path();
			if path.is_file()
					&& path.extension().and_then(std::ffi::OsStr::to_str).unwrap_or("") == "dmm"
					&& !path.file_stem().unwrap().to_str().unwrap().ends_with("_big") {
				paths_result.push(path);
			}
		}
		paths_result
	} else {
		vec![path.into()]
	};

	for path in paths {
		let out_filename = format!(
			"{}_big.{}",
			path.file_stem().unwrap().to_str().unwrap(),
			path.extension().unwrap().to_str().unwrap()
		);
		let output_path = path.parent().unwrap().join(out_filename);
		let map = dmm::Map::from_file(&path);
		let map = map.unwrap();
		
		println!("starting map {}", path.display());
		let out_map = upscale_map(&map, &objtree);
		println!("map finished");
		out_map.to_file(&output_path).unwrap();
		println!("saved map {}", output_path.display());
	}

	println!("done");
}

pub fn upscale_map(map: &Map, objtree: &ObjectTree) -> Map {
	let (width, height, z_level_count) = map.dim_xyz();
	assert!(z_level_count == 1); // TODO

	let mut out_map : WipMap = WipMap::new(2 * width, 2 * height);

	for ((z, y, x), key) in map.grid.indexed_iter() {
		let coord = coord_from_raw((z, y, x), map.grid.dim());
		let prefabs = &map.dictionary[key];
		for prefab in prefabs {
			let path = split_path(&prefab.path);
			let path = &path.as_slice()[1..];
			let mut mut_prefab = prefab.clone();
			let solid_neigh = find_neighbor(&map, coord, |prefab|
				prefab.path.starts_with("/turf/simulated/wall") ||
				prefab.path.starts_with("/obj/wingrille_spawn") ||
				prefab.path.starts_with("/obj/window") ||
				prefab.path.starts_with("/obj/machinery/door")
			);
			let dir = get_var(prefab, &objtree, "dir")
				.and_then(|x| Dir::try_from(x).ok())
				.unwrap_or(Dir::South);

			let big_tile: BigTileTemplate = match path {
				["obj", "machinery", "light", "small", "floor", ..] => 
					BIG_TILE_FILL.clone(),
				["obj", "xmastree", ..] |
				["obj", "landmark", "gps_waypoint", ..] |
				["obj", "landmark", "map", ..] |
				["obj", "item", "device", "radio", "beacon", ..] |
				["obj", "machinery", "navbeacon", ..] => 
					BIG_TILE_JUST_BOTTOM_LEFT.clone(),
				["obj", "decal", "fakeobjects", "airmonitor_broken", ..] |
				["obj", "machinery", "sparker", ..] |
				["obj", "machinery", "light_switch", "auto", ..] => {
					solid_neigh
					.and_then(|x| Some(big_tile_two_on_side(x)))
					.unwrap_or(BIG_TILE_FILL.clone())
				}
				["obj", "airbridge_controller", ..] => {
					let tunnel_width = get_var(prefab, &objtree, "tunnel_width")
						.and_then(Constant::to_float)
						.unwrap_or(0.);
					mut_prefab.vars.insert("tunnel_width".to_string(), Constant::Float(tunnel_width * 2.));
					solid_neigh
					.and_then(|x| Some(big_tile_one_on_side_biased(x)))
					.unwrap_or(BIG_TILE_FILL.clone())
				}
				["obj", "machinery", "drone_recharger", ..] => 
					big_tile_two_on_side(Dir::North),
				["obj", "stool", "chair", "couch", ..] => {
					match dir {
						Dir::North | Dir::South => big_tile_two_on_side(Dir::North),
						Dir::East => big_tile_template!(
							big_tile_modification!("dir" => Dir::South.to_constant()), BigTilePart::Source,
							BigTilePart::Empty, BigTilePart::Empty
						),
						Dir::West => big_tile_template!(
							BigTilePart::Source, big_tile_modification!("dir" => Dir::South.to_constant()),
							BigTilePart::Empty, BigTilePart::Empty
						),
						_ => BIG_TILE_FILL.clone(),
					}
				}
				["obj", "stool", "chair", "boxingrope_corner", ..] => {
					let rope_down = Prefab{
						path: "/obj/decal/boxingrope".to_string(),
						vars: Default::default(),
					};
					let mut rope_left = rope_down.clone();
					rope_left.vars.insert("dir".to_string(), Dir::West.to_constant());
					let mut rope_right = rope_down.clone();
					rope_right.vars.insert("dir".to_string(), Dir::East.to_constant());
					match dir {
						Dir::NorthEast => big_tile_template!(
							BigTilePart::Empty, BigTilePart::Empty,
							BigTilePart::FixedPrefab(rope_down), BigTilePart::Source
						),
						Dir::NorthWest => big_tile_template!(
							BigTilePart::Empty, BigTilePart::Empty,
							BigTilePart::Source, BigTilePart::FixedPrefab(rope_down)
						),
						Dir::SouthEast => big_tile_template!(
							BigTilePart::Empty, BigTilePart::FixedPrefab(rope_right),
							BigTilePart::FixedPrefab(rope_down), BigTilePart::Source
						),
						Dir::SouthWest => big_tile_template!(
							BigTilePart::FixedPrefab(rope_left), BigTilePart::Empty,
							BigTilePart::Source, BigTilePart::FixedPrefab(rope_down)
						),
						_ => BIG_TILE_FILL.clone(),
					}
				}
				["obj", "atlasplaque", ..] |
				["obj", "machinery", "phone", "wall", ..] |
				["obj", "noticeboard", "persistent", ..] |
				["obj", "bookshelf", "persistent", ..] |
				["obj", "submachine", "GTM", ..] |
				["obj", "machinery", "computer", "airbr", ..] |
				["obj", "machinery", "computer", "riotgear", ..] |
				["obj", "machinery", "door_timer", ..] |
				["obj", "machinery", "power", "apc", ..] => {
					get_pixel_shift(prefab, &objtree)
						.and_then(dir::pixel_shift_to_cardinal_dir)
						.and_then(|x| Some(big_tile_one_on_side(x)))
						.unwrap_or(BIG_TILE_JUST_BOTTOM_LEFT.clone())
				}
				/*
				["obj", "machinery", "camera", ..] => {
					get_pixel_shift(prefab, &objtree)
						.and_then(dir::pixel_shift_to_cardinal_dir)
						.or(Some(dir.flip()))
						.and_then(|x| Some(big_tile_one_on_side(x)))
						.unwrap_or(BIG_TILE_JUST_BOTTOM_LEFT.clone())
				}
				*/
				["obj", "item", "device", "radio", "intercom", ..] => {
					big_tile_one_on_side(dir.flip())
				}
				["obj", "machinery", "firealarm", ..] | 
				["obj", "submachine", "ATM", ..] | 
				["obj", "machinery", "door_control", ..] |
				["obj", "noticeboard", ..] |
				["obj", "bookshelf", ..] |
				["obj", "machinery", "turretid", ..] |
				["obj", "item_dispenser", ..] |
				["obj", "machinery", "light_switch", ..] |
				["obj", "drink_rack", ..] |
				["obj", "item", "device", "audio_log", "wall_mounted", ..] |
				["obj", "machinery", "activation_button", ..] |
				["obj", "machinery", "ai_status_display", ..] |
				["obj", "machinery", "flasher", "solitary", ..] |
				["obj", "item", "storage", "secure", "ssafe", ..] |
				["obj", "blind_switch", ..] |
				["obj", "machinery", "light", "incandescent", "small", ..] |
				["obj", "machinery", "light", "small", ..] => {
					get_pixel_shift(prefab, &objtree)
						.and_then(dir::pixel_shift_to_cardinal_dir)
						.or(Some(dir))
						.and_then(|x| Some(big_tile_two_on_side(x)))
						.unwrap_or(BIG_TILE_FILL.clone())
				},
				["obj", "window", ..] if !dir.is_cardinal() => {
					BIG_TILE_FILL.clone()
				}
				["obj", "stool", "chair", ..] => {
					find_neighbor_dir_filter(&map, coord, |prefab, table_dir|
						dir == table_dir && (
							prefab.path.starts_with("/obj/table") ||
							prefab.path.starts_with("/obj/machinery/computer")
						)
					).or(solid_neigh)
					.and_then(|x| Some(big_tile_two_on_side(x)))
					.unwrap_or_else(|| big_tile_two_on_side(dir))
				}
				["obj", "machinery", "light", ..] |
				["obj", "machinery", "power", "terminal"] |
				["obj", "machinery", "door", "airlock", "pyro", "glass", "windoor", ..] |
				["obj", "machinery", "atmospherics", "unary", ..] | // pipe?
				["obj", "machinery", "atmospherics", "pipe", "vent", ..] | // pipe?
				["obj", "machinery", "atmospherics", "portables_connector", ..] | // pipe?
				["obj", "machinery", "atmospherics", "pipe", "tank", ..] | // pipe?
				["obj", "potted_plant", ..] |
				["obj", "window", ..] |
				["obj", "decal", "boxingrope", ..] |
				["obj", "decal", "boxingropeenter", ..] |
				["obj", "machinery", "recharger", "wall", ..] |
				["obj", "machinery", "door", "window", ..] |
				["obj", "machinery", "shower", ..] |
				["obj", "disposalpipe", "broken", ..] |
				["obj", "channel", ..] |
				["obj", "railing", ..] =>
					big_tile_two_on_side(dir),
				["obj", "pool_springboard", ..] => {
					let dir = match dir {
						Dir::South => Dir::West,
						Dir::North => Dir::East,
						x => x
					};
					big_tile_two_on_side(dir)
				}
				["obj", "pool", ..] => {
					let icon_state = get_var(prefab, &objtree, "icon_state").unwrap().as_str().unwrap();
					match icon_state {
						"ladder" => big_tile_two_on_side(Dir::South),
						_ => big_tile_two_on_side(dir.flip()),
					}
				}
				["obj", "item", "storage", "wall", ..] |
				["obj", "machinery", "networked", "secdetector", ..] |
				["obj", "machinery", "disposaloutlet", ..] |
				["obj", "submachine", "chef_sink", ..] => 
					big_tile_two_on_side(dir.flip()),
				["obj", "machinery", "ghostdrone_factory", ..] =>
					big_tile_one_on_side(Dir::East),
				["obj", "machinery", "vehicle", "pod_smooth", ..] => {
					place_with_shift_scale(&mut out_map, &mut_prefab, coord.xy(), (2, 2));
					BIG_TILE_EMPTY.clone()
				}
				["obj", "landmark", "random_room", room_size] => {
					let scale = match *room_size {
						"size3x3" => (3, 3),
						"size3x5" => (3, 5),
						"size5x3" => (5, 3),
						_ => (0, 0),
					};
					place_with_shift_scale(&mut out_map, &mut_prefab, coord.xy(), scale);
					BIG_TILE_EMPTY.clone()
				}
				["obj", "cable", ..] => {
					let icon_state = get_var(prefab, &objtree, "icon_state").unwrap().as_str().unwrap();
					big_tile_cable(icon_state).unwrap_or(BIG_TILE_FILL.clone())
				}
				["obj", "forcefield", "energyshield", "perma", "doorlink", ..] => {
					// these screw themselves up if left like this
					mut_prefab.path = "/obj/forcefield/energyshield/perma".to_string();
					BIG_TILE_FILL.clone()
				}
				
				// DISPOSAL PIPES
				["obj", "disposalpipe", "trunk", subpath @ ..] => {
					let filtered_subpath = subpath.iter().filter(|&&x| !["north", "south", "east", "west"].contains(&x)).map(|x| *x).collect::<Vec<_>>();
					let mut straight_pipe = prefab.clone();
					straight_pipe.path = build_path("/obj/disposalpipe/segment", filtered_subpath.as_slice());
					straight_pipe.vars.insert("dir".to_string(), dir.to_constant());
					match dir {
						Dir::North => big_tile_template!(
							BigTilePart::FixedPrefab(straight_pipe.clone()), BigTilePart::FixedPrefab(straight_pipe),
							BigTilePart::Source, BigTilePart::Source
						),
						Dir::South => big_tile_template!(
							BigTilePart::Empty, BigTilePart::Empty,
							BigTilePart::Source, BigTilePart::Source
						),
						Dir::East => big_tile_template!(
							BigTilePart::Source, BigTilePart::FixedPrefab(straight_pipe.clone()),
							BigTilePart::Source, BigTilePart::FixedPrefab(straight_pipe)
						),
						Dir::West => big_tile_template!(
							BigTilePart::Source, BigTilePart::Empty,
							BigTilePart::Source, BigTilePart::Empty
						),
						_ => BIG_TILE_FILL.clone()
					}
				}
				["obj", "disposalpipe", "segment", subpath @ ..] => {
					let icon_state = get_var(prefab, &objtree, "icon_state").and_then(Constant::as_str).unwrap_or("");
					match icon_state {
						"pipe-c" => {
							let filtered_subpath = subpath.iter().filter(|&&x| !["bent", "north", "south", "east", "west"].contains(&x)).map(|x| *x).collect::<Vec<_>>();
							let mut straight_pipe = prefab.clone();
							straight_pipe.path = build_path("/obj/disposalpipe/segment", filtered_subpath.as_slice());
							straight_pipe.vars.insert("icon_state".to_string(), Constant::string("pipe-s"));
							let mut straight_pipe_side = straight_pipe.clone();
							straight_pipe_side.vars.insert("dir".to_string(), dir.turn_clockwise().to_constant());
							match dir {
								Dir::North => big_tile_template!(
									BigTilePart::FixedPrefab(straight_pipe), BigTilePart::Source,
									BigTilePart::Source, BigTilePart::FixedPrefab(straight_pipe_side)
								),
								Dir::South => big_tile_template!(
									BigTilePart::FixedPrefab(straight_pipe_side), BigTilePart::Source,
									BigTilePart::Source, BigTilePart::FixedPrefab(straight_pipe)
								),
								Dir::East => big_tile_template!(
									BigTilePart::Source, BigTilePart::FixedPrefab(straight_pipe),
									BigTilePart::FixedPrefab(straight_pipe_side), BigTilePart::Source
								),
								Dir::West => big_tile_template!(
									BigTilePart::Source, BigTilePart::FixedPrefab(straight_pipe_side),
									BigTilePart::FixedPrefab(straight_pipe), BigTilePart::Source
								),
								_ => BIG_TILE_FILL.clone()
							}
						}
						_ => BIG_TILE_FILL.clone()
					}
				}
				["obj", "disposalpipe", "junction", ..] |
				["obj", "disposalpipe", "switch_junction", ..] => {
					let icon_state = get_var(prefab, &objtree, "icon_state").and_then(Constant::as_str).unwrap_or("");
					let down_dir = match icon_state {
						"pipe-j1" | "pipe-sj1" => dir.turn_clockwise(),
						"pipe-j2" | "pipe-sj2" => dir.turn_counterclockwise(),
						"pipe-y" | _ => dir.flip()
					};
					let mut down_pipe = Prefab{
						path: "/obj/disposalpipe/segment".to_string(),
						vars: Default::default(),
					};
					down_pipe.vars.insert("dir".to_string(), down_dir.to_constant());
					let mut side_pipe = down_pipe.clone();
					side_pipe.vars.insert("dir".to_string(), down_dir.turn_clockwise().to_constant());
					match down_dir {
						Dir::South => BigTileTemplate {
							parts: [
								vec![BigTilePart::FixedPrefab(side_pipe.clone())], vec![BigTilePart::Source],
								vec![BigTilePart::Source], vec![BigTilePart::FixedPrefab(side_pipe), BigTilePart::FixedPrefab(down_pipe)],
							]
						},
						Dir::North => BigTileTemplate {
							parts: [
								vec![BigTilePart::FixedPrefab(down_pipe), BigTilePart::FixedPrefab(side_pipe.clone())], vec![BigTilePart::Source],
								vec![BigTilePart::Source], vec![BigTilePart::FixedPrefab(side_pipe)],
							]
						},
						Dir::East => BigTileTemplate {
							parts: [
								vec![BigTilePart::Source], vec![BigTilePart::FixedPrefab(down_pipe), BigTilePart::FixedPrefab(side_pipe.clone())],
								vec![BigTilePart::FixedPrefab(side_pipe)], vec![BigTilePart::Source]
							]
						},
						Dir::West => BigTileTemplate {
							parts: [
								vec![BigTilePart::Source], vec![BigTilePart::FixedPrefab(side_pipe.clone())],
								vec![BigTilePart::FixedPrefab(down_pipe), BigTilePart::FixedPrefab(side_pipe)], vec![BigTilePart::Source]
							]
						},
						_ => BIG_TILE_FILL.clone()
					}
				}

				// ATMOS PIPES
				["obj", "machinery", "atmospherics", "pipe", "simple", "junction", ..] |
				["obj", "machinery", "atmospherics", "valve", ..] |
				["obj", "machinery", "atmospherics", "binary", ..] => {
					let mut straight_pipe = Prefab{
						path: "/obj/machinery/atmospherics/pipe/simple".to_string(),
						vars: Default::default(),
					};
					straight_pipe.vars.insert("level".to_string(), Constant::Float(2.));
					straight_pipe.vars.insert("dir".to_string(), dir.to_constant());
					match dir {
						Dir::North => big_tile_template!(
							BigTilePart::Source, BigTilePart::Source,
							BigTilePart::FixedPrefab(straight_pipe.clone()), BigTilePart::FixedPrefab(straight_pipe)
						),
						Dir::South => big_tile_template!(
							BigTilePart::FixedPrefab(straight_pipe.clone()), BigTilePart::FixedPrefab(straight_pipe),
							BigTilePart::Source, BigTilePart::Source
						),
						Dir::East => big_tile_template!(
							BigTilePart::FixedPrefab(straight_pipe.clone()), BigTilePart::Source, 
							BigTilePart::FixedPrefab(straight_pipe), BigTilePart::Source
						),
						Dir::West => big_tile_template!(
							BigTilePart::Source, BigTilePart::FixedPrefab(straight_pipe.clone()),
							BigTilePart::Source, BigTilePart::FixedPrefab(straight_pipe)
						),
						_ => BIG_TILE_FILL.clone()
					}
				}
				["obj", "machinery", "atmospherics", "pipe", "simple", ..] => {
					match dir {
						Dir::North | Dir::South | Dir::East | Dir::West => BIG_TILE_FILL.clone(),
						Dir::NorthEast => big_tile_template!(
							big_tile_modification!("dir" => Dir::North.to_constant()), BigTilePart::Source,
							BigTilePart::Source, big_tile_modification!("dir" => Dir::East.to_constant())
						),
						Dir::NorthWest => big_tile_template!(
							BigTilePart::Source, big_tile_modification!("dir" => Dir::North.to_constant()), 
							big_tile_modification!("dir" => Dir::West.to_constant()), BigTilePart::Source
						),
						Dir::SouthEast => big_tile_template!(
							BigTilePart::Source, big_tile_modification!("dir" => Dir::East.to_constant()), 
							big_tile_modification!("dir" => Dir::South.to_constant()), BigTilePart::Source
						),
						Dir::SouthWest => big_tile_template!(
							big_tile_modification!("dir" => Dir::West.to_constant()), BigTilePart::Source,
							BigTilePart::Source, big_tile_modification!("dir" => Dir::South.to_constant())
						)
					}
				}
				["obj", "machinery", "atmospherics", pipe_type @ "mixer", ..]  | 
				["obj", "machinery", "atmospherics", pipe_type @ "pipe", "manifold", ..] => {
					let down_dir = match *pipe_type {
						"pipe" => dir.flip(),
						"mixer" => dir.turn_clockwise(),
						_ => dir.flip(),
					};
					let mut down_pipe = Prefab{
						path: "/obj/machinery/atmospherics/pipe/simple".to_string(),
						vars: Default::default(),
					};
					down_pipe.vars.insert("level".to_string(), Constant::Float(2.));
					down_pipe.vars.insert("dir".to_string(), down_dir.to_constant());
					let mut side_pipe = down_pipe.clone();
					side_pipe.vars.insert("dir".to_string(), down_dir.turn_clockwise().to_constant());
					// copypasted code here we go >:)
					match down_dir {
						Dir::South => BigTileTemplate {
							parts: [
								vec![BigTilePart::FixedPrefab(side_pipe.clone())], vec![BigTilePart::Source],
								vec![BigTilePart::Source], vec![BigTilePart::FixedPrefab(side_pipe), BigTilePart::FixedPrefab(down_pipe)],
							]
						},
						Dir::North => BigTileTemplate {
							parts: [
								vec![BigTilePart::FixedPrefab(down_pipe), BigTilePart::FixedPrefab(side_pipe.clone())], vec![BigTilePart::Source],
								vec![BigTilePart::Source], vec![BigTilePart::FixedPrefab(side_pipe)],
							]
						},
						Dir::East => BigTileTemplate {
							parts: [
								vec![BigTilePart::Source], vec![BigTilePart::FixedPrefab(down_pipe), BigTilePart::FixedPrefab(side_pipe.clone())],
								vec![BigTilePart::FixedPrefab(side_pipe)], vec![BigTilePart::Source]
							]
						},
						Dir::West => BigTileTemplate {
							parts: [
								vec![BigTilePart::Source], vec![BigTilePart::FixedPrefab(side_pipe.clone())],
								vec![BigTilePart::FixedPrefab(down_pipe), BigTilePart::FixedPrefab(side_pipe)], vec![BigTilePart::Source]
							]
						},
						_ => BIG_TILE_FILL.clone()
					}
				}
				["obj", "machinery", "atmospherics", ..] =>
					BIG_TILE_FILL.clone(),

				["turf", "unsimulated", "wall", "trench", "side", ..] => big_tile_template!(
					BigTilePart::Source, BigTilePart::Source,
					BigTilePart::FixedPrefab(Prefab{
						path: "/turf/space/fluid".to_string(),
						vars: Default::default(),
					}),
					BigTilePart::FixedPrefab(Prefab{
						path: "/turf/space/fluid".to_string(),
						vars: Default::default(),
					})
				),
				["turf", "space", "fluid", "warp_z5", "edge", ..] => big_tile_template!(
					BigTilePart::Source, BigTilePart::Source,
					BigTilePart::FixedPrefab(Prefab{
						path: "/turf/space/fluid_warp_z5".to_string(),
						vars: Default::default(),
					}),
					BigTilePart::FixedPrefab(Prefab{
						path: "/turf/space/fluid/warp_z5".to_string(),
						vars: Default::default(),
					})
				),
				["obj", "machinery", "power", "pt_laser", ..] |
				["obj", "machinery", "power", "furnace", ..] => 
					big_tile_upscale_dynamic(prefab, &objtree),
				["obj", "machinery", "portable_atmospherics", ..] |
				["obj", "machinery", "vehicle", ..] |
				["obj", "machinery", "mass_driver", ..] |
				["obj", "machinery", "launcher_loader", ..] |
				["obj", "decal", "poster", "wallsign", "stencil", ..] |
				["obj", "machinery", "networked", "telepad", ..] |
				["obj", "machinery", "turret", ..] |
				["obj", "submachine", "chicken_incubator", ..] |
				["obj", "machinery", "cargo_router", ..] |
				["obj", "machinery", "power", ..] =>
					BIG_TILE_FILL.clone(),
				["turf", "simulated", "wall", "auto", "shuttle", ..] =>
					BIG_TILE_UPSCALE_TURF.clone(),
				["obj", "machinery", ..] |
				["obj", "shrub", ..] |
				["obj", "submachine", ..] |
				["obj", "cryotron", ..] |
				["obj", "overlay", ..] |
				["obj", "plasticflaps", ..] |
				["obj", "decal", "cleanable", "ripped_poster", ..] |
				["obj", "dartboard", ..] |
				["obj", "lattice", ..] |
				["obj", "grille", "catwalk", ..] |
				["obj", "decal", "poster", ..] |
				["obj", "effects", "background_objects", ..] |
				["obj", "tree1", ..] |
				["obj", "indestructible", ..] |
				["obj", "fitness", "speedbag", ..] |
				["obj", "barber_pole", ..] |
				["obj", "decal", "fakeobjects", ..] |
				["obj", "voting_box", ..] |
				["obj", "securearea", ..] |
				["obj", "disposaloutlet", ..] |
				["obj", "reagent_dispensers", "watertank", "fountain", ..] |
				["obj", "decal", "cleanable", "cobweb", ..] |
				["obj", "decal", "cleanable", "cobweb2", ..] |
				["obj", "item", "storage", "toilet", ..] |
				["obj", "kitchenspike", ..] |
				["obj", "juggleplaque", ..] |
				["obj", "item", "instrument", "large", ..] |
				["obj", "monkeyplant", ..] |
				["obj", "player_piano", ..] => {
					let anchored = get_var(prefab, &objtree, "anchored").and_then(Constant::to_float).unwrap_or(1.) != 0.;
					let dense = get_var(prefab, &objtree, "density").and_then(Constant::to_float).unwrap_or(1.) != 0.;
					if anchored || !dense {
						big_tile_upscale_dynamic(prefab, &objtree)
					} else {
						BIG_TILE_FILL.clone()
					}
				}
				_ => BIG_TILE_FILL.clone(),
			};
			apply_big_tile(&mut out_map, &big_tile, &mut_prefab, coord.xy());
		}
	}
	println!("tiles placed");

	out_map.finish().unwrap()
}