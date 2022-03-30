use std::{sync::Arc, cmp::min, cmp::max};

use lazy_static::lazy_static;

use dm::{constants::Constant, objtree::{ObjectTree, self}};
use dmmtools::dmm::{Prefab, Coord2};

use crate::{WipMap, dir::Dir, dir::DirCent, coord::shift_coord2, get_var};

pub const TILE_SIZE : f32 = 32.;

#[derive(Clone)]
pub enum BigTilePart{
	FixedPrefab(Prefab),
	Source,
	ModifiedSource(Arc<dyn Sync + Send + Fn(&Prefab) -> Prefab>),
	Empty,
}

use BigTilePart::*;

#[derive(Clone)]
pub struct BigTileTemplate {
	pub parts: [Vec<BigTilePart>; 4],
}

macro_rules! big_tile_template {
	($p1:expr, $p2:expr, $p3:expr, $p4:expr) => {
		BigTileTemplate {
			parts: [
				vec![$p1],
				vec![$p2],
				vec![$p3],
				vec![$p4],
			],
		}
	};
}

pub(crate) use big_tile_template;

macro_rules! big_tile_modification {
	($($varname:expr => $value:expr),+) => {
		BigTilePart::ModifiedSource(std::sync::Arc::new(move |prefab| {
			let mut output = prefab.clone();
			$(output.vars.insert($varname.to_string(), $value.clone());)*
			output
		}))
	};
}

macro_rules! big_tile_modification_fun {
	($prefab: ident, $($varname:expr => $value:expr),+) => {
		BigTilePart::ModifiedSource(std::sync::Arc::new(|$prefab| {
			let mut output = $prefab.clone();
			$(output.vars.insert($varname.to_string(), $value.clone());)*
			output
		}))
	};
}

pub(crate) use big_tile_modification;

lazy_static! {
	pub static ref BIG_TILE_FILL: BigTileTemplate = big_tile_template!(
		Source, Source,
		Source, Source
	);
}

lazy_static! {
	pub static ref BIG_TILE_EMPTY: BigTileTemplate = big_tile_template!(
		Empty, Empty,
		Empty, Empty
	);
}

lazy_static! {
	pub static ref BIG_TILE_JUST_BOTTOM_LEFT: BigTileTemplate = big_tile_template!(
		Empty, Empty,
		Source, Empty
	);
}

pub fn big_tile_two_on_side(dir: Dir) -> BigTileTemplate {
	match dir {
		Dir::North => big_tile_template!(
			Source, Source,
			Empty, Empty
		),
		Dir::South => big_tile_template!(
			Empty, Empty,
			Source, Source
		),
		Dir::West => big_tile_template!(
			Source, Empty,
			Source, Empty
		),
		Dir::East => big_tile_template!(
			Empty, Source,
			Empty, Source
		),
		Dir::NorthWest => big_tile_template!(
			Source, Empty,
			Empty, Empty
		),
		Dir::NorthEast => big_tile_template!(
			Empty, Source,
			Empty, Empty
		),
		Dir::SouthWest => big_tile_template!(
			Empty, Empty,
			Source, Empty
		),
		Dir::SouthEast => big_tile_template!(
			Empty, Empty,
			Empty, Source
		),
	}
}

lazy_static! {
	pub static ref BIG_TILE_UPSCALE: BigTileTemplate = big_tile_template!(
		Empty, Empty,
		big_tile_modification_fun!(prefab,
			"bound_width" => Constant::Float(2. * TILE_SIZE),
			"bound_height" => Constant::Float(2. * TILE_SIZE),
			"pixel_x" => Constant::Float(2. * prefab.vars.get("pixel_x").and_then(Constant::to_float).unwrap_or(0.)),
			"pixel_y" => Constant::Float(2. * prefab.vars.get("pixel_y").and_then(Constant::to_float).unwrap_or(0.)),
			"transform" => Constant::List(Box::new([
				(Constant::Float(2.), None),
				(Constant::Float(0.), None),
				(Constant::Float(TILE_SIZE / 2.), None),
				
				(Constant::Float(0.), None),
				(Constant::Float(2.), None),
				(Constant::Float(TILE_SIZE / 2.), None),
			]))
		), Empty
	);
}

lazy_static! {
	pub static ref BIG_TILE_UPSCALE_TURF: BigTileTemplate = big_tile_template!(
		Source, Source,
		big_tile_modification_fun!(prefab,
			"transform" => Constant::List(Box::new([
				(Constant::Float(2.), None),
				(Constant::Float(0.), None),
				(Constant::Float(TILE_SIZE / 2.), None),
				
				(Constant::Float(0.), None),
				(Constant::Float(2.), None),
				(Constant::Float(TILE_SIZE / 2.), None),
			])),
			"layer" => Constant::Float(2.1)
		), Source
	);
}

pub fn big_tile_upscale_dynamic(prefab: &Prefab, objtree: &ObjectTree) -> BigTileTemplate {
	let bound_width = get_var(prefab, objtree, "bound_width").and_then(Constant::to_float).unwrap_or(TILE_SIZE);
	let bound_height = get_var(prefab, objtree, "bound_height").and_then(Constant::to_float).unwrap_or(TILE_SIZE);
	let pixel_x = get_var(prefab, objtree, "pixel_x").and_then(Constant::to_float).unwrap_or(0.);
	let pixel_y = get_var(prefab, objtree, "pixel_y").and_then(Constant::to_float).unwrap_or(0.);
	let bound_x = get_var(prefab, objtree, "bound_x").and_then(Constant::to_float).unwrap_or(0.);
	let bound_y = get_var(prefab, objtree, "bound_y").and_then(Constant::to_float).unwrap_or(0.);
	big_tile_template!(
		Empty, Empty,
		big_tile_modification!(
			"bound_width" => Constant::Float(2. * bound_width),
			"bound_height" => Constant::Float(2. * bound_height),
			"bound_x" => Constant::Float(2. * bound_x),
			"bound_y" => Constant::Float(2. * bound_y),
			"pixel_x" => Constant::Float(2. * pixel_x),
			"pixel_y" => Constant::Float(2. * pixel_y),
			"transform" => Constant::List(Box::new([
				(Constant::Float(2.), None),
				(Constant::Float(0.), None),
				(Constant::Float(bound_width / 2.), None),
				
				(Constant::Float(0.), None),
				(Constant::Float(2.), None),
				(Constant::Float(bound_height / 2.), None),
			]))
		), Empty
	)
}

pub fn big_tile_one_on_side(dir: Dir) -> BigTileTemplate {
	match dir {
		Dir::North => big_tile_template!(
			Empty, Source,
			Empty, Empty
		),
		Dir::South => big_tile_template!(
			Empty, Empty,
			Source, Empty
		),
		Dir::West => big_tile_template!(
			Source, Empty,
			Empty, Empty
		),
		Dir::East => big_tile_template!(
			Empty, Empty,
			Empty, Source
		),
		Dir::NorthWest => big_tile_template!(
			Source, Empty,
			Empty, Empty
		),
		Dir::NorthEast => big_tile_template!(
			Empty, Source,
			Empty, Empty
		),
		Dir::SouthWest => big_tile_template!(
			Empty, Empty,
			Source, Empty
		),
		Dir::SouthEast => big_tile_template!(
			Empty, Empty,
			Empty, Source
		),
	}
}

pub fn big_tile_one_on_side_biased(dir: Dir) -> BigTileTemplate {
	match dir {
		Dir::North => big_tile_template!(
			Source, Empty,
			Empty, Empty
		),
		Dir::South => big_tile_template!(
			Empty, Empty,
			Source, Empty
		),
		Dir::West => big_tile_template!(
			Source, Empty,
			Empty, Empty
		),
		Dir::East => big_tile_template!(
			Empty, Source,
			Empty, Empty
		),
		Dir::NorthWest => big_tile_template!(
			Source, Empty,
			Empty, Empty
		),
		Dir::NorthEast => big_tile_template!(
			Empty, Source,
			Empty, Empty
		),
		Dir::SouthWest => big_tile_template!(
			Empty, Empty,
			Source, Empty
		),
		Dir::SouthEast => big_tile_template!(
			Empty, Empty,
			Empty, Source
		),
	}
}

pub const BIG_TILE_SHIFTS: [(i32, i32); 4] = [
	(0, 1), (1, 1),
	(0, 0), (1, 0),
];

pub fn apply_big_tile(out_map: &mut WipMap, big_tile: &BigTileTemplate, prefab: &Prefab, coord: Coord2) {
	let big_coord = Coord2::new(coord.x * 2 - 1, coord.y * 2 - 1);
	for (&shift, part_list) in BIG_TILE_SHIFTS.iter().zip(&big_tile.parts) {
		let shifted_coord = shift_coord2(big_coord, shift);
		for ref part in part_list {
			let prefab_to_add = match part {
				BigTilePart::FixedPrefab(p) => p.clone(),
				BigTilePart::Source => prefab.clone(),
				/*
				BigTilePart::ModifiedSource(vars) => {
					let mut p = prefab.clone();
					for (key, value) in vars.iter() {
						p.vars.insert(key.to_string(), value.clone());
					}
					p
				},
				*/
				BigTilePart::ModifiedSource(modification) => modification(prefab),
				BigTilePart::Empty => continue,
			};

			out_map.add_prefab(shifted_coord, prefab_to_add);
		}
	}
}

pub fn place_with_shift_scale(out_map: &mut WipMap, prefab: &Prefab, coord: Coord2, (scale_x, scale_y): (i32, i32)) {
	let big_coord = Coord2::new(coord.x * 2 - 1, coord.y * 2 - 1);
	for (base_x, base_y) in BIG_TILE_SHIFTS {
		let shift = (scale_x * base_x, scale_y * base_y);
		let shifted_coord = shift_coord2(big_coord, shift);
		out_map.add_prefab(shifted_coord, prefab.clone());
	}
}

pub fn big_tile_cable(icon_state: &str) -> Option<BigTileTemplate> {
	let icon_state_parts = icon_state.split("-").collect::<Vec<_>>();
	let icon_state_parts = icon_state_parts.as_slice();
	let (a, b, icon_mod) = match *icon_state_parts {
		[a, b, icon_mod] => (a, b, "-".to_owned() + icon_mod),
		[a, b] => (a, b, "".to_owned()),
		_ => return None,
	};
	let a = DirCent::try_from(a.parse::<i32>().ok()?).ok()?;
	let b = DirCent::try_from(b.parse::<i32>().ok()?).ok()?;
	let (ma, mb, unturn, unmirror) =
		[
			(0, false), (1, false), (2, false), (3, false),
			(0, true), (1, true), (2, true), (3, true)
		]
		.iter()
		.map(|(turn_amount, do_mirror)| {
			let ma = a.mirror_turn(*do_mirror, *turn_amount);
			let mb = b.mirror_turn(*do_mirror, *turn_amount);
			(min(ma, mb), max(ma, mb), -turn_amount, *do_mirror)})
		.min()
		.unwrap();

	macro_rules! modcable {
		($a:expr, $b:expr) => {
			{
				let icon_mod = icon_mod.clone();
				BigTilePart::ModifiedSource(Arc::new(move |prefab| {
					let mut output = prefab.clone();
					let reta = $a.turn_mirror(unturn, unmirror);
					let retb = $b.turn_mirror(unturn, unmirror);
					output.vars.insert("icon_state".to_string(), Constant::String(format!("{}-{}{}", min(reta, retb), max(reta, retb), icon_mod).into()));
					output
				}))
			}
		}
	}
	use DirCent::*;

	let mut result = match (ma, mb) {
		(North, South) | (East, West) =>
			BIG_TILE_FILL.clone(),
		(Center, North) =>
			BigTileTemplate {
				parts: [
					vec![
						modcable!(Center, North),
						modcable!(Center, South),
					],
					vec![
						modcable!(Center, North),
						modcable!(Center, South),
					],
					vec![
						modcable!(Center, North),
					],
					vec![
						modcable!(Center, North),
					],
				]
			},
		(Center, NorthEast) =>
			BigTileTemplate {
				parts: [
					vec![
						modcable!(Center, East),
					],
					vec![
						modcable!(Center, NorthEast),
						modcable!(Center, West),
						modcable!(Center, SouthWest),
						modcable!(Center, South),
					],
					vec![
						modcable!(Center, NorthEast),
					],
					vec![
						modcable!(Center, North),
					],
				]
			},
		(North, NorthEast) =>
			BigTileTemplate {
				parts: [
					vec![
						modcable!(North, South),
					],
					vec![
						modcable!(North, NorthEast),
						modcable!(SouthWest, NorthEast),
					],
					vec![
						modcable!(North, NorthEast),
					],
					vec![
					],
				]
			},
		(North, SouthEast) =>
			BigTileTemplate {
				parts: [
					vec![
						modcable!(North, SouthEast),
					],
					vec![
						modcable!(North, South),
					],
					vec![
					],
					vec![
						modcable!(NorthWest, SouthEast),
						modcable!(North, SouthEast),
					],
				]
			},
		(NorthEast, SouthEast) =>
			BigTileTemplate {
				parts: [
					vec![
					],
					vec![
						modcable!(NorthEast, South),
					],
					vec![
					],
					vec![
						modcable!(North, SouthEast),
					],
				]
			},
		(NorthEast, SouthWest) =>
			BigTileTemplate {
				parts: [
					vec![
					],
					vec![
						modcable!(NorthEast, SouthWest),
					],
					vec![
						modcable!(NorthEast, SouthWest),
					],
					vec![
					],
				]
			},
		(North, East) => big_tile_template!(
			modcable!(North, South), Source,
			Source, modcable!(East, West)
		),
		_ => {
			let all_dir_cables = crate::dir::DIRS.iter().map(|&dir| modcable!(Center, DirCent::from_dir(dir))).collect::<Vec<_>>();
			BigTileTemplate {
				parts: [
					all_dir_cables.clone(), all_dir_cables.clone(),
					all_dir_cables.clone(), all_dir_cables,
				]
			}
		}
	};
	turn_2x2(&mut result.parts, unturn);
	if unmirror {
		mirror_2x2(&mut result.parts);
	}
	Some(result)
}



fn turn_2x2_clockwise<T>(matrix: &mut [T; 4]) {
	matrix.swap(0, 1);
	matrix.swap(0, 3);
	matrix.swap(0, 2);
}

fn turn_2x2<T>(matrix: &mut [T; 4], amount: i32) {
	for _ in 0..((amount % 4 + 4) % 4) {
		turn_2x2_clockwise(matrix);
	}
}

fn mirror_2x2<T>(matrix: &mut [T; 4]) {
	matrix.swap(0, 1);
	matrix.swap(2, 3);
}