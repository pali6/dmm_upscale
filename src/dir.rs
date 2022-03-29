use dm::constants::Constant;
use dmmtools::dmm::Coord3;
use std::{convert::TryFrom, fmt::{Display, Formatter}};

use crate::coord::shift_coord;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Dir {
	North = 1,
	South = 2,
	East = 4,
	West = 8,
	NorthEast = 5,
	NorthWest = 9,
	SouthEast = 6,
	SouthWest = 10,
}

pub const DIRS: [Dir; 8] = [
	Dir::North,
	Dir::South,
	Dir::West,
	Dir::East,
	Dir::NorthWest,
	Dir::NorthEast,
	Dir::SouthWest,
	Dir::SouthEast,
];

pub const CARDINAL_DIRS : [Dir; 4] = [
	Dir::North,
	Dir::South,
	Dir::West,
	Dir::East,
];

impl Dir {
	pub fn flip(self) -> Dir {
		match self {
			Dir::North => Dir::South,
			Dir::South => Dir::North,
			Dir::West => Dir::East,
			Dir::East => Dir::West,
			Dir::NorthWest => Dir::SouthEast,
			Dir::NorthEast => Dir::SouthWest,
			Dir::SouthWest => Dir::NorthEast,
			Dir::SouthEast => Dir::NorthWest,
		}
	}

	pub fn to_coord_shift(self) -> (i32, i32) {
		match self {
			Dir::North => (0, 0 + 1),
			Dir::South => (0, 0 - 1),
			Dir::West => (0 - 1, 0),
			Dir::East => (0 + 1, 0),
			Dir::NorthWest => (0 - 1, 0 + 1),
			Dir::NorthEast => (0 + 1, 0 + 1),
			Dir::SouthWest => (0 - 1, 0 - 1),
			Dir::SouthEast => (0 + 1, 0 - 1),
		}
	}

	pub fn shift_coord(self, coord: Coord3) -> Coord3 {
		shift_coord(coord, self.to_coord_shift())
	}

	pub fn is_cardinal(self) -> bool {
		match self {
			Dir::North | Dir::South | Dir::West | Dir::East => true,
			_ => false,
		}
	}

	pub fn turn_clockwise(self) -> Dir {
		match self {
			Dir::North => Dir::East,
			Dir::East => Dir::South,
			Dir::South => Dir::West,
			Dir::West => Dir::North,
			Dir::NorthEast => Dir::SouthEast,
			Dir::SouthEast => Dir::SouthWest,
			Dir::SouthWest => Dir::NorthWest,
			Dir::NorthWest => Dir::NorthEast,
		}
	}

	pub fn turn_counterclockwise(self) -> Dir {
		match self {
			Dir::North => Dir::West,
			Dir::East => Dir::North,
			Dir::South => Dir::East,
			Dir::West => Dir::South,
			Dir::NorthEast => Dir::NorthWest,
			Dir::SouthEast => Dir::NorthEast,
			Dir::SouthWest => Dir::SouthEast,
			Dir::NorthWest => Dir::SouthWest,
		}
	}

	pub fn turn(self, amount: i32) -> Dir {
		let mut dir = self;
		for _ in 0..((amount % 4 + 4) % 4) {
			dir = dir.turn_clockwise();
		}
		dir
	}

	pub fn mirror(self) -> Dir {
		match self {
			Dir::East => Dir::West,
			Dir::West => Dir::East,
			Dir::NorthEast => Dir::NorthWest,
			Dir::SouthEast => Dir::SouthWest,
			Dir::SouthWest => Dir::SouthEast,
			Dir::NorthWest => Dir::NorthEast,
			x => x,
		}
	}

	pub fn mirror_turn(self, do_mirror: bool, amount: i32) -> Dir {
		if do_mirror {
			self.mirror().turn(amount)
		}
		else {
			self.turn(amount)
		}
	}

	pub fn turn_mirror(self, amount: i32, do_mirror: bool) -> Dir {
		if do_mirror {
			self.turn(amount).mirror()
		}
		else {
			self.turn(amount)
		}
	}

	pub fn to_constant(self) -> Constant {
		Constant::Float((self as i32) as f32)
	}
}

impl TryFrom<i32> for Dir {
	type Error = ();

	fn try_from(v: i32) -> Result<Self, Self::Error> {
		match v {
			x if x == Self::North as i32 => Ok(Self::North),
			x if x == Self::South as i32 => Ok(Self::South),
			x if x == Self::West as i32 => Ok(Self::West),
			x if x == Self::East as i32 => Ok(Self::East),
			x if x == Self::NorthWest as i32 => Ok(Self::NorthWest),
			x if x == Self::NorthEast as i32 => Ok(Self::NorthEast),
			x if x == Self::SouthWest as i32 => Ok(Self::SouthWest),
			x if x == Self::SouthEast as i32 => Ok(Self::SouthEast),
			_ => Err(()),
		}
	}
}

impl TryFrom<f32> for Dir {
	type Error = ();

	fn try_from(v: f32) -> Result<Self, Self::Error> {
		Self::try_from(v as i32)
	}
}

impl TryFrom<&Constant> for Dir {
	type Error = ();

	fn try_from(v: &Constant) -> Result<Self, Self::Error> {
		match v {
			&Constant::Float(f) => Self::try_from(f),
			_ => Err(()),
		}
	}
}

pub fn pixel_shift_to_dir((x, y): (f32, f32)) -> Option<Dir> {
	match (x.signum() as i32, y.signum() as i32) {
		(1, 0) => Some(Dir::East),
		(0, -1) => Some(Dir::South),
		(-1, 0) => Some(Dir::West),
		(0, 1) => Some(Dir::North),
		(1, -1) => Some(Dir::SouthEast),
		(-1, -1) => Some(Dir::SouthWest),
		(1, 1) => Some(Dir::NorthEast),
		(-1, 1) => Some(Dir::NorthWest),
		_ => None,
	}
}

pub fn pixel_shift_to_cardinal_dir((x, y): (f32, f32)) -> Option<Dir> {
	if x.abs() > y.abs() {
		if x > 0.0 {
			Some(Dir::East)
		} else if x < 0.0 {
			Some(Dir::West)
		}
		else {
			None
		}
	}
	else {
		if y > 0.0 {
			Some(Dir::North)
		} else if y < 0.0 {
			Some(Dir::South)
		}
		else {
			None
		}
	}
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DirCent {
	Center = 0,
	North = 1,
	South = 2,
	East = 4,
	West = 8,
	NorthEast = 5,
	NorthWest = 9,
	SouthEast = 6,
	SouthWest = 10,
}

impl Display for DirCent {
	fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
		write!(f, "{}", *self as i32)
	}
}

impl DirCent {
	pub fn to_dir(self) -> Option<Dir> {
		Some(match self {
			DirCent::Center => return None,
			DirCent::North => Dir::North,
			DirCent::South => Dir::South,
			DirCent::East => Dir::East,
			DirCent::West => Dir::West,
			DirCent::NorthEast => Dir::NorthEast,
			DirCent::NorthWest => Dir::NorthWest,
			DirCent::SouthEast => Dir::SouthEast,
			DirCent::SouthWest => Dir::SouthWest,
		})
	}

	pub fn from_dir(dir: Dir) -> DirCent {
		match dir {
			Dir::North => DirCent::North,
			Dir::South => DirCent::South,
			Dir::East => DirCent::East,
			Dir::West => DirCent::West,
			Dir::NorthEast => DirCent::NorthEast,
			Dir::NorthWest => DirCent::NorthWest,
			Dir::SouthEast => DirCent::SouthEast,
			Dir::SouthWest => DirCent::SouthWest,
		}
	}

	pub fn to_constant(self) -> Constant {
		Constant::Float((self as i32) as f32)
	}

	pub fn flip(self) -> DirCent {
		self.to_dir().map(Dir::flip).map(DirCent::from_dir).unwrap_or(self)
	}

	pub fn is_cardinal(self) -> bool {
		self.to_dir().map(Dir::is_cardinal).unwrap_or(false)
	}

	pub fn turn_clockwise(self) -> DirCent {
		self.to_dir().map(Dir::turn_clockwise).map(DirCent::from_dir).unwrap_or(self)
	}

	pub fn turn_counterclockwise(self) -> DirCent {
		self.to_dir().map(Dir::turn_counterclockwise).map(DirCent::from_dir).unwrap_or(self)
	}

	pub fn turn(self, amount: i32) -> DirCent {
		self.to_dir().map(|dir| dir.turn(amount)).map(DirCent::from_dir).unwrap_or(self)
	}

	pub fn mirror(self) -> DirCent {
		self.to_dir().map(Dir::mirror).map(DirCent::from_dir).unwrap_or(self)
	}

	pub fn mirror_turn(self, do_mirror: bool, amount: i32) -> DirCent {
		self.to_dir().map(|dir| dir.mirror_turn(do_mirror, amount)).map(DirCent::from_dir).unwrap_or(self)
	}

	pub fn turn_mirror(self, amount: i32, do_mirror: bool) -> DirCent {
		self.to_dir().map(|dir| dir.turn_mirror(amount, do_mirror)).map(DirCent::from_dir).unwrap_or(self)
	}
}

impl TryFrom<i32> for DirCent {
	type Error = ();

	fn try_from(v: i32) -> Result<Self, Self::Error> {
		match v {
			x if x == Self::Center as i32 => Ok(Self::Center),
			x => Ok(DirCent::from_dir(Dir::try_from(x)?))
		}
	}
}

impl TryFrom<f32> for DirCent {
	type Error = ();

	fn try_from(v: f32) -> Result<Self, Self::Error> {
		Self::try_from(v as i32)
	}
}

impl TryFrom<&Constant> for DirCent {
	type Error = ();

	fn try_from(v: &Constant) -> Result<Self, Self::Error> {
		match v {
			&Constant::Float(f) => Self::try_from(f),
			_ => Err(()),
		}
	}
}

impl From<Dir> for DirCent {
	fn from(v: Dir) -> Self {
		DirCent::from_dir(v)
	}
}

impl TryFrom<DirCent> for Dir {
	type Error = ();

	fn try_from(v: DirCent) -> Result<Self, Self::Error> {
		v.to_dir().ok_or(())
	}
}