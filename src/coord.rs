use dmmtools::dmm::{Coord3, Coord2};


pub fn coord_from_raw((z, y, x): (usize, usize, usize), (_dim_z, dim_y, _dim_x): (usize, usize, usize)) -> Coord3 {
	Coord3 { x: x as i32 + 1, y: (dim_y - y) as i32, z: z as i32 + 1 }
}

pub fn shift_coord(coord: Coord3, (x, y): (i32, i32)) -> Coord3 {
	Coord3 { x: coord.x + x, y: coord.y + y, z: coord.z }
}

pub fn shift_coord2(coord: Coord2, (x, y): (i32, i32)) -> Coord2 {
	Coord2 { x: coord.x + x, y: coord.y + y}
}

pub fn coord_to_raw(coord: Coord3, (dim_z, dim_y, dim_x): (usize, usize, usize)) -> (usize, usize, usize) {
	assert!(coord.x >= 1 && coord.x <= dim_x as i32, "x={} not in [1, {}]", coord.x, dim_x);
	assert!(coord.y >= 1 && coord.y <= dim_y as i32, "y={} not in [1, {}]", coord.y, dim_y);
	assert!(coord.z >= 1 && coord.z <= dim_z as i32, "y={} not in [1, {}]", coord.z, dim_z);
	(coord.z as usize - 1, dim_y - coord.y as usize, coord.x as usize - 1)
}