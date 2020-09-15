use std::ops;

use num_traits::PrimInt;

pub struct IntSet<T> {
	ranges: Vec<ops::Range<T>>
}

enum IntPlacement {
	In(usize),
	Before(usize),
}

impl<T: PrimInt> IntSet<T> {
	pub fn new() -> Self {
		IntSet {
			ranges: vec![],
		}
	}
	
	// Find the placement of 'val' between ranges at indices 'start' and 'end'
	fn find(&self, val: T, start: usize, end: usize) -> IntPlacement {
		if end - start == 0 {
			IntPlacement::Before(start)
		} else if end - start == 1 {
			let range = &self.ranges[start];
			if val < range.start {
				IntPlacement::Before(start)
			} else if val >= range.end {
				IntPlacement::Before(start + 1)
			} else {
				IntPlacement::In(start)
			}
		} else {
			let mid_idx = (end-start)/2;
			if val < self.ranges[mid_idx].start {
				self.find(val, start, mid_idx)
			} else {
				self.find(val, mid_idx, end)
			}
		}
	}
	
	fn try_merge(&mut self, idx: usize) {
		if self.ranges[idx].end == self.ranges[idx+1].start {
			self.ranges[idx].end = self.ranges[idx+1].end;
			self.ranges.remove(idx+1);
		}
	}
	
	pub fn find_hole(&self) -> T {
		if self.ranges.len() == 0 {
			T::zero()
		} else {
			self.ranges[0].end
		}
	}
	
	pub fn add(&mut self, val: T) {
		match self.find(val, 0, self.ranges.len()) {
			IntPlacement::In(_idx) => panic!("Int is already in IntSet"),
			IntPlacement::Before(idx) => {
				if idx > 0 && self.ranges[idx-1].end == val {
					let before = &mut self.ranges[idx-1];
					before.end = before.end + T::one();
					if idx < self.ranges.len() {
						self.try_merge(idx-1);
					}
				} else if idx < self.ranges.len() && self.ranges[idx].start == val + T::one() {
					let after = &mut self.ranges[idx];
					after.start = after.start - T::one();
					if idx > 0 {
						self.try_merge(idx-1);
					}
				} else {
					self.ranges.insert(idx, val..(val + T::one()));
				}
			},
		}
	}
}
