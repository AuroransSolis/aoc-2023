pub fn part1(input: &str) -> usize {
    let input = input.as_bytes();
    let mut process = Process1::new(&input);
    let mut sum = 0;
    while let Some(val) = process.step() {
        sum += val;
    }
    sum
}

#[derive(Debug)]
struct Process1<'a> {
    r0: &'a [u8],
    r1: &'a [u8],
    r2: &'a [u8],
    rest: &'a [u8],
}

impl<'a> Process1<'a> {
    fn new(s: &'a [u8]) -> Self {
        let mut i = 0;
        let mut r1 = s;
        let mut r2 = s;
        let set = [&mut r1, &mut r2];
        let mut set_ind = 0;
        let mut last_start = 0;
        while set_ind < set.len() {
            if s[i] == b'\n' {
                *set[set_ind] = &s[last_start..i];
                set_ind += 1;
                last_start = i + 1;
            }
            i += 1;
        }
        let rest = &s[i..];
        Process1 {
            r0: &s[0..0],
            r1,
            r2,
            rest,
        }
    }

    fn step(&mut self) -> Option<usize> {
        if self.r1.is_empty() {
            return None;
        }
        let rowval = if self.r0.is_empty() {
            process_2row(self.r1, self.r2)
        } else if self.r2.is_empty() {
            process_2row(self.r1, self.r0)
        } else {
            process_3row(self.r0, self.r1, self.r2)
        };
        self.r0 = self.r1;
        self.r1 = self.r2;
        self.r2 = &self.rest.get(0..self.r1.len()).unwrap_or(&self.rest[0..0]);
        self.rest = &self
            .rest
            .get(self.r1.len() + 1..)
            .unwrap_or(&self.rest[0..0]);
        Some(rowval)
    }
}

fn process_2row(a: &[u8], b: &[u8]) -> usize {
    let mut sum = 0;
    let mut i = 0;
    let mut has_symbol = false;
    let mut prev = 0;
    while i < a.len() {
        let cur = a[i];
        if cur.is_ascii_digit() {
            prev = prev * 10 + (cur - b'0') as usize;
            has_symbol = has_symbol || is_symbol(b[i]);
        } else if cur == b'.' {
            let vert_symbol = is_symbol(b[i]);
            if has_symbol || vert_symbol {
                sum += prev;
            }
            has_symbol = vert_symbol;
            prev = 0;
        } else {
            sum += prev;
            has_symbol = true;
            prev = 0;
        }
        i += 1;
    }
    sum
}

fn process_3row(a: &[u8], b: &[u8], c: &[u8]) -> usize {
    let mut sum = 0;
    let mut i = 0;
    let mut has_symbol = false;
    let mut prev = 0;
    while i < b.len() {
        let cur = b[i];
        if cur.is_ascii_digit() {
            prev = prev * 10 + (cur - b'0') as usize;
            has_symbol = has_symbol || is_symbol(a[i]) || is_symbol(c[i]);
        } else if cur == b'.' {
            let vert_symbol = is_symbol(a[i]) || is_symbol(c[i]);
            if has_symbol || vert_symbol {
                sum += prev;
            }
            has_symbol = vert_symbol;
            prev = 0;
        } else {
            sum += prev;
            has_symbol = true;
            prev = 0;
        }
        i += 1;
    }
    if has_symbol {
        sum += prev;
    }
    sum
}

fn is_symbol(byte: u8) -> bool {
    !(byte.is_ascii_digit()) && byte != b'.'
}

pub fn part2(input: &str) -> usize {
    0
}

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    use std::fs::read_to_string;

    #[test]
    fn p1test1() {
        let input = read_to_string("input/d3test.txt").unwrap();
        assert_eq!(super::part1(&input), 4361);
    }
}
