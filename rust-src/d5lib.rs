pub fn part1(input: &str) -> usize {
    let input = input.as_bytes();
    let mut seeds = Vec::new();
    let mut i = 7;
    // read seeds
    let mut acc = 0;
    loop {
        if input[i] == b'\n' {
            seeds.push((false, acc));
            break;
        } else if input[i] == b' ' {
            seeds.push((false, acc));
            acc = 0;
        } else {
            acc = acc * 10 + (input[i] - b'0') as usize;
        }
        i += 1;
    }
    // skip past newlines
    i += 2;
    apply_all_mappings(input, &mut i, &mut seeds);
    let mut min = !0;
    for (_, val) in seeds {
        min = min.min(val);
    }
    min
}

fn apply_all_mappings(input: &[u8], i: &mut usize, seeds: &mut [(bool, usize)]) {
    // skip past "seed-to-soil map:\n"
    *i += 18;
    apply_mapping(input, i, seeds);
    // skip past "\nsoil-to-fertilizer map:\n"
    *i += 25;
    apply_mapping(input, i, seeds);
    // skip past "\nfertilizer-to-water map:\n"
    *i += 26;
    apply_mapping(input, i, seeds);
    // skip past "\nwater-to-light map:\n"
    *i += 22;
    apply_mapping(input, i, seeds);
    // skip past "\nlight-to-temperature map:\n"
    *i += 27;
    apply_mapping(input, i, seeds);
    // skip past "\ntemperature-to-humidity map:\n"
    *i += 30;
    apply_mapping(input, i, seeds);
    // skip past "\nhumidity-to-location map:\n"
    *i += 27;
    apply_mapping(input, i, seeds);
}

fn apply_mapping(input: &[u8], i: &mut usize, vals: &mut [(bool, usize)]) {
    let mut dst;
    let mut src = 0;
    let mut len = 0;
    while *i < input.len() {
        let cur = input[*i];
        if cur == b'\n' {
            break;
        }
        dst = (cur - b'0') as usize;
        *i += 1;
        while input[*i] != b' ' {
            dst = dst * 10 + (input[*i] - b'0') as usize;
            *i += 1;
        }
        *i += 1;
        while input[*i] != b' ' {
            src = src * 10 + (input[*i] - b'0') as usize;
            *i += 1;
        }
        *i += 1;
        while input[*i] != b'\n' {
            len = len * 10 + (input[*i] - b'0') as usize;
            *i += 1;
        }
        *i += 1;
        let range = src..=(src + len);
        for (updated, val) in vals.iter_mut() {
            if !*updated && range.contains(val) {
                *val = *val - src + dst;
                *updated = true;
            }
        }
        src = 0;
        len = 0;
    }
    vals.iter_mut().for_each(|(updated, _)| *updated = false);
}

pub fn part2(input: &str) -> usize {
    // this part sucks pretty hard so im just not going to do it yet
    0
}

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    use std::fs::read_to_string;

    #[test]
    fn part1test() {
        let input = read_to_string("input/d5test.txt").unwrap();
        assert_eq!(super::part1(&input), 35);
    }

    #[test]
    fn part2test() {
        let input = read_to_string("input/d5test.txt").unwrap();
        assert_eq!(super::part2(&input), 46);
    }
}
