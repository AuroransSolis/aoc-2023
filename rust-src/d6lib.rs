pub fn part1(input: &str) -> usize {
    let input = input.as_bytes();
    let mut times = Vec::new();
    let mut dists = Vec::new();
    let mut i = 9;
    // skip to first number
    while input[i] == b' ' {
        i += 1;
    }
    // read times
    let mut time = 0;
    loop {
        if input[i] == b'\n' {
            i += 1 + 9;
            break;
        }
        if input[i] == b' ' {
            if time > 0 {
                times.push(time);
                time = 0;
            }
        } else {
            time = time * 10 + (input[i] - b'0') as usize;
        }
        i += 1;
    }
    times.push(time);
    // skip to first number
    while input[i] == b' ' {
        i += 1;
    }
    // read dists
    let mut dist = 0;
    loop {
        if input[i] == b'\n' {
            break;
        }
        if input[i] == b' ' {
            if dist > 0 {
                dists.push(dist);
                dist = 0;
            }
        } else {
            dist = dist * 10 + (input[i] - b'0') as usize;
        }
        i += 1;
    }
    dists.push(dist);
    times
        .into_iter()
        .zip(dists.into_iter())
        .map(|(t, d)| {
            let t = t as f32;
            let d = d as f32;
            let disc = (t * t - 4.0 * d).sqrt() / 2.0;
            let t2 = t / 2.0;
            let s1 = t2 - disc;
            let start = if s1.fract() > 0.0 {
                s1.floor() as usize
            } else {
                s1.floor() as usize + 1
            };
            let end = (t2 + disc).floor() as usize;
            end - start
        })
        .product()
}

pub fn part2(input: &str) -> usize {
    let input = input.as_bytes();
    let mut i = 9;
    let mut time = 0;
    // skip to first number
    while input[i] == b' ' {
        i += 1;
    }
    // read time
    loop {
        if input[i] == b'\n' {
            i += 1 + 9;
            break;
        }
        if input[i] != b' ' {
            time = time * 10 + (input[i] - b'0') as usize;
        }
        i += 1;
    }
    // skip to first number
    while input[i] == b' ' {
        i += 1;
    }
    let mut dist = 0;
    loop {
        if input[i] == b'\n' {
            break;
        }
        if input[i] != b' ' {
            dist = dist * 10 + (input[i] - b'0') as usize;
        }
        i += 1;
    }
    let time = time as f64;
    let dist = dist as f64;
    let disc = (time * time - 4.0 * dist).sqrt() / 2.0;
    let time2 = time / 2.0;
    let s1 = time2 - disc;
    let start = if s1.fract() > 0.0 {
        s1.floor() as usize
    } else {
        s1.floor() as usize + 1
    };
    let end = (time2 + disc).floor() as usize;
    end - start
}

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    use std::fs::read_to_string;

    #[test]
    fn part1test() {
        let input = read_to_string("input/d6test.txt").unwrap();
        assert_eq!(super::part1(&input), 288);
    }

    #[test]
    fn part2test() {
        let input = read_to_string("input/d6test.txt").unwrap();
        assert_eq!(super::part2(&input), 71503);
    }
}
