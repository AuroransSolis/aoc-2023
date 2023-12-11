pub fn part1(input: &str) -> usize {
    let input = input.as_bytes();
    let mut winning_vals = Vec::new();
    let mut sum = 0;
    let mut i = 0;
    while i < input.len() {
        let mut line_val = 0;
        i += 6;
        while input[i] != b':' {
            i += 1;
        }
        i += 2;
        while input[i] != b'|' {
            let num = read_num(input, i);
            winning_vals.push(num);
            i += 3;
        }
        i += 2;
        while i < input.len() && input[i] != b'C' {
            let num = read_num(input, i);
            line_val = match (winning_vals.contains(&num), line_val) {
                (true, 0) => 1,
                (true, _) => line_val * 2,
                (false, _) => line_val,
            };
            i += 3;
        }
        sum += line_val;
        winning_vals.clear();
    }
    sum
}

pub fn part2(input: &str) -> usize {
    let input = input.as_bytes();
    let mut winning_vals = Vec::new();
    let mut counts = Vec::new();
    let mut sum = 0;
    let mut i = 0;
    while i < input.len() {
        let mut line_val = 0;
        i += 6;
        while input[i] != b':' {
            i += 1;
        }
        i += 2;
        while input[i] != b'|' {
            let num = read_num(input, i);
            winning_vals.push(num);
            i += 3;
        }
        i += 2;
        while i < input.len() && input[i] != b'C' {
            let num = read_num(input, i);
            line_val += winning_vals.contains(&num) as usize;
            i += 3;
        }
        counts.push(line_val);
        winning_vals.clear();
    }
    for i in (0..counts.len() - 1).rev() {
        let card_count = 1 + counts[i + 1..].iter().take(counts[i]).sum::<usize>();
        counts[i] = card_count;
        sum += card_count;
    }
    sum + 1
}

fn read_num(input: &[u8], i: usize) -> u8 {
    let tens = if input[i] == b' ' {
        0
    } else {
        (input[i] - b'0') * 10
    };
    let ones = input[i + 1] - b'0';
    tens + ones
}

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    use std::fs::read_to_string;

    #[test]
    fn p1test() {
        let input = read_to_string("input/d4test.txt").unwrap();
        assert_eq!(super::part1(&input), 13);
    }

    #[test]
    fn p2test() {
        let input = read_to_string("input/d4test.txt").unwrap();
        assert_eq!(super::part2(&input), 30);
    }
}
