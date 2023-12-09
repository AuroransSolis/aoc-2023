pub fn part1(input: &str) -> usize {
    let input = input.as_bytes();
    let mut sum = 0;
    let mut i = 0;
    let mut first = 10;
    let mut second = 10;
    while i < input.len() {
        if input[i] == b'\n' {
            if second > 9 {
                second = first;
            }
            let new = first * 10 + second;
            sum += new;
            first = 10;
            second = 10;
        } else if input[i].is_ascii_digit() {
            if first > 9 {
                first = (input[i] - b'0') as usize;
            } else {
                second = (input[i] - b'0') as usize;
            }
        }
        i += 1;
    }
    sum
}

pub fn part2(input: &str) -> usize {
    let input = input.as_bytes();
    let mut sum = 0;
    let mut i = 0;
    let mut first = 10;
    let mut second = 10;
    while i < input.len() {
        if input[i] == b'\n' {
            if second > 9 {
                second = first;
            }
            let new = first * 10 + second;
            sum += new;
            first = 10;
            second = 10;
            i += 1;
        } else if input[i].is_ascii_digit() {
            let digit = (input[i] - b'0') as usize;
            if first > 9 {
                first = digit;
            } else {
                second = digit;
            }
            i += 1;
        } else {
            let old = i;
            if let Some(digit) = try_get_digit(input, &mut i) {
                if first > 9 {
                    first = digit;
                } else {
                    second = digit;
                }
            }
            if old == i {
                i += 1;
            }
        }
    }
    sum
}

fn try_get_digit(input: &[u8], cursor: &mut usize) -> Option<usize> {
    match input[*cursor] {
        b'o' => {
            *cursor += 1;
            try_get_1(input, cursor)
        }
        b't' => {
            *cursor += 1;
            try_get_23(input, cursor)
        }
        b'f' => {
            *cursor += 1;
            try_get_45(input, cursor)
        }
        b's' => {
            *cursor += 1;
            try_get_67(input, cursor)
        }
        b'e' => {
            *cursor += 1;
            try_get_8(input, cursor)
        }
        b'n' => {
            *cursor += 1;
            try_get_9(input, cursor)
        }
        _ => None,
    }
}

fn try_get_1(input: &[u8], cursor: &mut usize) -> Option<usize> {
    let mut i = *cursor;
    if input[i] == b'n' {
        i += 1;
        if input[i] == b'e' {
            *cursor = i;
            Some(1)
        } else {
            None
        }
    } else {
        None
    }
}

fn try_get_23(input: &[u8], cursor: &mut usize) -> Option<usize> {
    let mut i = *cursor;
    if input[i] == b'w' {
        i += 1;
        if input[i] == b'o' {
            *cursor = i;
            Some(2)
        } else {
            None
        }
    } else if input[i] == b'h' {
        i += 1;
        if input[i] == b'r' {
            i += 1;
            if input[i] == b'e' {
                i += 1;
                if input[i] == b'e' {
                    *cursor = i;
                    Some(3)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn try_get_45(input: &[u8], cursor: &mut usize) -> Option<usize> {
    let mut i = *cursor;
    if input[i] == b'o' {
        i += 1;
        if input[i] == b'u' {
            i += 1;
            if input[i] == b'r' {
                *cursor = i;
                Some(4)
            } else {
                None
            }
        } else {
            None
        }
    } else if input[i] == b'i' {
        i += 1;
        if input[i] == b'v' {
            i += 1;
            if input[i] == b'e' {
                *cursor = i;
                Some(5)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn try_get_67(input: &[u8], cursor: &mut usize) -> Option<usize> {
    let mut i = *cursor;
    if input[i] == b'i' {
        i += 1;
        if input[i] == b'x' {
            *cursor = i;
            Some(6)
        } else {
            None
        }
    } else if input[i] == b'e' {
        i += 1;
        if input[i] == b'v' {
            i += 1;
            if input[i] == b'e' {
                i += 1;
                if input[i] == b'n' {
                    *cursor = i;
                    Some(7)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn try_get_8(input: &[u8], cursor: &mut usize) -> Option<usize> {
    let mut i = *cursor;
    if input[i] == b'i' {
        i += 1;
        if input[i] == b'g' {
            i += 1;
            if input[i] == b'h' {
                i += 1;
                if input[i] == b't' {
                    Some(8)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn try_get_9(input: &[u8], cursor: &mut usize) -> Option<usize> {
    let mut i = *cursor;
    if input[i] == b'i' {
        i += 1;
        if input[i] == b'n' {
            i += 1;
            if input[i] == b'e' {
                *cursor = i;
                Some(9)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    use std::fs::read_to_string;

    #[test]
    fn p1test() {
        let input = read_to_string("input/d1test1.txt").unwrap();
        assert_eq!(super::part1(&input), 142);
    }

    #[test]
    fn p2test1() {
        let input = read_to_string("input/d1test2.txt").unwrap();
        assert_eq!(super::part2(&input), 281);
    }

    #[test]
    fn p2test2() {
        let input = read_to_string("input/d1test3.txt").unwrap();
        assert_eq!(super::part2(&input), 316);
    }
}
