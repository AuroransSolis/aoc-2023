const MAXR: usize = 12;
const MAXG: usize = 13;
const MAXB: usize = 14;

#[derive(PartialEq, Eq)]
enum Mode {
    NewGame,
    ContinueGame,
    FinishLine,
}

pub fn part1(input: &str) -> usize {
    let input = input.as_bytes();
    let mut sum = 0;
    let mut mode = Mode::NewGame;
    let mut game_id = 0;
    let mut game_maxr = 0;
    let mut game_maxg = 0;
    let mut game_maxb = 0;
    let mut i = 0;
    while i < input.len() {
        match mode {
            Mode::FinishLine => {
                if input[i] == b'\n' {
                    mode = Mode::NewGame;
                }
                i += 1;
                continue;
            }
            Mode::NewGame => {
                i += 5;
                game_id = 0;
                game_maxr = 0;
                game_maxg = 0;
                game_maxb = 0;
                while input[i] != b':' {
                    game_id = game_id * 10 + (input[i] - b'0') as usize;
                    i += 1;
                }
                i += 2;
            }
            Mode::ContinueGame => i += 1,
        }
        let num = read_num(input, &mut i);
        if input[i] == b'r' {
            game_maxr = game_maxr.max(num);
            i += 3;
        } else if input[i] == b'g' {
            game_maxg = game_maxg.max(num);
            i += 5;
        } else {
            game_maxb = game_maxb.max(num);
            i += 4;
        }
        if game_maxr > MAXR || game_maxg > MAXG || game_maxb > MAXB {
            mode = Mode::FinishLine;
        } else {
            if input[i] == b'\n' {
                mode = Mode::NewGame;
                sum += game_id;
            } else {
                mode = Mode::ContinueGame;
            }
            i += 1;
        }
    }
    sum
}

pub fn part2(input: &str) -> usize {
    let input = input.as_bytes();
    let mut sum = 0;
    let mut mode = Mode::NewGame;
    let mut game_maxr = 0;
    let mut game_maxg = 0;
    let mut game_maxb = 0;
    let mut i = 0;
    while i < input.len() {
        match mode {
            Mode::NewGame => {
                i += 5;
                game_maxr = 0;
                game_maxg = 0;
                game_maxb = 0;
                while input[i] != b':' {
                    i += 1;
                }
                i += 2;
            }
            _ => i += 1,
        }
        let num = read_num(input, &mut i);
        if input[i] == b'r' {
            game_maxr = game_maxr.max(num);
            i += 3;
        } else if input[i] == b'g' {
            game_maxg = game_maxg.max(num);
            i += 5;
        } else {
            game_maxb = game_maxb.max(num);
            i += 4;
        }
        if input[i] == b'\n' {
            mode = Mode::NewGame;
            sum += game_maxr * game_maxg * game_maxb;
        } else {
            mode = Mode::ContinueGame;
        }
        i += 1;
    }
    sum
}

fn read_num(input: &[u8], i: &mut usize) -> usize {
    let mut sum = 0;
    while input[*i] != b' ' {
        sum = sum * 10 + (input[*i] - b'0') as usize;
        *i += 1;
    }
    *i += 1;
    sum
}

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    use std::fs::read_to_string;

    #[test]
    fn read_num_test() {
        let input = b"20 ";
        let mut i = 0;
        assert_eq!(super::read_num(input, &mut i), 20);
    }

    #[test]
    fn p1test() {
        let input = read_to_string("input/d2test.txt").unwrap();
        assert_eq!(super::part1(&input), 8);
    }

    #[test]
    fn p2test() {
        let input = read_to_string("input/d2test.txt").unwrap();
        assert_eq!(super::part2(&input), 2286);
    }
}
