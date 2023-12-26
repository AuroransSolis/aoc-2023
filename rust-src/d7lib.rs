use std::cmp::Ordering;

use util::ArrayVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeKind,
    FullHouse,
    FourKind,
    FiveKind,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum CardType1 {
    C2 = 0,
    C3 = 1,
    C4 = 2,
    C5 = 3,
    C6 = 4,
    C7 = 5,
    C8 = 6,
    C9 = 7,
    CT = 8,
    CJ = 9,
    CQ = 10,
    CK = 11,
    CA = 12,
}

impl CardType1 {
    fn from_byte(byte: u8) -> Self {
        match byte {
            b'2' => CardType1::C2,
            b'3' => CardType1::C3,
            b'4' => CardType1::C4,
            b'5' => CardType1::C5,
            b'6' => CardType1::C6,
            b'7' => CardType1::C7,
            b'8' => CardType1::C8,
            b'9' => CardType1::C9,
            b'T' => CardType1::CT,
            b'J' => CardType1::CJ,
            b'Q' => CardType1::CQ,
            b'K' => CardType1::CK,
            _ => CardType1::CA,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Hand1 {
    cards: [CardType1; 5],
    hand_type: HandType,
    wager: usize,
}

impl Ord for Hand1 {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.hand_type.cmp(&other.hand_type) {
            Ordering::Equal => self.cards.cmp(&other.cards),
            other => other,
        }
    }
}

impl PartialOrd for Hand1 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hand1 {
    fn read(input: &[u8], i: &mut usize) -> Self {
        let original @ [c0, c1, c2, c3, c4] = [
            input[*i],
            input[*i + 1],
            input[*i + 2],
            input[*i + 3],
            input[*i + 4],
        ]
        .map(CardType1::from_byte);
        *i += 6;
        let mut wager = (input[*i] - b'0') as usize;
        *i += 1;
        while input[*i] != b'\n' {
            wager = wager * 10 + (input[*i] - b'0') as usize;
            *i += 1;
        }
        // sorting done by sorting network
        // [(0, 3), (1, 4)]
        let [c0, c3] = pair_sort(c0, c3);
        let [c1, c4] = pair_sort(c1, c4);
        // [(0, 2), (1, 3)]
        let [c0, c2] = pair_sort(c0, c2);
        let [c1, c3] = pair_sort(c1, c3);
        // [(0, 1), (2, 4)]
        let [c0, c1] = pair_sort(c0, c1);
        let [c2, c4] = pair_sort(c2, c4);
        // [(1, 2), (3, 4)]
        let [c1, c2] = pair_sort(c1, c2);
        let [c3, c4] = pair_sort(c3, c4);
        // [(2, 3)]
        let [c2, c3] = pair_sort(c2, c3);
        // find hand type
        let eq01 = c0 == c1;
        let eq12 = c1 == c2;
        let eq23 = c2 == c3;
        let eq34 = c3 == c4;
        let hand_type = if eq01 && eq12 && eq23 && eq34 {
            HandType::FiveKind
        } else {
            let mid_3k = eq12 && eq23;
            if mid_3k && (eq01 || eq34) {
                HandType::FourKind
            } else {
                let srt_3k = eq01 && eq12;
                let end_3k = eq23 && eq34;
                if (srt_3k && eq34) || (end_3k && eq01) {
                    HandType::FullHouse
                } else if srt_3k || mid_3k || end_3k {
                    HandType::ThreeKind
                } else {
                    match [eq01, eq12, eq23, eq34] {
                        [true, false, true, false]
                        | [true, false, false, true]
                        | [false, true, false, true] => HandType::TwoPair,
                        [true, false, false, false]
                        | [false, true, false, false]
                        | [false, false, true, false]
                        | [false, false, false, true] => HandType::OnePair,
                        _ => HandType::HighCard,
                    }
                }
            }
        };
        Hand1 {
            cards: original,
            hand_type,
            wager,
        }
    }
}

fn pair_sort<T: Ord>(a: T, b: T) -> [T; 2] {
    if a > b {
        [a, b]
    } else {
        [b, a]
    }
}

pub fn part1(input: &str) -> usize {
    let input = input.as_bytes();
    let mut i = 0;
    let mut hands = Vec::new();
    while i < input.len() {
        let hand = Hand1::read(input, &mut i);
        hands.push(hand);
        i += 1;
    }
    // println!("{hands:?}");
    hands.sort_unstable();
    // eprintln!("{hands:?}");
    hands.into_iter().fold([0, 1], |[acc, rank], hand| {
        [acc + rank * hand.wager, rank + 1]
    })[0]
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum CardType2 {
    CJ = 0,
    C2 = 1,
    C3 = 2,
    C4 = 3,
    C5 = 4,
    C6 = 5,
    C7 = 6,
    C8 = 7,
    C9 = 8,
    CT = 9,
    CQ = 10,
    CK = 11,
    CA = 12,
}

impl CardType2 {
    fn from_byte(byte: u8) -> Self {
        match byte {
            b'J' => CardType2::CJ,
            b'2' => CardType2::C2,
            b'3' => CardType2::C3,
            b'4' => CardType2::C4,
            b'5' => CardType2::C5,
            b'6' => CardType2::C6,
            b'7' => CardType2::C7,
            b'8' => CardType2::C8,
            b'9' => CardType2::C9,
            b'T' => CardType2::CT,
            b'Q' => CardType2::CQ,
            b'K' => CardType2::CK,
            _ => CardType2::CA,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Hand2 {
    cards: [CardType2; 5],
    hand_type: HandType,
    wager: usize,
}

impl Ord for Hand2 {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.hand_type.cmp(&other.hand_type) {
            Ordering::Equal => self.cards.cmp(&other.cards),
            other => other,
        }
    }
}

impl PartialOrd for Hand2 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hand2 {
    fn read(input: &[u8], i: &mut usize) -> Self {
        let original @ [c0, c1, c2, c3, c4] = [
            input[*i],
            input[*i + 1],
            input[*i + 2],
            input[*i + 3],
            input[*i + 4],
        ]
        .map(CardType2::from_byte);
        *i += 6;
        let mut wager = (input[*i] - b'0') as usize;
        *i += 1;
        while input[*i] != b'\n' {
            wager = wager * 10 + (input[*i] - b'0') as usize;
            *i += 1;
        }
        let mut types = ArrayVec::new();
        insert_card(c0, &mut types);
        insert_card(c1, &mut types);
        insert_card(c2, &mut types);
        insert_card(c3, &mut types);
        insert_card(c4, &mut types);
        let hand_type = match &types[..] {
            [] | [_] => HandType::FiveKind,
            // see haskell version for reasoning
            &[(c0, _), (c1, _)] => match [c0, c1] {
                [3, 2] | [2, 2 | 3] => HandType::FullHouse,
                _ => HandType::FourKind,
            },
            // see haskell version for reasoning
            &[(c0, _), (c1, _), (c2, _)] => {
                if c0 + c1 + c2 == 5 && [c0, c1, c2].contains(&2) {
                    HandType::TwoPair
                } else {
                    HandType::ThreeKind
                }
            }
            // see haskell version for reasoning
            [_, _, _, _] => HandType::OnePair,
            _ => HandType::HighCard,
        };
        Hand2 {
            cards: original,
            hand_type,
            wager,
        }
    }
}

fn insert_card(card: CardType2, types: &mut ArrayVec<(u8, CardType2), 5>) {
    match card {
        CardType2::CJ => {}
        other => {
            for i in 0..types.len() {
                if types[i].1 == other {
                    types[i].0 += 1;
                    return;
                }
            }
            types.push((1, other));
        }
    }
}

pub fn part2(input: &str) -> usize {
    let input = input.as_bytes();
    let mut i = 0;
    let mut hands = Vec::new();
    while i < input.len() {
        let hand = Hand2::read(input, &mut i);
        hands.push(hand);
        i += 1;
    }
    hands.sort_unstable();
    hands.into_iter().fold([0, 1], |[acc, rank], hand| {
        [acc + rank * hand.wager, rank + 1]
    })[0]
}

#[cfg(test)]
#[allow(unused_imports)]
mod test {
    use std::fs::read_to_string;

    #[test]
    fn part1test1() {
        let input = read_to_string("input/d7test1.txt").unwrap();
        assert_eq!(super::part1(&input), 6440);
    }

    #[test]
    fn part1test2() {
        let input = read_to_string("input/d7test2.txt").unwrap();
        assert_eq!(super::part1(&input), (1..=12).map(|n| n * n).sum());
    }

    #[test]
    fn part2test() {
        let input = read_to_string("input/d7test1.txt").unwrap();
        assert_eq!(super::part2(&input), 5905);
    }
}
