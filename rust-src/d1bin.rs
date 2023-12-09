mod d1lib;

const INPUT: &str = "input/d1.txt";

fn main() {
    let input = std::fs::read_to_string(INPUT).unwrap();
    println!("p1: {}", d1lib::part1(&input));
    println!("p2: {}", d1lib::part2(&input));
}
