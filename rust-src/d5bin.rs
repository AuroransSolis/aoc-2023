mod d5lib;

fn main() {
    let input = std::fs::read_to_string("input/d5.txt").unwrap();
    println!("p1: {}", d5lib::part1(&input));
    println!("p2: {}", d5lib::part2(&input));
}
