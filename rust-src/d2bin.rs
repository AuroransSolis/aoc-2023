mod d2lib;

fn main() {
    let input = std::fs::read_to_string("input/d2.txt").unwrap();
    println!("p1: {}", d2lib::part1(&input));
    println!("p2: {}", d2lib::part2(&input));
}
