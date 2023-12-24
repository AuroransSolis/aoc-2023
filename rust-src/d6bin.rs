mod d6lib;

fn main() {
    let input = std::fs::read_to_string("input/d6.txt").unwrap();
    println!("p1: {}", d6lib::part1(&input));
    println!("p2: {}", d6lib::part2(&input));
}
