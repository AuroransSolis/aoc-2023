mod d3lib;

fn main() {
    let input = std::fs::read_to_string("input/d3.txt").unwrap();
    println!("p1: {}", d3lib::part1(&input));
    println!("p2: {}", d3lib::part2(&input));
}
