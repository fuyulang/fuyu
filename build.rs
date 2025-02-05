fn main() {
    lalrpop::process_root().unwrap();
    println!("cargo:rerun-if-changed=src/compiler/grammar.lalrpop");
}
