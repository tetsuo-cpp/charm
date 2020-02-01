use std::env;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    match args.get(1) {
        Some(file_name) => {
            let config = charm::Config::new(file_name);
            match charm::run(config) {
                Ok(()) => Ok(()),
                Err(e) => Err(format!("{}", e)),
            }
        }
        None => Err(String::from("Usage: charm <file_name>")),
    }
}
