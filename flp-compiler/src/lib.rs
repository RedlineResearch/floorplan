
use std::process::*;
use std::io::Write;

#[derive(Clone, Debug)]
pub struct Build {
    src: Option<String>,
    dest: Option<String>,
}

impl Build {

    pub fn new() -> Build {
        Build {
            src: None,
            dest: None,
        }
    }

    pub fn src(&mut self, s: &str) -> &mut Build {
        self.src = Some(s.to_string());
        self
    }

    pub fn dest(&mut self, d: &str) -> &mut Build {
        self.dest = Some(d.to_string());
        self
    }

    pub fn compile(&mut self) {
        if let Err(e) = self.try_compile() {
            fail(&e);
        }
    }

    pub fn try_compile(&mut self) -> Result<(), String> {
        let src = self.src.clone().unwrap();
        let dest = self.dest.clone().unwrap();
        Command::new("flp")
                .args(&[src, dest])
                .output()
                .expect("Floorplan failed to run.");
        Ok(())
    }

}

fn fail(s: &str) -> ! {
    let _ = writeln!(std::io::stderr(), "\nError: {}\n\n", s);
    std::process::exit(1);
}

