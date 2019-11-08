extern crate backtrace;

macro_rules! backtraceHere {
    ( $val:expr ) => {
        {   use std::fmt::Write;
            let bt = &backtrace::Backtrace::new();
            let frames_count = bt.frames().len();
            let frames = &bt.frames();
            for i in 2..frames_count {
                let symb = &frames[i].symbols()[0];
                if let Some(name) = symb.name() {
                    if let Some(ln) = symb.lineno() {
						let mut output = String::new();
						let _ = write!(&mut output, "{}", name);
                        if output.starts_with("std::rt") { break; }
                        eprintln!("    {}:{}] = 0x{:X}", name, ln, $val);
                    }
                }
            }
        }
    };
}


