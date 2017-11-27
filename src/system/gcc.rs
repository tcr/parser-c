// Original file: "GCC.hs"
// File auto-generated using Corollary.

//use corollary_support::*;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use system::preprocess::{Preprocessor, CppArgs, PPResult};
use system::preprocess::CppOption::*;

pub struct GCC {
    gcc_path: PathBuf,
}

impl GCC {
    pub fn new<P: Into<PathBuf>>(path: P) -> GCC {
        GCC { gcc_path: path.into() }
    }
}

impl Preprocessor for GCC {
    fn parse_cpp_args(&self, args: Vec<String>) -> Result<(CppArgs, Vec<String>), String> {
        parse_gcc_cpp_args(args)
    }

    fn run_cpp(&self, cpp_args: &CppArgs) -> PPResult<()> {
        // copy the input to the outputfile, because in case the input is preprocessed,
        // gcc -E will do nothing.
        let out = cpp_args.output_file.as_ref().unwrap();
        fs::copy(&cpp_args.input_file, out)?;
        let meta = fs::metadata(out)?;
        meta.permissions().set_readonly(false);
        fs::set_permissions(out, meta.permissions())?;
        // now run GCC
        let mut cmd = Command::new(&self.gcc_path);
        build_cpp_cmd(&mut cmd, cpp_args);
        if cmd.status()?.success() {
            Ok(())
        } else {
            Err("GCC exited with error".into())
        }
    }
}

fn parse_gcc_cpp_args(args: Vec<String>) -> Result<(CppArgs, Vec<String>), String> {
    let mut input_file = None;
    let mut output_file = None;
    let mut cppopt = vec![];
    let mut extra = vec![];
    let mut other = vec![];

    let mut args = args.into_iter();
    while let Some(arg) = args.next() {
        // -E is implicitly enabled -> ignore it
        if arg == "-E" {
            continue;
        }

        // some extra flags we don't want to pass on to CPP
        if arg == "-MF" || arg == "-MT" || arg == "-MQ" {
            if let Some(marg) = args.next() {
                other.push(arg);
                other.push(marg);
            } else {
                return Err(format!("{} option needs an ergument", arg));
            }
            continue;
        }

        if arg == "-c" || arg == "-S" || arg.starts_with("-M") {
            other.push(arg);
            continue;
        }

        // CPP options
        if arg.starts_with("-I") {
            cppopt.push(IncludeDir(arg[2..].into()));
            continue;
        } else if arg.starts_with("-U") {
            cppopt.push(Undefine(arg[2..].into()));
            continue;
        } else if arg.starts_with("-D") {
            let mut iter = arg[2..].splitn(2, '=');
            cppopt.push(Define(iter.next().unwrap().into(),
                               iter.next().unwrap_or("").into()));
            continue;
        }

        // output file option
        if arg == "-o" {
            if output_file.is_some() {
                return Err("two output files given".into());
            }
            if let Some(oarg) = args.next() {
                output_file = Some(Path::new(&oarg).into());
            } else {
                return Err("-o option needs an argument".into());
            }
            continue;
        }

        // input file names
        if arg.ends_with(".c") || arg.ends_with(".hc") || arg.ends_with(".h") {
            if input_file.is_some() {
                return Err("two output files given".into());
            }
            input_file = Some(Path::new(&arg).into());
            continue;
        }

        // others we put into the CPP args
        extra.push(arg);
    }

    match input_file {
        None => Err("No .c / .hc / .h source file given".into()),
        Some(ifile) => Ok((CppArgs {
            cpp_options: cppopt,
            extra_options: extra,
            input_file: ifile,
            cpp_tmp_dir: None,
            output_file: output_file,
        }, other))
    }
}

fn build_cpp_cmd(cmd: &mut Command, cpp_args: &CppArgs) {
    for opt in &cpp_args.cpp_options {
        match *opt {
            IncludeDir(ref incl) => { cmd.arg("-I"); cmd.arg(incl); }
            Define(ref key, ref value) => { cmd.arg(
                &format!("-D{}{}{}", key, if value.is_empty() { "" } else { "=" }, value));
            }
            Undefine(ref key) => { cmd.arg(&format!("-U{}", key)); }
            IncludeFile(ref incfile) => { cmd.arg("-include"); cmd.arg(incfile); }
        }
    }
    if let Some(ofile) = cpp_args.output_file.as_ref() {
        cmd.arg("-o");
        cmd.arg(ofile);
    }
    cmd.arg("-E");
    cmd.arg(&cpp_args.input_file);
    cmd.args(&cpp_args.extra_options);
}
