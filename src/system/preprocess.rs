// Original file: "Preprocess.hs"
// File auto-generated using Corollary.

use std::fs;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::ffi::OsStr;
use tempdir;

use data::input_stream::InputStream;

pub type PPResult<T> = Result<T, Box<Error>>;

// 'Preprocessor' encapsulates the abstract interface for invoking C preprocessors
pub trait Preprocessor {
    // parse the given command line arguments, and return a pair of parsed and ignored arguments
    fn parse_cpp_args(&self, Vec<String>) -> Result<(CppArgs, Vec<String>), String>;
    // run the preprocessor
    fn run_cpp(&self, &CppArgs) -> PPResult<()>;
}

pub const PREPROCESSED_EXT: &str = "i";

pub fn is_preprocessed(x: &Path) -> bool {
    x.extension() == Some(OsStr::new(PREPROCESSED_EXT))
}

#[derive(Clone)]
pub enum CppOption {
    IncludeDir(String),
    Define(String, String),
    Undefine(String),
    IncludeFile(String),
}

#[derive(Clone)]
pub struct CppArgs {
    pub cpp_options: Vec<CppOption>,
    pub extra_options: Vec<String>,
    pub cpp_tmp_dir: Option<PathBuf>,
    pub input_file: PathBuf,
    pub output_file: Option<PathBuf>,
}

impl CppArgs {
    pub fn for_file(input_file: PathBuf) -> CppArgs {
        CppArgs {
            cpp_options: vec![],
            extra_options: vec![],
            cpp_tmp_dir: None,
            input_file: input_file,
            output_file: None,
        }
    }

    pub fn raw(opts: Vec<String>, input_file: PathBuf) -> CppArgs {
        CppArgs {
            input_file: input_file,
            cpp_options: vec![],
            extra_options: opts,
            output_file: None,
            cpp_tmp_dir: None,
        }
    }

    pub fn add_option(&mut self, opt: CppOption) {
        self.cpp_options.push(opt);
    }

    pub fn add_extra_option(&mut self, extra: String) {
        self.extra_options.push(extra);
    }
}

pub fn run_preprocessor<P: Preprocessor>(cpp: P, cpp_args: CppArgs) -> PPResult<InputStream> {
    // There are three modes we handle the output directory and file:
    // - outputFile is given: use as filename, don't remove it
    // - cppTmpDir is given: create temp file there, remove it but not the dir
    // - nothing is given: create temp dir, remove it

    let inner = if cpp_args.output_file.is_some() {
        preprocess_with_out_file
    } else if cpp_args.cpp_tmp_dir.is_some() {
        preprocess_with_given_temp_dir
    } else {
        preprocess_with_new_temp_dir
    };

    inner(cpp, cpp_args)
}

fn preprocess_with_out_file<P: Preprocessor>(cpp: P, cpp_args: CppArgs) -> PPResult<InputStream> {
    let output = cpp_args.output_file.as_ref().expect("output_file should be set");
    cpp.run_cpp(&cpp_args).and_then(|_| InputStream::from_file(output).map_err(Into::into))
}

fn preprocess_with_given_temp_dir<P: Preprocessor>(cpp: P, mut cpp_args: CppArgs) -> PPResult<InputStream> {
    let mut input_file = cpp_args.input_file.clone();
    input_file.set_extension(PREPROCESSED_EXT);
    let mut output_file = cpp_args.cpp_tmp_dir.clone().expect("cpp_tmp_dir should be set");
    output_file.push(input_file.file_name().unwrap());
    cpp_args.output_file = Some(output_file.clone());
    let res = preprocess_with_out_file(cpp, cpp_args);
    let _ = fs::remove_file(&output_file);
    res
}

fn preprocess_with_new_temp_dir<P: Preprocessor>(cpp: P, mut cpp_args: CppArgs) -> PPResult<InputStream> {
    let tmpdir = tempdir::TempDir::new("parse-c")?; // will self-destruct on drop
    cpp_args.cpp_tmp_dir = Some(tmpdir.path().to_path_buf());
    preprocess_with_given_temp_dir(cpp, cpp_args)
}
