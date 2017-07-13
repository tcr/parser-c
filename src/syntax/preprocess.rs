// Original file: "Preprocess.hs"
// File auto-generated using Corollary.

use std::fs;
use std::error::Error;
use std::path::{Path, PathBuf};
use tempdir;

use data::input_stream::InputStream;

pub type PPResult<T> = Result<T, Box<Error>>;

// 'Preprocessor' encapsulates the abstract interface for invoking C preprocessors
pub trait Preprocessor {
    // parse the given command line arguments, and return a pair of parsed and ignored arguments
    fn parseCPPArgs(&self, Vec<String>) -> Result<(CppArgs, Vec<String>), String>;
    // run the preprocessor
    fn runCPP(&self, &CppArgs) -> PPResult<()>;
}

pub const PREPROCESSED_EXT: &str = "i";

pub fn isPreprocessed(x: &Path) -> bool {
    x.ends_with(&format!(".{}", PREPROCESSED_EXT))
}

#[derive(Clone)]
pub enum CppOption {
    IncludeDir(String),
    Define(String, String),
    Undefine(String),
    IncludeFile(String),
}
pub use self::CppOption::*;

#[derive(Clone)]
pub struct CppArgs {
    pub cppOptions: Vec<CppOption>,
    pub extraOptions: Vec<String>,
    pub cppTmpDir: Option<PathBuf>,
    pub inputFile: PathBuf,
    pub outputFile: Option<PathBuf>,
}

impl CppArgs {
    pub fn for_file(input_file: PathBuf) -> CppArgs {
        CppArgs {
            cppOptions: vec![],
            extraOptions: vec![],
            cppTmpDir: None,
            inputFile: input_file,
            outputFile: None,
        }
    }

    pub fn raw(opts: Vec<String>, input_file: PathBuf) -> CppArgs {
        CppArgs {
            inputFile: input_file,
            cppOptions: vec![],
            extraOptions: opts,
            outputFile: None,
            cppTmpDir: None,
        }
    }

    pub fn addOption(&mut self, opt: CppOption) {
        self.cppOptions.push(opt);
    }

    pub fn addExtraOption(&mut self, extra: String) {
        self.extraOptions.push(extra);
    }
}

pub fn runPreprocessor<P: Preprocessor>(cpp: P, cpp_args: CppArgs) -> PPResult<InputStream> {
    // There are three modes we handle the output directory and file:
    // - outputFile is given: use as filename, don't remove it
    // - cppTmpDir is given: create temp file there, remove it but not the dir
    // - nothing is given: create temp dir, remove it

    let inner = if cpp_args.outputFile.is_some() {
        preprocess_with_out_file
    } else if cpp_args.cppTmpDir.is_some() {
        preprocess_with_given_temp_dir
    } else {
        preprocess_with_new_temp_dir
    };

    inner(cpp, cpp_args)
}

fn preprocess_with_out_file<P: Preprocessor>(cpp: P, cpp_args: CppArgs) -> PPResult<InputStream> {
    cpp.runCPP(&cpp_args).map(|_| InputStream::from_file(cpp_args.outputFile.as_ref().unwrap()))
}

fn preprocess_with_given_temp_dir<P: Preprocessor>(cpp: P, mut cpp_args: CppArgs) -> PPResult<InputStream> {
    let mut inputFile = cpp_args.inputFile.clone();
    inputFile.set_extension(PREPROCESSED_EXT);
    let mut outputFile = cpp_args.cppTmpDir.clone().unwrap();
    outputFile.push(inputFile.file_name().unwrap());
    cpp_args.outputFile = Some(outputFile.clone());
    let res = preprocess_with_out_file(cpp, cpp_args);
    fs::remove_file(&outputFile).unwrap();
    res
}

fn preprocess_with_new_temp_dir<P: Preprocessor>(cpp: P, mut cpp_args: CppArgs) -> PPResult<InputStream> {
    let tmpdir = tempdir::TempDir::new("parse-c").unwrap(); // will self-destruct on drop
    cpp_args.cppTmpDir = Some(tmpdir.path().to_path_buf());
    preprocess_with_given_temp_dir(cpp, cpp_args)
}
