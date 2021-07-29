//! The `linker` module provides structs to generate linker commands for
//! a number of popular linkers

use std::io;
use std::process::Command;

use semver::Version;

pub use crate::OutFormat;

/// The `Linker` trait provides a generic interface for any linker
pub trait Linker: Default + Sized {
    /// Add an object file by path to the linker's input
    fn add_object_file(&mut self, obj: impl AsRef<std::path::Path>);

    /// Add a library to the linker's input files
    fn add_library(&mut self, lib: impl AsRef<std::path::Path>) {
        self.add_object_file(lib)
    }

    /// Set the output format for this linker
    fn set_format(&mut self, format: OutFormat);

    /// Set the entry function of the linked output
    fn set_entry(&mut self, entry: Option<&str>);

    /// Set the output file name
    fn set_output_file(&mut self, output: impl AsRef<std::path::Path>);

    ///Link using the given input files by spawning a process
    fn link(self) -> io::Result<()>;
}

/// An interface to the windows native LINK tool from visual studio build tools
pub struct WinLink {
    /// The entry point of the finished exe, if any
    entry: Option<String>,

    /// The output file name
    output: String,

    /// The output format of the linker
    format: OutFormat,

    /// Paths to input linker files
    input_files: Vec<String>,

    /// The path to the LINK.EXE file
    linker_path: String,
}

impl Default for WinLink {
    fn default() -> Self {
        use std::fs;
        //Get the location of LINK.exe
        let linker_path = fs::read_dir(
            r"C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Tools\MSVC",
        )
        .unwrap_or_else(|e| panic!("Failed to get location of Microsoft build tools: {}", e))
        .max_by(|this, next| {
            let this = this
                .as_ref()
                .unwrap_or_else(|e| panic!("Failed to get location of LINK.EXE: {}", e));
            let next = next
                .as_ref()
                .unwrap_or_else(|e| panic!("Failed to get location of LINK.EXE: {}", e));
            let this_ver: Result<Version, _> = this.file_name().to_str().unwrap().parse();
            let next_ver: Result<Version, _> = next.file_name().to_str().unwrap().parse();
            match (this_ver, next_ver) {
                (Ok(this_ver), Ok(next_ver)) => this_ver.cmp(&next_ver),
                (Err(_), Ok(_)) => std::cmp::Ordering::Less,
                (Ok(_), Err(_)) => std::cmp::Ordering::Greater,
                (_, _) => std::cmp::Ordering::Equal,
            }
        })
        .unwrap_or_else(|| panic!("Failed to get highest version of LINK.exe"))
        .unwrap()
        .path()
        .join(r"bin\Hostx64\x64\link.exe")
        .to_str()
        .unwrap()
        .to_owned();
        Self {
            entry: None,
            output: "a.out".to_owned(),
            format: OutFormat::Exe,
            input_files: vec![],
            linker_path,
        }
    }
}

impl Linker for WinLink {
    fn add_object_file(&mut self, obj: impl AsRef<std::path::Path>) {
        self.input_files
            .push(obj.as_ref().to_str().unwrap().to_owned())
    }

    fn set_format(&mut self, format: OutFormat) {
        self.format = format
    }

    fn set_entry(&mut self, entry: Option<&str>) {
        self.entry = entry.map(|s| s.to_owned())
    }

    fn set_output_file(&mut self, output: impl AsRef<std::path::Path>) {
        self.output = output.as_ref().to_str().unwrap().to_owned()
    }

    fn link(self) -> io::Result<()> {
        use std::process::Stdio;
        let mut args = self.input_files;
        args.push("/NOLOGO".to_owned());

        if let Some(entry) = self.entry {
            args.push(format!("/ENTRY:{}", entry));
        }

        args.push(format!(
            "/OUT:{}",
            self.output
                + match self.format {
                    OutFormat::Exe => ".exe",
                    OutFormat::Lib => ".lib",
                    OutFormat::Obj => ".obj",
                    _ => unreachable!(),
                }
        ));

        let out = Command::new(self.linker_path)
            .args(args)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()?;

        println!(
            "LINK.EXE stderr: {}",
            std::str::from_utf8(out.stderr.as_slice()).unwrap()
        );
        println!(
            "LINK.EXE stdout: {}",
            std::str::from_utf8(out.stdout.as_slice()).unwrap()
        );

        Ok(())
    }
}

/// An interface to the most commonly used linux linker, ld
pub struct LdLink {
    /// The entry point of the finished binary, if any
    entry: Option<String>,

    /// The output file name
    output: String,

    /// The output format of the linker
    format: OutFormat,

    /// Paths to input linker files
    input_files: Vec<String>,
}

impl Default for LdLink {
    fn default() -> Self {
        Self {
            entry: Some("main".to_owned()),
            output: "a.out".to_owned(),
            format: OutFormat::Exe,
            input_files: vec![],
        }
    }
}

impl Linker for LdLink {
    fn add_object_file(&mut self, obj: impl AsRef<std::path::Path>) {
        self.input_files
            .push(obj.as_ref().to_str().unwrap().to_owned())
    }

    fn set_entry(&mut self, entry: Option<&str>) {
        self.entry = entry.map(|s| s.to_owned())
    }

    fn set_output_file(&mut self, output: impl AsRef<std::path::Path>) {
        self.output = output.as_ref().to_str().unwrap().to_string()
    }

    fn set_format(&mut self, format: OutFormat) {
        self.format = format
    }

    fn link(self) -> io::Result<()> {
        use std::process::Stdio;
        let mut args = self.input_files;

        if let Some(entry) = self.entry {
            args.push(format!("-e{}", entry));
        }

        args.push(format!(
            "-o{}",
            self.output
                + match self.format {
                    OutFormat::Exe => "",
                    OutFormat::Lib => ".lib",
                    OutFormat::Obj => ".o",
                    _ => unreachable!(),
                }
        ));

        args.push("-dynamic-linker".to_owned()); //Segfault instantly unless this option is passed

        let out = Command::new("ld")
            .args(args)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()?;

        println!(
            "ld stderr: {}",
            std::str::from_utf8(out.stderr.as_slice()).unwrap()
        );
        println!(
            "ld stdout: {}",
            std::str::from_utf8(out.stdout.as_slice()).unwrap()
        );

        Ok(())
    }
}
