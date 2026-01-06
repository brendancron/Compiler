use std::collections::HashMap;
use std::path::PathBuf;

pub trait EmbedResolver {
    fn read(&self, path: &str) -> String;
}

pub struct DefaultResolver {
    pub base_dir: PathBuf,
}

impl EmbedResolver for DefaultResolver {
    fn read(&self, path: &str) -> String {
        let full = self.base_dir.join(path);
        std::fs::read_to_string(&full).unwrap_or_else(|_| panic!("embed failed: {full:?}"))
    }
}

pub struct TestResolver {
    pub files: HashMap<String, String>,
}

impl EmbedResolver for TestResolver {
    fn read(&self, path: &str) -> String {
        self.files[path].clone()
    }
}
