use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::backend::BackendOptions;
use crate::frontend::{Semantics, TypedProgram};

const CACHE_SCHEMA_VERSION: u32 = 2;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BuildCacheEntry {
	pub fingerprint: String,
	pub object_path: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BuildCacheIndex {
	pub schema_version: u32,
	pub backend: String,
	pub target_triple: String,
	pub debug_info: bool,
	pub optimization_level: String,
	pub modules: HashMap<String, BuildCacheEntry>,
}

impl BuildCacheIndex {
	fn empty(
		backend: &str,
		target_triple: &str,
		debug_info: bool,
		optimization_level: &str,
	) -> Self {
		Self {
			schema_version: CACHE_SCHEMA_VERSION,
			backend: backend.to_string(),
			target_triple: target_triple.to_string(),
			debug_info,
			optimization_level: optimization_level.to_string(),
			modules: HashMap::new(),
		}
	}
}

pub struct BuildCache {
	pub cache_dir: PathBuf,
	pub index_path: PathBuf,
	pub index: BuildCacheIndex,
}

#[derive(Clone, Debug)]
pub struct ModuleBuildPlan {
	pub module_id: String,
	pub fingerprint: String,
	pub object_path: PathBuf,
	pub needs_compile: bool,
}

impl BuildCache {
	pub fn load_or_create(options: &BackendOptions, backend_name: &str) -> Self {
		let cache_dir = options.emit_dir.join(".heron-cache");
		let index_path = cache_dir.join("index.json");
		let target = options
			.target_triple
			.clone()
			.unwrap_or_else(|| "default".to_string());
		let mut index = BuildCacheIndex::empty(
			backend_name,
			&target,
			options.debug_info,
			options.optimization_level.as_str(),
		);

		if let Ok(raw) = fs::read_to_string(&index_path)
			&& let Ok(parsed) = serde_json::from_str::<BuildCacheIndex>(&raw)
			&& parsed.schema_version == CACHE_SCHEMA_VERSION
			&& parsed.backend == backend_name
			&& parsed.target_triple == target
			&& parsed.debug_info == options.debug_info
			&& parsed.optimization_level == options.optimization_level.as_str()
		{
			index = parsed;
		}

		Self {
			cache_dir,
			index_path,
			index,
		}
	}

	pub fn save(&self) -> Result<(), String> {
		if let Some(parent) = self.index_path.parent()
			&& let Err(err) = fs::create_dir_all(parent)
		{
			return Err(format!("failed to create cache dir: {err}"));
		}
		let raw = serde_json::to_string_pretty(&self.index)
			.map_err(|err| format!("failed to serialize cache index: {err}"))?;
		fs::write(&self.index_path, raw)
			.map_err(|err| format!("failed to write cache index: {err}"))
	}

	pub fn compute_build_plan(
		&self,
		program: &TypedProgram,
		semantics: &Semantics,
		backend_name: &str,
		options: &BackendOptions,
	) -> Vec<ModuleBuildPlan> {
		let mut module_ids: Vec<String> = program.modules.keys().cloned().collect();
		module_ids.sort();

		let mut fingerprints = HashMap::new();
		for module_id in &module_ids {
			let fingerprint =
				module_fingerprint(module_id, semantics, backend_name, options);
			fingerprints.insert(module_id.clone(), fingerprint);
		}

		let dependents = build_dependents(program);
		let mut dirty: HashSet<String> = HashSet::new();

		for module_id in &module_ids {
			let fingerprint = fingerprints.get(module_id).cloned().unwrap_or_default();
			let previous = self.index.modules.get(module_id);
			let object_path = module_object_path(&options.emit_dir, module_id);
			let path_exists = object_path.is_file();
			let changed = previous
				.map(|entry| {
					entry.fingerprint != fingerprint
						|| entry.object_path
							!= object_path.to_string_lossy().as_ref()
				})
				.unwrap_or(true);
			if changed || !path_exists {
				dirty.insert(module_id.clone());
			}
		}

		let mut queue: VecDeque<String> = dirty.iter().cloned().collect();
		while let Some(module_id) = queue.pop_front() {
			if let Some(users) = dependents.get(&module_id) {
				for dependent in users {
					if dirty.insert(dependent.clone()) {
						queue.push_back(dependent.clone());
					}
				}
			}
		}

		module_ids
			.into_iter()
			.map(|module_id| {
				let object_path = module_object_path(&options.emit_dir, &module_id);
				ModuleBuildPlan {
					module_id: module_id.clone(),
					fingerprint: fingerprints
						.get(&module_id)
						.cloned()
						.unwrap_or_default(),
					object_path,
					needs_compile: dirty.contains(&module_id),
				}
			})
			.collect()
	}

	pub fn prune_modules(&mut self, program: &TypedProgram) {
		let live: HashSet<String> = program.modules.keys().cloned().collect();
		self.index
			.modules
			.retain(|module_id, _| live.contains(module_id));
	}

	pub fn update_module(&mut self, module_id: &str, fingerprint: String, object_path: &Path) {
		self.index.modules.insert(
			module_id.to_string(),
			BuildCacheEntry {
				fingerprint,
				object_path: object_path.to_string_lossy().to_string(),
			},
		);
	}
}

fn build_dependents(program: &TypedProgram) -> HashMap<String, Vec<String>> {
	let mut map: HashMap<String, Vec<String>> = HashMap::new();
	for (module_id, module) in &program.modules {
		for imported in module.imports.alias_to_module.values() {
			map.entry(imported.clone())
				.or_default()
				.push(module_id.clone());
		}
	}
	map
}

fn module_fingerprint(
	module_id: &str,
	semantics: &Semantics,
	backend_name: &str,
	options: &BackendOptions,
) -> String {
	let mut data = String::new();
	data.push_str("schema=1\n");
	data.push_str("module=");
	data.push_str(module_id);
	data.push('\n');
	data.push_str("backend=");
	data.push_str(backend_name);
	data.push('\n');
	data.push_str("debug=");
	data.push_str(if options.debug_info { "1" } else { "0" });
	data.push('\n');
	data.push_str("target=");
	data.push_str(options.target_triple.as_deref().unwrap_or("default"));
	data.push('\n');
	data.push_str("opt=");
	data.push_str(options.optimization_level.as_str());
	data.push('\n');

	if let Some(module) = semantics.modules.get(module_id) {
		data.push_str("file=");
		data.push_str(&module.file_path);
		data.push('\n');
		if let Ok(source) = fs::read_to_string(&module.file_path) {
			data.push_str("source=");
			data.push_str(&source);
			data.push('\n');
		}
		let mut imports: Vec<String> =
			module.imports.alias_to_module.values().cloned().collect();
		imports.sort();
		for import in imports {
			data.push_str("import=");
			data.push_str(&import);
			data.push('\n');
		}
	}

	format!("{:016x}", fnv1a_64(data.as_bytes()))
}

fn module_object_path(emit_dir: &Path, module_id: &str) -> PathBuf {
	let file = format!("{}.o", module_id.replace('.', "__"));
	emit_dir.join("obj").join(file)
}

fn fnv1a_64(bytes: &[u8]) -> u64 {
	let mut hash = 0xcbf29ce484222325u64;
	for byte in bytes {
		hash ^= u64::from(*byte);
		hash = hash.wrapping_mul(0x100000001b3);
	}
	hash
}
