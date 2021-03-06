module api

pub struct Vpkg {
pub mut:
    command string
    options map[string]string
    unknown []string
    dir string
    manifest_file_path string
    manifest PkgManifest
    is_global bool
}

struct Registry {
    packages []Package
}

struct Package {
    name string
    url string
    method string
}

struct InstalledPackage {
mut:
    name string
    path string   [skip]
    version string
}

struct PkgManifest {
    name string
    author []string
    version string
    sources []string
    repo string
    dependencies []string
}

struct VpmPackage {
    id int
    name string
    url string
    nr_downloads int
}

struct Lockfile {
mut:
    version string
    packages []InstalledPackage
}