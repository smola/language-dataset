using build

class Build : BuildPod {
	
	// to compile (from the cmd line) you'll need to copy the .jar to C:\Apps\fantom-1.0.63\lib\java\ext
	// see http://fantom.org/sidewalk/topic/1799
	// "Also we don't support using Java FFI on yourself - so you should build jade.jar as a pod by 
	// itself and then have your pod depend on it. Or just stick jade.jar in 'lib/java/ext' "
	
	new make() {
		podName = "afMicromod"
		summary = "A music player for MOD, S3M, and XM files"
		version = Version("1.0.5")

		meta = [	
			"proj.name"			: "Micromod",
			"org.name"			: "MuMart",
			"org.uri"			: "https://sites.google.com/site/mumart/home/micromodibxm",
			"repo.tags"			: "misc",
			"repo.public"		: "false"
		]

		depends = [
			"sys          1.0", 
			"concurrent   1.0",

			"afConcurrent 1.0.8 - 1.0"
		]

		srcDirs = [`fan/`]
		resDirs = [`doc/`, `lib/java/ibxm-a61.jar`]
	}
}