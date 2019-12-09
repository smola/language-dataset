defineReplace(copyAndroidSources) {
    commandAlias = $$1
    targetDirectory = $$2
    sourceFiles = $$3

    # Ensure we were provided a valid alias for this command
    isEmpty(commandAlias) {
        error("copyAndroidSources requires a valid command alias as its first argument.")
    }

    # Ensure we were provided a valid target directory
    isEmpty(targetDirectory) {
        error("copyAndroidSources requires a valid target directory path as its second argument.")
    }

    # Ensure we were provide a valid set of source files
    isEmpty(sourceFiles) {
        error("copyAndroidSources requires a valid set of files as its third argument.")
    }

    # Create the target directory
    $${commandAlias}.commands += $(MKDIR) $$shell_path($$ANDROID_PACKAGE_SOURCE_DIR/$$targetDirectory)

    # Process each file that we were provided
    for(fileName, sourceFiles) {
        # Copy the file from the source directory to the target directory
        $${commandAlias}.commands += $$escape_expand(\n\t)$(COPY_FILE) $$fileName $$shell_path($$ANDROID_PACKAGE_SOURCE_DIR/$$targetDirectory/$$basename(fileName))

        # To prevent our library files ending up in other repositories we add the copied file to to a .gitignore in the target directory
        gitIgnoreFile = $$shell_path($$ANDROID_PACKAGE_SOURCE_DIR/$$targetDirectory/.gitignore)
        gitIgnoreLine = $$basename(fileName)

        exists($$gitIgnoreFile) {
            # There is an existing .gitignore file, so only add the file's entry if it does not already exist
            fileContents = $$cat($$gitIgnoreFile, blob)
            searchResults = $$find(fileContents, $$gitIgnoreLine)
            isEmpty(searchResults) {
                write_file($$gitIgnoreFile, gitIgnoreLine, append)
            }
        } else {
            # There is no existing .gitignore file, so create one and add the file's entry
            write_file($$gitIgnoreFile, gitIgnoreLine)
        }
    }

    # We need to make our new command globally available so that qmake can add it to the generated Makefile
    export($${commandAlias}.commands)

    # To ensure our command is run before compilation occurs we add our command to the dependency tree of 'first' (creating the tree if required)
    isEmpty(first.depends) {
        first.depends = $(first) $${commandAlias}
        export(first.depends)
        return(first $${commandAlias})
    } else {
        first.depends += $${commandAlias}
        export(first.depends)
        return($${commandAlias})
    }
}
