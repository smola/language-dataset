import ceylon.ant { Ant, AntProject, currentAntProject, AntDefinition, AntAttributeDefinition, ant }
import ceylon.test { assertEquals, assertTrue, assertFalse, test }
import ceylon.file { parsePath, Path, File, Directory, Nil }
import ceylon.language.meta.model { Interface }

File|Directory|Nil retrieveActualResource(String relativeResourceName) {
    AntProject antProject = currentAntProject();
    String effectiveBaseDirectory = antProject.effectiveBaseDirectory();
    Path exampleFilePath = parsePath(effectiveBaseDirectory + "/" + relativeResourceName);
    File|Directory|Nil actualResource = exampleFilePath.resource.linkedResource;
    return actualResource;
}

void verifyResource(String relativeResourceName, Interface<File|Directory|Nil> expectedResourceType, String failMessage) {
    File|Directory|Nil actualResource = retrieveActualResource(relativeResourceName);
    if(expectedResourceType.typeOf(actualResource)) {
        print("``relativeResourceName`` is ``expectedResourceType``");
    } else {
        throw Exception("``failMessage``: ``relativeResourceName`` is not ``expectedResourceType``");
    }
}

AntDefinition? filterAntDefinition({AntDefinition*} antDefinitions, String antName) {
    {AntDefinition*} filteredAntDefinitions = antDefinitions.filter { function selecting(AntDefinition antDefintion) => (antDefintion.antName == antName); };
    switch (filteredAntDefinitions.size)
    case (0) {
        return null;
    }
    case (1) {
        return filteredAntDefinitions.first;
    }
    else {
        throw Exception("More than one Ant type/task found for ``antName``");
    }
}

test void testEcho() {
    ant("echo", { "message" -> "G'day mate! " }, {}, "Cheerio!" );
}

test void testFileTasks() {
    String buildDirectory = "target/build-test-file-tasks-directory";
    Ant fileset = Ant("fileset", { "dir" -> "``buildDirectory``" }, [
        Ant("include", { "name" -> "example.txt" } )
    ] );
    ant("mkdir", { "dir" -> "``buildDirectory``" } );
    verifyResource("``buildDirectory``", `Directory`, "Cannot create directory");
    ant("echo", { "message" -> "File created.", "file" -> "``buildDirectory``/example.txt" } );
    verifyResource("``buildDirectory``/example.txt", `File`, "Cannot create file");
    ant("mkdir", { "dir" -> "``buildDirectory``/sub-directory" } );
    verifyResource("``buildDirectory``/sub-directory", `Directory`, "Cannot create directory");
    ant("copy", { "todir" -> "``buildDirectory``/sub-directory" }, [
        fileset
    ] );
    verifyResource("``buildDirectory``/sub-directory/example.txt", `File`, "Cannot copy to file");
    ant("delete", { }, [
        fileset
    ] );
    verifyResource("``buildDirectory``/example.txt", `Nil`, "Cannot delete file");
    ant("delete", { "dir" -> "``buildDirectory``", "verbose" -> "true" } );
    verifyResource("``buildDirectory``", `Nil`, "Cannot delete directory");
}

test void testAntDefinitions() {
    AntProject antProject = currentAntProject();
    List<AntDefinition> allTopLevelAntDefinitions = antProject.allTopLevelAntDefinitions();
    assertTrue(allTopLevelAntDefinitions.size > 0);
    // now print out ant definitions
    for(antDefinition in allTopLevelAntDefinitions) {
        print("``antDefinition.antName``");
    }
}

test void testAntDefinition() {
    AntProject antProject = currentAntProject();
    AntDefinition? copyAntDefinition = filterAntDefinition(antProject.allTopLevelAntDefinitions(), "copy");
    assert(exists copyAntDefinition);
    {String*} copyAttributeNames = copyAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name);
    assertTrue(copyAttributeNames.contains("todir"));
    AntDefinition? filesetAntDefinition = filterAntDefinition(copyAntDefinition.nestedAntDefinitions(), "fileset");
    assert(exists filesetAntDefinition);
    {String*} filesetAttributeNames = filesetAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name);
    assertTrue(filesetAttributeNames.contains("dir"));
    AntDefinition? includeAntDefinition = filterAntDefinition(filesetAntDefinition.nestedAntDefinitions(), "include");
    assert(exists includeAntDefinition);
    {String*} includeAttributeNames = includeAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name);
    assertTrue(includeAttributeNames.contains("name"));
}

test void testProperties() {
    AntProject antProject = currentAntProject();
    Map<String,String> allProperties = antProject.allProperties();
    assertTrue(allProperties.size > 0);
    // now print out all properties
    Set<String> propertyNames = allProperties.keys;
    String[] propertyNamesSorted = propertyNames.sort(byIncreasing((String s) => s));
    for(propertyName in propertyNamesSorted) {
        String? propertyValue = allProperties.get(propertyName);
        if(exists propertyValue) {
            print("``propertyName``=``propertyValue``");
        }
    }
}

test void testProperty() {
    String propertyName = "ceylon.ant.test.test-property";
    String propertyConstant = "test-property-set";
    AntProject antProject = currentAntProject();
    antProject.setProperty(propertyName, null);
    String? propertyValue1 = antProject.getProperty(propertyName);
    assertEquals(propertyValue1, null);
    antProject.setProperty(propertyName, propertyConstant);
    String? propertyValue2 = antProject.getProperty(propertyName);
    assertEquals(propertyValue2, propertyConstant);
}

"Test whether the `<ftp>` tasks exists, which is part of modul `org.apache.ant.ant-commons-net`.
 Depending of Ceylon's module implementation, this might not be available to the module `ceylon.ant`."
test void testExternalDependency() {
    AntProject antProject = currentAntProject();
    AntDefinition? ftpAntDefinition = filterAntDefinition(antProject.allTopLevelAntDefinitions(), "ftp");
    AntDefinition? undefinedAntDefinition = filterAntDefinition(antProject.allTopLevelAntDefinitions(), "--undefined--");
    if(exists ftpAntDefinition) {
        // ok
    } else {
        throw Exception("ExternalDependency: ftp task does not exists. Module org.apache.ant.ant-commons-net not imported properly.");
    }
    if(exists undefinedAntDefinition) {
        throw Exception("ExternalDependency: --undefined-- task exists.");
    } else {
        // ok
    }
}

"Checks the difference between top level <include> task and <include> datatype within <fileset>"
test void testIncludeAsTaskAndType() {
    AntProject antProject = currentAntProject();
    AntDefinition? includeAntDefinition = filterAntDefinition(antProject.allTopLevelAntDefinitions(), "include");
    assert(exists includeAntDefinition);
    print("<include: ``includeAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name)``>");
    AntDefinition? copyAntDefinition = filterAntDefinition(antProject.allTopLevelAntDefinitions(), "copy");
    assert(exists copyAntDefinition);
    print("<copy: ``copyAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name)``>");
    AntDefinition? copyFilesetAntDefinition = filterAntDefinition(copyAntDefinition.nestedAntDefinitions(), "fileset");
    assert(exists copyFilesetAntDefinition);
    print("<copy-fileset: ``copyFilesetAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name)``>");
    AntDefinition? copyFilesetIncludeAntDefinition = filterAntDefinition(copyFilesetAntDefinition.nestedAntDefinitions(), "include");
    assert(exists copyFilesetIncludeAntDefinition);
    print("<copy-fileset-include: ``copyFilesetIncludeAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name)``>");
    assertTrue(includeAntDefinition.isTask());
    assertTrue(includeAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name).contains("taskname"));
    assertFalse(includeAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name).contains("name"));
    assertFalse(copyFilesetIncludeAntDefinition.isTask());
    assertTrue(copyFilesetIncludeAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name).contains("name"));
    assertFalse(copyFilesetIncludeAntDefinition.attributes().map<String>((AntAttributeDefinition a) => a.name).contains("taskname"));
}

"Test a typesave by hand generated Ceylon interface"
test void testPseudoGenerated() {
    String buildDirectory = "target/build-test-file-tasks-directory";
    Ant fileset = Ant("fileset", { "dir" -> "``buildDirectory``" }, [
        Ant("include", { "name" -> "example.txt" } )
    ] );
    ant("mkdir", { "dir" -> "``buildDirectory``" } );
    verifyResource("``buildDirectory``", `Directory`, "Cannot create directory");
    ant("echo", { "message" -> "File created.", "file" -> "``buildDirectory``/example.txt" } );
    verifyResource("``buildDirectory``/example.txt", `File`, "Cannot create file");
    ant("mkdir", { "dir" -> "``buildDirectory``/sub-directory" } );
    verifyResource("``buildDirectory``/sub-directory", `Directory`, "Cannot create directory");
    // invocation of pseudo generated typesave Ant interface (need to get rid of _containingElements someday)
    copy { todir="``buildDirectory``/sub-directory"; _containingElements = [
        FileSet{ dir="``buildDirectory``"; _containingElements = [
            Include { name = "example.txt"; }
        ]; }
    ]; };
    // end typesave invocation
    verifyResource("``buildDirectory``/sub-directory/example.txt", `File`, "Cannot copy to file");
    ant("delete", { }, [
        fileset
    ] );
    verifyResource("``buildDirectory``/example.txt", `Nil`, "Cannot delete file");
    ant("delete", { "dir" -> "``buildDirectory``", "verbose" -> "true" } );
    verifyResource("``buildDirectory``", `Nil`, "Cannot delete directory");
}
