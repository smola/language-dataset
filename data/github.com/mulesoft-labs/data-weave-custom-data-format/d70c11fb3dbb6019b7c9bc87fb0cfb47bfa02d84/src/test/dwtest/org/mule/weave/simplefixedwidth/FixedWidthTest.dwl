%dw 2.0
import * from dw::test::Tests
import * from dw::test::Asserts
import * from org::mule::weave::simplefixedwidth::FixedWidth
---
"Fixed Width Data Format" describedBy [
    "Should parse correctly in" in do {
        var content = "Mariano             de Achaval          " as Binary {encoding: "UTF-8"}
        var testResult = readFixedWidth(content, "UTF-8", {schemaUrl: "classpath://fw-schema.yml"})
        ---
        testResult must equalTo([{name: "Mariano",	lastName: "de Achaval"}])
    },
    "Should write correctly in" in do {
            var content = [{ name: "Mariano", lastName: "de Achaval" }]
            var testResult = writeFixedWidth(content, {schemaUrl: "classpath://fw-schema.yml"}) as String {encoding: "UTF-8"}
            ---
            testResult must equalTo("Mariano             de Achaval          \n")
        }

]

