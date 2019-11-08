// Add annotations for @ServiceConfig & @ResourceConfig
// to provide custom path and limit to POST
// Get payload from the POST request

// To run it:
// ballerina run 2_demo_annotations.bal
// To invoke:
// curl -X POST -d "Demo" localhost:9090

import ballerina/http;

// Add this annotation to the service to change the base path.
@http:ServiceConfig {
   basePath: "/"
}
service hello on new http:Listener(9090) {
    // Add this annotation to the resource to change its path
    // and to limit the calls to POST only.
    @http:ResourceConfig {
        path: "/",
        methods: ["POST"]
    }
    resource function hi (http:Caller caller, http:Request request) returns error? {
        // Extract the payload from the request
        // getTextPayload actually returns a union of string | error.
        // We will show how to handle the error later in the demo
        // for now, just use check that "removes" the error
        // (in reality, if there is error it will pass it up the caller stack).
        var payload = check request.getTextPayload();
        http:Response res = new;
        // use it in the response
        res.setPayload("Hello " + <@untainted> payload + "!\n");
        _ = check caller->respond(res);
        return;
    }
}
