import ballerina/http;
import ballerina/log;
import ballerina/time;
import ballerinax/kubernetes;
import ballerinax/istio;

@istio:Gateway {}
@istio:VirtualService {}
@kubernetes:Service {
    name: "ballerina-time-service"
}
listener http:Listener timeEP = new(9095);

@kubernetes:Deployment {
    image: "ballerina-time-service",
    name: "ballerina-time-service"
}
@http:ServiceConfig { basePath: "/localtime" }
service time on timeEP {
    @http:ResourceConfig {
        path: "/",
        methods: ["GET"]
    }
    resource function getTime (http:Caller caller, http:Request request) {
        time:Time currentTime = time:currentTime();
        var customTimeString = time:format(currentTime, "yyyy-MM-dd'T'HH:mm:ss");
        if (customTimeString is string) {
            json timeJ = { currentTime: customTimeString };
            var responseResult = caller->respond(timeJ);
            if (responseResult is error) {
                log:printError("Error responding back to client");
            }
        } else {
            http:Response errorResponse = new;
            json errorJson = { errorMsg: "internal server error occurred" };
            errorResponse.setPayload(errorJson);
            errorResponse.statusCode = 500;
            var responseResult = caller->respond(errorResponse);
            if (responseResult is error) {
                log:printError("Error responding back to client");
            }
        }
    }
}
