import ballerina/http;
import ballerinax/kubernetes;
import wso2/twitter;
import ballerina/config;
import ballerina/io;

endpoint twitter:Client tw {
    accessToken: config:getAsString("accessToken"),
    accessTokenSecret: config:getAsString("accessTokenSecret"),
    clientId: config:getAsString("clientId"),
    clientSecret: config:getAsString("clientSecret"),
    clientConfig: {}
};

endpoint http:Client quoteEP {
    url: "http://quotes:8080",
    retryConfig: {
        // Initial retry interval in milliseconds.
        interval: 20,
        // Number of retry attempts before giving up
        count: 5,
        // Multiplier of the retry interval to exponentailly
        // increase; retry interval
        backOffFactor: 2,
        // Upper limit of the retry interval in milliseconds
        // If interval into backOffFactor value exceeded
        // maxWaitInterval interval values. maxWaitInterval
        // will be considered as the retry intrval.
        maxWaitInterval: 20000
    }
};
@kubernetes:Ingress {
    hostname: "demo.lakmal.me"
}
@kubernetes:Service {
    serviceType: "NodePort",
    name: "hello-service"
}
endpoint http:Listener listener {
    port: 9090
};

@http:ServiceConfig {
    basePath: "/"
}
@kubernetes:ConfigMap {
    ballerinaConf: "../twitter.toml"
}
@kubernetes:Deployment {
    name: "hello-service-deployment",
    image: "hello-service-k8s",
    replicas: 1
}
service<http:Service> hello bind listener{
    @http:ResourceConfig {
        path: "/",
        methods: ["POST"]
    }
    hi (endpoint caller, http:Request request) {
        http:Response res;
        json payload = check request.getJsonPayload();

        http:Response quote = check quoteEP->post("/quotes/quote/",untaint payload["quote"]);
        
        json quoteJson = check quote.getJsonPayload();
        // get the first quote from the list of quotes
        string quoteStr = check <string>quoteJson[0]["QUOTE"];
        // extract event value from the payload json
        string event = check <string>payload["event"];
        // extract mention value from the payload json
        string mention = check <string>payload["mention"];

        string tweetStr = quoteStr + " " + event + " " + mention; 

        twitter:Status st = check tw->tweet(tweetStr);
        json resJson = {
            text: tweetStr,
            id: st.id,
            agent: "ballerina"
        }; 
        res.setPayload(untaint resJson);
        _ = caller->respond(res);
    }
}
