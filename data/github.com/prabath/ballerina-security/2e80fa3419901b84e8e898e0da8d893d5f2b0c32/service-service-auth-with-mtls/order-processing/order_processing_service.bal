import ballerina/http;
import ballerina/runtime;
import ballerina/log;
import ballerina/io;

http:AuthProvider jwtAuthProvider = {
    scheme:"jwt",
    issuer:"wso2is",
    audience: "3VTwFk7u1i366wzmvpJ_LZlfAV4a",
    certificateAlias:"wso2carbon",
    trustStore: {
        path: "order-processing/keys/truststore.p12",
        password: "wso2carbon"
    }
};

endpoint http:Client httpEndpoint {
    url: "https://localhost:9009",
    secureSocket: {
        keyStore: {
            path: "order-processing/keys/keystore.p12",
            password: "wso2carbon"
        },
        trustStore: {
            path: "order-processing/keys/truststore.p12",
            password: "wso2carbon"
        },
        protocol: {
            name: "TLS",
            versions: ["TLSv1.2"]
        },
        ciphers: ["TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA"]
    }
};

endpoint http:Listener ep {
    port: 9008,
    authProviders:[jwtAuthProvider],

    secureSocket: {
        keyStore: {
            path: "order-processing/keys/keystore.p12",
            password: "wso2carbon"
        },
        trustStore: {
            path: "order-processing/keys/truststore.p12",
            password: "wso2carbon"
        }
    }
};

@http:ServiceConfig {
    basePath: "/order-processing",
    authConfig: {
        authentication: { enabled: true }
    }
}
service<http:Service> orderprocessing bind ep {
    @http:ResourceConfig {
        methods: ["POST"],
        path: "/orders",
        authConfig: {
            scopes: ["place-order"]
        }
    }
    placeOrder(endpoint caller, http:Request req) {
        http:Request invReq = new;
        json invPayload = {"items" :[{"code" : "10001","qty" : 4}]};
        invReq.setJsonPayload(invPayload, contentType = "application/json");

        // add a custom header to carry the end-user data.
        invReq.addHeader("authn-user", runtime:getInvocationContext().userPrincipal.username);
        
        var response = httpEndpoint->post("/inventory/items",invReq);
        match response {
            http:Response resp => { 
                string log = "response from inventory service " + check resp.getPayloadAsString();
                log:printInfo(log);
                json success = {"status" : "order created successfully"};
                http:Response res = new;
                res.setPayload(success);
                _ = caller->respond(res);
            }
            error err => { 
                log:printError("call to the inventory endpoint failed.");
                json failure = {"status" : "failed to create a new order"};
                http:Response res = new;
                res.setPayload(failure);
                res.statusCode = 500;
                _ = caller->respond(res);
            }
        }        
    }
}


