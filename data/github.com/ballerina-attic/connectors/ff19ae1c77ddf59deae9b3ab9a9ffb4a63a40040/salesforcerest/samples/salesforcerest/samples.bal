import org.wso2.ballerina.connectors.salesforcerest;

import ballerina.lang.jsons;
import ballerina.lang.system;
import ballerina.net.http;

function main (string[] args) {

    http:Response response;
    json JSONResponse;

    string accessToken = args[1];
    string clientID = args[2];
    string clientSecret = args[3];
    string refreshToken = args[4];
    string apiInstance = args[5];
    string refreshEndpoint = args[6];

    salesforcerest:ClientConnector salesforceClient;

    if (args[0] == "describeGlobal") {
        system:println("-----Calling describeGlobal action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.describeGlobal (args[7]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "sObjectDescribe") {
        system:println("-----Calling sObjectDescribe action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.sObjectDescribe (args[8], args[7]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "listAvailableApiVersion") {
        system:println("-----Calling listAvailableApiVersion action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.listAvailableApiVersion ();
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "listOrganizationLimits") {
        system:println("-----Calling listOrganizationLimits action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.listOrganizationLimits (args[7]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "listResourcesByApiVersion") {
        system:println("-----Calling listResourcesByApiVersion action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.listResourcesByApiVersion (args[7]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "sObjectBasicInfo") {
        system:println("-----Calling sObjectBasicInfo action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.sObjectBasicInfo (args[8], args[7]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "sObjectGetDeleted") {
        system:println("-----Calling sObjectGetDeleted action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.sObjectGetDeleted (args[7], args[8], args[9], args[10]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "sObjectGetUpdated") {
        system:println("-----Calling sObjectGetUpdated action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.sObjectGetUpdated (args[7], args[8], args[9], args[10]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "sObjectPlatformAction") {
        system:println("-----Calling sObjectPlatformAction action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.sObjectPlatformAction (args[7]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "sObjectRows") {
        system:println("-----Calling sObjectRows action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.sObjectRows (args[7], args[8], args[9]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "sObjectRowsByExternalId") {
        system:println("-----Calling sObjectRowsByExternalId action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.sObjectRowsByExternalId (args[7], args[8], args[9], args[10]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "listviewQueryPerformanceFeedback") {
        system:println("-----Calling listviewQueryPerformanceFeedback action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.listviewQueryPerformanceFeedback (args[7], args[8]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "query") {
        system:println("-----Calling query action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.query (args[7], args[8]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "queryAll") {
        system:println("-----Calling queryAll action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.queryAll (args[7], args[8]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "queryAllMore") {
        system:println("-----Calling queryAllMore action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.queryAllMore (args[7], args[8]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "queryMore") {
        system:println("-----Calling queryMore action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.queryMore (args[7], args[8]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "queryPerformanceFeedback") {
        system:println("-----Calling queryPerformanceFeedback action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.queryPerformanceFeedback (args[7], args[8]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "createRecord") {
        system:println("-----Calling createRecord action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        json sampleCreateRecord = {"Name":"TestingBallerina"};
        response = salesforceClient.createRecord (args[7], "Account", sampleCreateRecord);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "createMultipleRecords") {
        system:println("-----Calling createMultipleRecords action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        json sampleCreateMultipleRecords = {"records":[{"attributes":{"type":"Account", "referenceId":"ref1"}, "name":"SampleAccount1"}, {"attributes":{"type":"Account", "referenceId":"ref2"}, "name":"SampleAccount2"}]};
        response = salesforceClient.createMultipleRecords (args[7], "Account", sampleCreateMultipleRecords);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "delete") {
        system:println("-----Calling delete action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.delete (args[7], args[8], args[9]);
        system:println(response.getStatusCode());
    }

    if (args[0] == "retrieveFieldValues") {
        system:println("-----Calling retrieveFieldValues action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.retrieveFieldValues (args[7], args[8], args[9], args[10]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "retrieveFieldValuesFromExternalObject") {
        system:println("-----Calling retrieveFieldValuesFromExternalObject action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.retrieveFieldValuesFromExternalObject (args[7], args[8],
                                                                           args[9], args[10]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "retrieveStandardFieldValuesFromExternalObjectWithExternalId") {
        system:println("-----Calling retrieveStandardFieldValuesFromExternalObjectWithExternalId action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        response = salesforceClient.retrieveStandardFieldValuesFromExternalObjectWithExternalId (args[7], args[8],
                                                                                                 args[9], args[10]);
        JSONResponse = response.getJsonPayload();
        system:println(jsons:toString(JSONResponse));
    }

    if (args[0] == "update") {
        system:println("-----Calling update action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        json sampleUpdate = {"Name":"TestUpdate"};
        response = salesforceClient.update (args[7], "Account", args[8], sampleUpdate);
        system:println(response.getStatusCode());
    }

    if (args[0] == "upsert") {
        system:println("-----Calling upsert action-----");
        salesforceClient = create salesforcerest:ClientConnector(accessToken, clientID, clientSecret, refreshToken,
                                                                 apiInstance, refreshEndpoint);
        json sampleUpsert = {"Name":"TestUpdate", "Type":"New Customer"};
        response = salesforceClient.upsert (args[7], args[8], args[9], args[10], sampleUpsert);
        system:println(response.getStatusCode());
    }
}