// Copyright (c) 2018 WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
//
// WSO2 Inc. licenses this file to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file except
// in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.

import ballerina/config;
import ballerina/log;
import ballerina/test;

string testJsonRpcVersion = config:getAsString("JSON_RPC_VERSION");
string testNetworkId = config:getAsString("NETWORK_ID");
string testJsonRpcEndpoint = config:getAsString("JSON_RPC_ENDPOINT");
string dataForSHA3 = config:getAsString("DATA");
string testBlock = config:getAsString("BLOCK");
string testposition = config:getAsString("POSITION");
string testBlockHash = config:getAsString("BLOCK_HASH");
string testMessage = config:getAsString("MESSAGE");
string testTransactionOptionsData = config:getAsString("TRANSACTION_OPTION_DATA");
string testTransactionObjectStatus = config:getAsString("TRANSACTION_OBJECT_STATUS");
string testTransactionCallObjectTovalue = config:getAsString("TRANSACTION_CALL_OBJECT_TO_VALUE");
string testNonce = config:getAsString("NONCE");
string testHeaderPowHash = config:getAsString("HEADER_POW_HASH");
string testMixDigest = config:getAsString("MIX_DIGEST");
string testAddress = "";
string testFilterID = "";

EthereumConfiguration testEthereumConfig = {
    jsonRpcVersion: testJsonRpcVersion,
    networkId: testNetworkId,
    jsonRpcEndpoint: testJsonRpcEndpoint
};

Client testEthereumClient = new (testEthereumConfig);

@test:Config {}
function testGetWeb3ClientVersion() {
    log:printInfo("Test case for get version of Web3Client.");
    string result = "";
    var ethereumRes = testEthereumClient->getWeb3ClientVersion();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(ethereumRes, " ", msg = "Failed to get version of Web3Client.");
}

@test:Config {}
function testGetWeb3Sha3() {
    log:printInfo("Test case for get Keccak-256 (not the standardized SHA3-256) of the given data");
    string result = "";
    var ethereumRes = testEthereumClient->getWeb3Sha3(dataForSHA3);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to convert data into a SHA3 hash.");
}

@test:Config {}
function testGetNetVersion() {
    log:printInfo("Test case for get current network id.");
    string result = "";
    var ethereumRes = testEthereumClient->getNetVersion();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get current network id.");
}

@test:Config {}
function testGetNetListening() {
    log:printInfo("Test case for check whether the client is actively listening for network connections/not.");
    boolean result = false;
    var ethereumRes = testEthereumClient->getNetListening();
    if (ethereumRes is boolean) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to check whether the client is actively listening for
     network connections/not.");
}

@test:Config {}
function testGeNetPeerCount() {
    log:printInfo("Test case for get the number of peers currently connected to the client.");
    string result = "";
    var ethereumRes = testEthereumClient->getNetPeerCount();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the number of peers currently
    connected to the client.");
}

@test:Config {}
function testGetEthProtocolVersion() {
    log:printInfo("Test case for get current ethereum protocol version.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthProtocolVersion();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get current ethereum protocol version.");
}

@test:Config {}
function testGetEthSyncing() {
    log:printInfo("Test case for get an object with data about the sync status.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthSyncing();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get an object with data about the sync status");
}

@test:Config {}
function testGetEthCoinbase() {
    log:printInfo("Test case for get the current coinbase address.-");
    string result = "";
    var ethereumRes = testEthereumClient->getEthCoinbase();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the current coinbase address.");
}

@test:Config {}
function testGetEthMining() {
    log:printInfo("Test case for check whether client is actively mining new blocks/not.");
    boolean result = false;
    var ethereumRes = testEthereumClient->getEthMining();
    if (ethereumRes is boolean) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to check whether client is actively mining new blocks/not.");
}

@test:Config {}
function testGetEthHashrate() {
    log:printInfo("Test case for get the number of hashes per second that the node is mining with.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthHashrate();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the number of hashes per second
    that the node is mining with.");
}

@test:Config {}
function testGetEthGasPrice() {
    log:printInfo("Test case for get the current price per gas in wei.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthGasPrice();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the current price per gas in wei.");
}

@test:Config {}
function testGetEthAccounts() {
    log:printInfo("Test case for get a list of addresses owned by client.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthAccounts();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    testAddress = <@untainted>result;
    test:assertNotEquals(result, " ", msg = "Failed to get a list of addresses owned by client.");
}

@test:Config {}
function testGetEthBlockNumber() {
    log:printInfo("Test case for get the number of most recent block.-");
    string result = "";
    var ethereumRes = testEthereumClient->getEthBlockNumber();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the number of most recent block.");
}

@test:Config {
    dependsOn: ["testGetEthAccounts"]
}
function testGetEthBalance() {
    log:printInfo("Test case for get the balance of the account of given address.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthBalance(testAddress, testBlock);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the balance of the account of given address.");
}

@test:Config {
    dependsOn: ["testGetEthAccounts"]
}
function testGetEthStorageAt() {
    log:printInfo("Test case for get the value from a storage position at a given address.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthStorageAt(testAddress, testposition, testBlock);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the value from a storage
    position at a given address.");
}

@test:Config {
    dependsOn: ["testGetEthAccounts"]
}
function testGetEthTransactionCount() {
    log:printInfo("Test case for get the number of transactions sent from an address.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthTransactionCount(testAddress, testBlock);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the number of transactions sent from an address.");
}

@test:Config {}
function testGetEthBlockTransactionCountByHash() {
    log:printInfo("Test case for get the number of transactions in a block from a block matching the given block "
        + "hash.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthBlockTransactionCountByHash(testBlockHash);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the number of transactions in a block from a block matching"
    + " the given block hash.");
}

@test:Config {}
function testGetEthBlockTransactionCountByNumber() {
    log:printInfo("Test case for get the number of transactions in a block matching the given block number.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthBlockTransactionCountByNumber(testBlock);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the number of transactions in a block
    matching the given block number.");
}

@test:Config {}
function testGetEthUncleCountByBlockHash() {
    log:printInfo("Test case for get the number of uncles in a block from a block matching the given block hash.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthUncleCountByBlockHash(testBlockHash);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the number of uncles in a block from a block
    matching the given block hash.");
}

@test:Config {}
function testGetEthUncleCountByBlockNumber() {
    log:printInfo("Test case for get the number of uncles in a block from a block matching the given block number.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthUncleCountByBlockNumber(testBlock);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the number of uncles in a block from
    a block matching the given block number.");
}

@test:Config {
    dependsOn: ["testGetEthAccounts"]
}
function testGetEthCode() {
    log:printInfo("Test case for get code at a given address.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthCode(testAddress, testBlock);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get code at a given address.");
}

@test:Config {
    dependsOn: ["testGetEthAccounts"]
}
function testGetEthSign() {
    log:printInfo("Test case for calculates an Ethereum specific signature.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthSign(testAddress, testMessage);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to calculate an Ethereum specific signature");
}

@test:Config {
    dependsOn: ["testGetEthAccounts"]
}
function testSendEthTransaction() {
    log:printInfo("Test case for creates new message call transaction or a contract creation, if the data field "
        + "contains code.");
    string result = "";
    json testTransactionOptions = {"from": testAddress, "data": dataForSHA3};
    var ethereumRes = testEthereumClient->sendEthTransaction(testTransactionOptions);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to create new message call transaction or
     a contract creation, if the data field contains code.");
}

@test:Config {}
function testSendEthRawTransaction() {
    log:printInfo("Test case for creates new message call transaction or a contract creation for signed transactions.");
    string result = "";
    var ethereumRes = testEthereumClient->sendEthRawTransaction(testTransactionOptionsData);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to creates new message call transaction or a contract creation for"
        + " signed transactions.");
}

@test:Config {}
function testGetEthCall() {
    log:printInfo("Test case for executes a new message call immediately without creating a transaction on the block"
        + " chain.");
    string result = "";
    json testTransactionCallObject = {"to": testTransactionCallObjectTovalue};
    var ethereumRes = testEthereumClient->getEthCall(testTransactionCallObject, testBlock);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to executes a new message call immediately without creating a"
        + " transaction on the block chain.");
}

@test:Config {}
function testGetEthEstimateGas() {
    log:printInfo("Test case for generates and get an estimate of how much gas is necessary to allow the transaction "
        + "to complete.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthEstimateGas({});
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to generates and get an estimate of how much gas is necessary to"
        + " allow the transaction to complete.");
}

@test:Config {}
function testGetEthBlockByHash() {
    log:printInfo("Test case for get information about a block by hash.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthBlockByHash(testBlockHash, getBoolean(testTransactionObjectStatus));
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get information about a block by hash.");
}

@test:Config {}
function testGetEthBlockByNumber() {
    log:printInfo("Test case for get information about a block by block number.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthBlockByNumber(testBlock, getBoolean(testTransactionObjectStatus));
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get information about a block by block number");
}

@test:Config {}
function testGetEthTransactionByHash() {
    log:printInfo("Test case for get the information about a transaction requested by transaction hash.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthTransactionByHash(testBlockHash);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to the information about a transaction requested by transaction"
        + " hash.");
}

@test:Config {}
function testGetEthTransactionByBlockHashAndIndex() {
    log:printInfo("Test case for get information about a transaction by block hash and transaction index position.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthTransactionByBlockHashAndIndex(testBlockHash, testposition);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the information about a transaction by block hash and"
        + " transaction index position.");
}

@test:Config {}
function testGetEthTransactionByBlockNumberAndIndex() {
    log:printInfo("Test case for get information about a transaction by block number and transaction index position.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthTransactionByBlockNumberAndIndex(testBlock, testposition);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get information about a transaction by block number and"
        + " transaction index position.");
}

@test:Config {}
function testGetEthTransactionReceipt() {
    log:printInfo("Test case for the receipt of a transaction by transaction hash");
    string result = "";
    var ethereumRes = testEthereumClient->getEthTransactionReceipt(testBlockHash);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get the receipt of a transaction by transaction hash.");
}

@test:Config {}
function testGetEthUncleByBlockHashAndIndex() {
    log:printInfo("Test case for get information about a uncle of a block by hash and uncle index position.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthUncleByBlockHashAndIndex(testBlockHash, testposition);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get information about a uncle of a block by hash and uncle"
        + " index position.");
}

@test:Config {}
function testGetEthUncleByBlockNumberAndIndex() {
    log:printInfo("Test case for get information about a uncle of a block by number and uncle index position.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthUncleByBlockNumberAndIndex(testBlock, testposition);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get information about a uncle of a block by number and uncle"
        + " index position.");
}

@test:Config {}
function testEthNewFilter() {
    log:printInfo("Test case for creates a filter object, based on filter options, to notify when the state changes.");
    string result = "";
    var ethereumRes = testEthereumClient->ethNewFilter({});
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    testFilterID = <@untainted>result;
    test:assertNotEquals(result, " ", msg = "Failed to creates a filter object, based on filter options, to notify"
        + " when the state changes (logs). ");
}

@test:Config {}
function testEthNewBlockFilter() {
    log:printInfo("Test case for creates a filter in the node, to notify when a new block arrives.");
    string result = "";
    var ethereumRes = testEthereumClient->ethNewBlockFilter();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to creates a filter in the node, to notify when a new block"
        + " arrives.");
}

@test:Config {}
function testEthNewPendingTransactionFilter() {
    log:printInfo("Test case for creates a filter in the node, to notify when new pending transactions arrive.");
    string result = "";
    var ethereumRes = testEthereumClient->ethNewPendingTransactionFilter();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to creates a filter in the node, to notify when new pending"
        + " transactions arrive.");
}

@test:Config {
    dependsOn: ["testEthNewFilter"]
}
function testUninstallEthFilter() {
    log:printInfo("Test case for uninstalls a filter with given id.");
    string result = "";
    var ethereumRes = testEthereumClient->uninstallEthFilter(testFilterID);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to uninstalls a filter with given id.");
}

@test:Config {
    dependsOn: ["testEthNewFilter"]
}
function testGetEthFilterLogs() {
    log:printInfo("Test case for get an array of all logs matching filter with given id.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthFilterLogs(testFilterID);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get an array of all logs matching filter with given id.");
}

@test:Config {}
function testGetEthFilterChanges() {
    log:printInfo("Test case for get an array of logs which occurred since last poll.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthFilterChanges(testFilterID);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to returns an array of logs which occurred
    since last poll.");
}

@test:Config {}
function testGetEthLogs() {
    log:printInfo("Test case for get an array of all logs matching a given filter object.");
    string result = "";
    var ethereumRes = testEthereumClient->getEthLogs({});
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to get an array of all logs matching a given filter object.");
}

@test:Config {}
function testGetEthWork() {
    log:printInfo("Test case for get the hash of the current block, the seedHash, and the boundary condition to be met" 
        + "(\"target\").");
    string result = "";
    var ethereumRes = testEthereumClient->getEthWork();
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to the hash of the current block, the seedHash, and the boundary"
        + " condition to be met (\"target\")");
}

@test:Config {}
function testSubmitEthWork() {
    log:printInfo("Test case for submitting a proof-of-work solution.");
    string result = "";
    var ethereumRes = testEthereumClient->submitEthWork(testNonce, testHeaderPowHash, testMixDigest);
    if (ethereumRes is string) {
        result = ethereumRes;
    } else {
        test:assertFail(msg = <string>ethereumRes.detail()?.message);
    }
    test:assertNotEquals(result, " ", msg = "Failed to submitting a proof-of-work solution.");
}
