import ballerina/grpc;
import ballerina/io;

public type InventoryServiceBlockingClient client object {
    private grpc:Client grpcClient = new;
    private grpc:ClientEndpointConfig config = {};
    private string url;

    function __init(string url, grpc:ClientEndpointConfig? config = ()) {
        self.config = config ?: {};
        self.url = url;
        // initialize client endpoint.
        grpc:Client c = new;
        c.init(self.url, self.config);
        error? result = c.initStub("blocking", ROOT_DESCRIPTOR, getDescriptorMap());
        if (result is error) {
            panic result;
        } else {
            self.grpcClient = c;
        }
    }


    remote function getItem(string req, grpc:Headers? headers = ()) returns ((Item, grpc:Headers)|error) {
        
        var payload = check self.grpcClient->blockingExecute("inventory.InventoryService/getItem", req, headers = headers);
        grpc:Headers resHeaders = new;
        any result = ();
        (result, resHeaders) = payload;
        var value = Item.convert(result);
        if (value is Item) {
            return (value, resHeaders);
        } else {
            error err = error("{ballerina/grpc}INTERNAL", {"message": value.reason()});
            return err;
        }
    }

};

public type InventoryServiceClient client object {
    private grpc:Client grpcClient = new;
    private grpc:ClientEndpointConfig config = {};
    private string url;

    function __init(string url, grpc:ClientEndpointConfig? config = ()) {
        self.config = config ?: {};
        self.url = url;
        // initialize client endpoint.
        grpc:Client c = new;
        c.init(self.url, self.config);
        error? result = c.initStub("non-blocking", ROOT_DESCRIPTOR, getDescriptorMap());
        if (result is error) {
            panic result;
        } else {
            self.grpcClient = c;
        }
    }


    remote function getItem(string req, service msgListener, grpc:Headers? headers = ()) returns (error?) {
        
        return self.grpcClient->nonBlockingExecute("inventory.InventoryService/getItem", req, msgListener, headers = headers);
    }

};

type Item record {
    string id;
    int quantity;
    
};



const string ROOT_DESCRIPTOR = "0A16496E76656E746F7279536572766963652E70726F746F1209696E76656E746F72791A1E676F6F676C652F70726F746F6275662F77726170706572732E70726F746F22320A044974656D120E0A02696418012001280952026964121A0A087175616E7469747918022001280352087175616E74697479324C0A10496E76656E746F72795365727669636512380A076765744974656D121C2E676F6F676C652E70726F746F6275662E537472696E6756616C75651A0F2E696E76656E746F72792E4974656D620670726F746F33";
function getDescriptorMap() returns map<string> {
    return {
        "InventoryService.proto":"0A16496E76656E746F7279536572766963652E70726F746F1209696E76656E746F72791A1E676F6F676C652F70726F746F6275662F77726170706572732E70726F746F22320A044974656D120E0A02696418012001280952026964121A0A087175616E7469747918022001280352087175616E74697479324C0A10496E76656E746F72795365727669636512380A076765744974656D121C2E676F6F676C652E70726F746F6275662E537472696E6756616C75651A0F2E696E76656E746F72792E4974656D620670726F746F33",
        "google/protobuf/wrappers.proto":"0A1E676F6F676C652F70726F746F6275662F77726170706572732E70726F746F120F676F6F676C652E70726F746F62756622230A0B446F75626C6556616C756512140A0576616C7565180120012801520576616C756522220A0A466C6F617456616C756512140A0576616C7565180120012802520576616C756522220A0A496E74363456616C756512140A0576616C7565180120012803520576616C756522230A0B55496E74363456616C756512140A0576616C7565180120012804520576616C756522220A0A496E74333256616C756512140A0576616C7565180120012805520576616C756522230A0B55496E74333256616C756512140A0576616C756518012001280D520576616C756522210A09426F6F6C56616C756512140A0576616C7565180120012808520576616C756522230A0B537472696E6756616C756512140A0576616C7565180120012809520576616C756522220A0A427974657356616C756512140A0576616C756518012001280C520576616C756542570A13636F6D2E676F6F676C652E70726F746F627566420D577261707065727350726F746F50015A057479706573F80101A20203475042AA021E476F6F676C652E50726F746F6275662E57656C6C4B6E6F776E5479706573620670726F746F33"
        
    };
}

