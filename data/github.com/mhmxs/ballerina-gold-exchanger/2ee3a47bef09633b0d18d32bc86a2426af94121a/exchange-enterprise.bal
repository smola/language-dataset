import ballerina/http;
import ballerina/log;
import ballerinax/kubernetes;

type CurrencyNotFoundError error<string>;

type Exchanger object {
    private map<float> currencies;

    function __init() {
        map<float> currencies = {
            USD: 34.80,
            EUR: 35.70,
            GBP: 32.00
        };
        self.currencies = currencies.freeze();
    }

    function getExchangeRate(string currency) returns float|CurrencyNotFoundError {
        float[] rates = self.currencies
            .filter(function ((string, float) pair) returns boolean {
                var (key, _) = pair;
                return key == currency;
            })
            .map(function ((string, float) pair) returns float {
                var (_, value) = pair;
                return value;
            });
        if (rates.length() > 0) {
            return rates[0];
        }
        CurrencyNotFoundError err = error(string `Currency not found: {{currency}}`);
        return err;
    }
};

type Response record {
    string currency;
    float rate;
    string description?; // runtime KeyNotFound !!!
};

type HttpHandler object {
    private Exchanger e = new;

    function generateResponse(http:Request request) returns json|error;
};

function HttpHandler.generateResponse(http:Request request) returns @untainted json|error {
    string currency = check request.getTextPayload();
    log:printInfo(string `Looking for currency {{ currency }}`);
    float rate = check self.e.getExchangeRate(currency);
    Response response = { currency: currency, rate: rate };
    json responseMap = check json.convert(response);
    return responseMap;
}

@kubernetes:Deployment {
    image: "exchange-enterprise:v.1.0",
    dockerHost: "tcp://192.168.99.100:2376", 
    dockerCertPath: "/Users/rkovacs/.minikube/certs"
}
@http:ServiceConfig {
    basePath: "/exchangeEnterprise"
}
@kubernetes:Service {
    serviceType: "NodePort"
}
service exchangeEnterprise on new http:Listener(9092) {
    private HttpHandler handler = new;

    resource function getRate(http:Caller caller, http:Request request) {
        log:printInfo("Request accepted");
        json|error message = self.handler.generateResponse(request);

        http:Response response = new;
        if (message is json) {
            log:printInfo(message.toString());
            response.setJsonPayload(message);
        } else {
            response.setJsonPayload({ "error": string `Invalid currency: {{ message.reason() }}` });
        }
        _ = caller -> respond(response);
    }
}

