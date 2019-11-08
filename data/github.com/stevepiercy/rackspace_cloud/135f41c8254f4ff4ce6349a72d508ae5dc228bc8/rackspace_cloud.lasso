[
/* ===========================================================================

Rackspace Cloud Files API for Lasso

This custom type is a Lasso 8 an implementation of both the Rackspace Cloud Files API[1] and the Cloud Identity API[2].  The most useful methods include retrieving the authentication token, getting URLs of containers, and getting a list of objects in a container.

To contribute to this project, please visit the GitHub repository:
https://github.com/stevepiercy/rackspace_cloud

Documentation of and an article on this project are published at:
http://www.stevepiercy.com/articles/rackspace_cloud-lasso-sdk-for-the-rackspace-cloud-files-api/

[1] http://docs.rackspace.com/files/api/v1/cf-devguide/content/Overview-d1e70.html
[2] http://docs.rackspace.com/auth/api/v2.0/auth-client-devguide/content/Overview-d1e65.html

Steve Piercy
web@stevepiercy.com
http://www.stevepiercy.com/

=========================================================================== */

define_type('rackspace_cloudfiles');
   private;
        // These instance variables are private because we do not want to
        // expose them to the developer outside of this source code.
        // Configure credentials to get an authentication token.
        local('credentials') = map(
            'username' = 'XXXXXXXXX',
            'apiKey' = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
            );
        local('RAXKSKEY') = map('RAX-KSKEY:apiKeyCredentials' = #credentials);
        local('auth') = map('auth' = #RAXKSKEY);
        local('data') = encode_json(#auth);
    /private;
    // =================================================================
    // Rackspace API variables
    // configure according to:
    // http://docs.rackspace.com/files/api/v1/cf-devguide/content/Retrieving_Auth_Token.html
    local('auth_endpoint') = 'https://identity.api.rackspacecloud.com/v2.0/';
    
    // =================================================================
    // Internal Rackspace API variables are set within member methods.
    local('auth_token') = string;
    local('internalURL') = string;
    local('publicURL') = string;
    local('tenantId') = string;
    local('versionInfo') = string;
    local('versionList') = string;
    local('versionId') = string;
    
    // =================================================================
    // Internal utility variables are set within member methods.
    
    // #access_map instance variable stores access variables for making
    // requests to the Rackspace API.
    local('access_map') = map;
    
    // The response_type is set by appending an extension to the endpoint URL.
    // Default is JSON.
    // http://docs.rackspace.com/auth/api/v2.0/auth-client-devguide/content/Request_Response_Types-d1e149.html
    !local_defined('response_type') ? local('response_type') = 'json';
    
    // rackspace_cloudfiles->'fault_map' returns the fault element,
    // message, and code, if any, after calling the getAuthFaults method.
    local('fault_map') = map;
    
    // =================================================================
    // External facing Rackspace API variables can be set by the user
    // by including its parameter name in the type's instance, e.g.,
    // local('rax') = rackspace_cloudfiles(-container = 'mycontainer');
    !local_defined('container') ? local('container') = string;
    !local_defined('http_method') ? local('http_method') = string;
    !local_defined('query_params') ? local('query_params') = array;
    !local_defined('region') ? local('region') = string;
    !local_defined('request') ? local('request') = 'json';
    // service_type represents the service type in the Rackspace service catalog
    // http://docs.rackspace.com/auth/api/v2.0/auth-client-devguide/content/Service_Types-d1e265.html
    !local_defined('service_type') ? local('service_type') = string;
    
    // Set MIME headers to align with the request type.
    // TODO implement XML methods.
    local('mimeheaders') = array;
    if(#request == 'xml');
        #mimeheaders->insert('Content-type' = 'application/xml');
    else;
        #mimeheaders->insert('Content-type' = 'application/json');
    /if;
    
    // The onCreate method stores instance variables in the type's instance
    // or allows us to override the default instance variables
    // by passing in values as parameters.
    define_tag('onCreate',
        -optional='auth_endpoint',
        -optional='container',
        -optional='data',
        -optional='http_method',
        -optional='mimeheaders',
        -optional='query_params',
        -optional='region',
        -optional='request',
        -optional='response_type',
        -optional='service_type'
        );
        local_defined('auth_endpoint') ? self->'auth_endpoint' = #auth_endpoint;
        local_defined('container') ? self->'container' = #container;
        local_defined('data') ? self->'data' = #data;
        local_defined('http_method') ? self->'http_method' = #http_method;
        local_defined('mimeheaders') ? self->'mimeheaders' = #mimeheaders;
        local_defined('query_params') ? self->'query_params' = #query_params;
        local_defined('region') ? self->'region' = #region;
        local_defined('request') ? self->'request' = #request;
        // xml response_type not implemented
        fail_if(
            (local_defined('response_type')
                && local('response_type') != 'json'),
            -10000,
            ('"' + #response_type + '" response_type not implemented. Use "json".'));
        local_defined('response_type') ? self->'response_type' = string_lowercase(#response_type);
        local_defined('service_type') ? self->'service_type' = #service_type;
    /define_tag;
    
    define_tag('getAuthToken',
        -description='Make a request to the Rackspace API to obtain an authentication token. If the response is successful, then the instance variable ``region`` is set and the method ``setAccess`` is called with the result. If the response fails, then the instance variable ``fault_map`` is set and becomes available to the developer for troubleshooting.',
        -optional='response_type',
        -optional='service_type');
        
        // xml response_type not implemented
        fail_if(
            (local_defined('response_type')
                && local('response_type') != 'json'),
            -10000,
            ('"' + #response_type + '" response_type not implemented. Use "json".'));
        local_defined('response_type') ? self->'response_type' = #response_type;
        local_defined('service_type') ? self->'service_type' = #service_type;
        local('result') = string;
        local('fault_code') = string;
        
        #result = include_url(
            self->'auth_endpoint' + 'tokens' + '.' + self->'response_type',
            -postparams=self->'data',
            -sendmimeheaders=self->'mimeheaders');
        #fault_code = error_code;
        if(self->'response_type' != 'json');
            return('"' + #response_type + '" response_type not implemented. Use "json".');
        else;
            #result = decode_json(#result);
        /if;
        if(#fault_code == 0);
            return(self->setAccess(-result=#result, -region=self->'region'));
        else;
            self->getAuthFaults(#result);
        /if;
    /define_tag;
    
    define_tag('getAuthFaults',
        -description='Used internally by the method ``getAuthToken``. Parses the result from ``getAuthToken`` if the response fails. Sets an instance variable ``fault_map``, which is a map containing the element, message, and code of the HTTP response.

Currently only a JSON response type decode to a Lasso map object is supported. When the XML response type is implemented, then this method must be modified accordingly.',
        -required='result', -type='map');
        
        local('fault_element') = string;
        local('fault_message') = string;
        local('fault_code') = string;
        local('fault_map') = map;
        
        // reset the fault_map map
        self->'fault_map' = #fault_map;
        // build the fault_map map
        if(#result->keys->size == 1);
            #fault_element = #result->keys->get(1);
            #fault_message = #result->find(#fault_element)->find('message');
            #fault_code = #result->find(#fault_element)->find('code');
            #fault_map->insert('element' = #fault_element);
            #fault_map->insert('message' = #fault_message);
            #fault_map->insert('code' = #fault_code);
        /if;
        self->'fault_map' = #fault_map;
    /define_tag;
    
    define_tag('setAccess', -description="Used internally by the method ``getAuthToken``. Parses a successful ``getAuthToken`` response. Sets instance variables for making calls to a specific Rackspace service type's endpoints in a specific geographic region. The available instance variables depend on the service type. The currently implemented instance variables are ``auth_token`` (using the authentication token from the result of ``getAuthToken``), ``region`` (using the default region, none is provided), ``internalURL``, ``publicURL``, ``versionInfo``, ``versionList``, and ``versionId``.",
        -required='result');
        
        local('access_map') = map;
        
        // Set the authentication token from the result of getAuthToken.
        self->'auth_token' == '' ? self->'auth_token' = #result->find('access')->find('token')->find('id');
        
        // If the region is not set, then use the default region for the user.
        self->'region' == '' ? self->'region' = #result->find('access')->find('user')->find('RAX-AUTH:defaultRegion');
        
        iterate(#result->find('access')->find('serviceCatalog'),local('i'));
            if(#i->find('type') == self->'service_type');
                iterate(#i->find('endpoints'),local('e'));
                    if(#e->find('region') == self->'region');
                        #e->find('internalURL') != null ? self->'internalURL' = #e->find('internalURL')->replace('\\','')&;
                        #e->find('publicURL') != null ? self->'publicURL' = #e->find('publicURL')->replace('\\','')&;
                        self->'tenantId' = #e->find('tenantId');
                        #e->find('versionInfo') != null ? self->'versionInfo' = #e->find('versionInfo')->replace('\\','')&;
                        #e->find('versionList') != null ? self->'versionList' = #e->find('versionList')->replace('\\','')&;
                        self->'versionId' = #e->find('versionId');
                        loop_abort;
                    /if;
                /iterate;
                loop_abort;
            /if;
        /iterate;
    /define_tag;
    
    define_tag('getContainerDetailsListObjects',
        -description='Gets container details and list objects.',
        -optional='container', -type='string',
        -optional='query_params', -type='array',
        -optional='response_type', -type='string');
        // See documentation at:
        // http://docs.rackspace.com/files/api/v1/cf-devguide/content/GET_listcontainerobjects_v1__account___container__containerServicesOperations_d1e000.html
        // override instance variables
        local_defined('container') ? self->'container' = #container;
        local_defined('query_params') ? self->'query_params' = #query_params;
        // xml response_type not implemented
        fail_if(
            (local_defined('response_type')
                && local('response_type') != 'json'),
            -10000,
            ('"' + #response_type + '" response_type not implemented. Use "json".'));
        local_defined('response_type') ? self->'response_type' = string_lowercase(#response_type);
        
        self->'service_type' = 'object-store';
        self->getAuthToken(-response_type=self->'response_type');
        local('qp') = array('format' = self->'response_type');
        #qp->merge(self->'query_params');
        local('result');
        
        #result = include_url(
            self->'publicURL' + '/' + self->'container',
            -getparams=#qp,
            -sendmimeheaders=array('X-Auth-Token' = self->'auth_token')
        );
        if(error_code == '0');
            if(self->'response_type' == 'xml');
                // todo
                #result = 'XML response format type not implemented. Use JSON.';
            else;
                #result = decode_json(#result);
            /if;
        /if;
        return(#result);
    /define_tag;
    
    define_tag('getContainerCDNMetadata',
        -description='Gets metadata for a CDN-enabled container. Returns a map.',
        -optional='container', -type='string');
        // See documentation at:
        // http://docs.rackspace.com/files/api/v1/cf-devguide/content/HEAD_retrieveCDNcontainermeta_v1__account___container__CDN_Container_Services-d1e2632.html
        // override instance variables
        local_defined('container') ? self->'container' = #container;
        self->'service_type' = 'rax:object-cdn';
        
        self->getAuthToken();
        
        local('cmd') = string;
        local('result') = string;
        local('result_map') = map;
        
        #cmd->append('curl ');
        #cmd->append(self->'publicURL');
        #cmd->append('/');
        #cmd->append(self->'container');
        #cmd->append(' -I -H "X-Auth-Token: ');
        #cmd->append(self->'auth_token');
        #cmd->append('"');
        
        #result = shell(#cmd);
        #result->trim;
        #result = #result->split('\r\n');
        
        iterate(#result, local('i'));
            if(#i->beginswith('HTTP/1.1 20'));
                local('t') = #i->split(' ');
                #result_map->insert('http_version' = #t->get(1));
                #result_map->insert('http_response_code' = #t->get(2));
                local('rl') = #t->get(1) + ' ' #t->get(2) + ' ';
                #t=#t->join(' ');
                #t->removeleading(#rl);
                #result_map->insert('http_response_message' = #t);
            else;
                local('h') = #i->split(': ');
                #result_map->insert(#h->first = #h->second);
            /if;
        /iterate;
        return(#result_map);
    /define_tag;
/define_type;
]
