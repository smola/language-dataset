define http_response => type {
    data
        private headerBytes::bytes,
        private protocol::string,
        private statusCode::integer,
        private statusMsg::string,
        private headers::map,
        private body::bytes


    public onCreate(response::bytes) => {
        local(break)   = #response->find('\r\n\r\n')

        .'headerBytes' = #response->sub(1, #break + 3)
        .'body'        = #response->sub(#break + 4)

        .populateHeaderInfo
    }

    // From curl->raw
    public onCreate(response::staticarray) => {
        local(header_data) = #response->get(2)
        
        // For some reason, it keeps all the intermediate headers
        // so remove all previous headers (such as auth chalenges)
        local(headers) = array
        local(break)
        while(#break := #header_data->find('\r\n\r\n')) => {
            #headers->insert(#header_data->sub(1, #break+3))
            #header_data = #header_data->sub(#break+4)
        }

        .`headerBytes` = #headers->last
        .'body'        = #response->get(3)

        .populateHeaderInfo
    }

    // Getters
    public 
        headerBytes => .`headerBytes`,
        protocol    => .`protocol`,
        statusCode  => .`statusCode`,
        statusMsg   => .`statusMsg`,
        headers     => .`headers`,
        body        => .`body`


    public headerString        => .'headerBytes'->exportAs(`ISO-8859-1`)
    public header(key::string) => .headers->find(#key)

    public bodyString(charset::string) => .'body'->exportAs(#charset)
    public bodyString => {
    // Code adapted from include_url
        // auto-discover charset
        local(charset) = string_findregexp(.headerString, -find='(?i)charset\\s*=\\s*([\\w\\-]+)')
        if(#charset->size >= 2) => {
            #charset = #charset->get(2)
        else
        // charset not found in headers, try meta on page
            #charset = string_findregexp(.'body', -find='(?i)charset\\s*=\\s*([\\w\\-]+)')
            #charset->size >= 2
                ? #charset = #charset->get(2)
                | #charset = 'utf-8'
        }
        #charset == 'ISO-8859-1'
            ? #charset = 'Windows-1252'

        return .bodyString(#charset)
    }


    private populateHeaderInfo => {
        local(tmp_headers) = .headerString->asCopy
        local(end_status)  = #tmp_headers->find('\r\n')
        local(message)     = #tmp_headers->sub(1, #end_status - 1)->split(' ')

        #tmp_headers
            ->remove(1, #end_status + 1)
            & replace('\r\n ' , ' ')
            & replace('\r\n\t', '\t')
            & removeTrailing('\r\n')

        .'headers' = map
        with header in #tmp_headers->split('\r\n')
        let key = #header->sub(1, #header->find(':') - 1)
        let val = #header->sub(#header->find(':') + 1)
        let cur = .'headers'->find(#key)
        do {

            #val->trim
            // Taking advantage of #cur being a references in the else clause
            #cur == void
                ? .'headers'->insert(#key=#val)
                | #cur->append(',' + #val)
        }

        .'protocol'   =  string(#message->get(1))
        .'statusCode' = integer(#message->get(2))
        .'statusMsg'  =  string(#message->get(3))
    }
}
