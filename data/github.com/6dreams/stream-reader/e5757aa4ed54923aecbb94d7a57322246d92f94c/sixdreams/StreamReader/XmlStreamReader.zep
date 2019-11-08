namespace SixDreams\StreamReader;

class XmlStreamReader implements StreamReaderInterface
{
    protected extractPath;
    protected collectPath;
    protected callback;

    protected optionsCallback;

    protected parser;

    protected currentPath;

    protected extracting;
    protected collecting;

    protected collected;
    protected collectedRef;

    protected lowerCase = false;

    public function parse(resource data, int buffer = 1024) -> bool
    {
        if (this->extractPath === null) {
            return false;
        }

        var chunk, eof, info;
        // \x78ml_parser_create
        let this->parser = xml_parser_create();

        xml_set_object(this->parser, this);
        xml_parser_set_option(this->parser, XML_OPTION_CASE_FOLDING, false);
        xml_parser_set_option(this->parser, XML_OPTION_SKIP_WHITE, true);
        var cb = this->optionsCallback;
        if cb !== null {
            {cb}(this->parser);
        }

        xml_set_element_handler(this->parser, "parseStart", "parseEnd");
        xml_set_character_data_handler(this->parser, "parseData");


        let this->collecting = false, this->extracting = false, this->currentPath = "";
        let this->collected = [], this->collectedRef = 0;

        let info = stream_get_meta_data(data);

        if info["seekable"] === true {
            fseek(data, 0);
        }

        let chunk = fread(data, buffer);
        while chunk !== false {
            let eof = feof(data);
            if (xml_parse(this->parser, chunk, eof) !== 1) {
                var code, line;
                let code = xml_get_error_code(this->parser), line = xml_get_current_line_number(this->parser);
                xml_parser_free(this->parser);

                throw new \Exception(sprintf("XML Parse Error: %d at line %d (chunk: %s)",code, line, chunk));
            }
            if (eof) {
                break;
            }
            let chunk = fread(data, buffer);
        }

        xml_parser_free(this->parser);

        return true;
    }

    public function registerCallback(string collectPath, string! extractPath, callable callback) -> <StreamReaderInterface>
    {
        let this->extractPath = strtolower(extractPath);
        if (collectPath === "") {
            let this->collectPath = this->extractPath;
        } else {
            let this->collectPath = strtolower(collectPath);
        }
        let this->callback = callback;
        if strpos(this->extractPath, this->collectPath) !== 0 {
            throw new \Exception("Path must extractPath must contain collectPath!");
        }

        return this;
    }

    public function setOptionCallbacks(callable optionsCallback) -> <StreamReaderInterface>
    {
        let this->optionsCallback = optionsCallback;

        return this;
    }

    public function setLowerCaseNames(bool state) -> <StreamReaderInterface>
    {
        let this->lowerCase = state;

        return this;
    }

    private function parseStart(parser, string name, array attributes) -> void
    {
        let this->currentPath = this->currentPath . "/" . strtolower(name);
        this->checkPath();

        if this->collecting || this->extracting {
            if this->extracting && !this->isExtract() {
                this->addData(this->buildElement([name, attributes, ""]));
                return;
            }
            this->addElement([name, attributes, ""]);
        }
    }

    private function addElement(array element) -> void
    {
        let this->collected[this->collectedRef] = element;
        let this->collectedRef++;
    }

    private function parseEnd(parser, string name) -> void
    {
        var extract = this->isExtract();
        if extract {
            var xml = "", value;
            for value in this->collected {
                let xml .= this->buildElement(value);
            }
            for value in reverse this->collected {
                let xml .= this->closeElement(value[0]);
            }
            this->fireCallback(xml);
        }
        if (this->collecting || this->extracting) {
            if this->extracting && !extract {
                this->addData(this->closeElement(name));
            } else {
                let this->collectedRef = this->collectedRef - 1;
                unset(this->collected[this->collectedRef]);
            }
        }

        let this->currentPath = substr(
            this->currentPath,
            0,
            strlen(this->currentPath) - (strlen(name) + 1)
        );

        this->checkPath();
    }

    private function parseData(parser, string data) -> void
    {
        if strlen(trim(data)) === 0 {
            return;
        }
        if !this->collecting && !this->extracting {
            return;
        }
        this->addData("<![CDATA[" . data . "]]>");
    }

    private function addData(string data) -> void
    {
        var ref;
        let ref = this->collectedRef - 1;
        let this->collected[ref][2] = this->collected[ref][2] . data;
    }

    private function checkPath() -> void
    {
        if this->collecting !== null {
            let this->collecting = strpos(this->currentPath, this->collectPath) === 0;
        }

        let this->extracting = strpos(this->currentPath, this->extractPath) === 0;
    }

    private function isExtract() -> bool
    {
        return this->currentPath === this->extractPath;
    }

    /**
     * Builds XML open element.
     *
     * @param array element
     *
     * @return string
     */
    private function buildElement(array element) -> string
    {
        var ret, k, v;
        let ret = "<" . (this->lowerCase ? strtolower(element[0]) : element[0]);
        for k, v in element[1] {
            let ret = ret . " " . (this->lowerCase ? strtolower(k) : k) . "=\"" . htmlentities(v, ENT_QUOTES, "UTF-8") . "\"";
        }

        return ret . ">" . element[2];
    }

    private function closeElement(string name) -> string
    {
        return "</" . (this->lowerCase ? strtolower(name) : name) . ">";
    }

    protected function fireCallback(string text) -> void
    {
        var cb = this->callback;
        {cb}(text);
    }
}