namespace HtmlFilter\HtmlParser\Model;

class HtmlElement extends HtmlModelAbstract
{
    const IS_ALLOWED = "permission";
    const IS_EMPTY = "empty";
    const IS_NESTABLE = "nestable";
    const ELEMENT_ALLOWS_TO_NEST = "nests";
    const ELEMENT_MAY_NEST_IN = "resides";

    /**
     * List of valid html tags
     * @var array validHtmlTags
     * @todo list is incomplete, quite a few tags missing
     */
    protected validHtmlElements = [
        "span": ["permission" : 1, "nestable": 1],
        "div": ["permission": 1, "nestable": 1], 
        "iframe": ["permission": 1, "nestable": 1], 
        "p": ["permission": 1, "nestable": 1],
        "strong": ["permission": 1, "nestable": 1],
        "applet": ["permission": 1, "nestable": 1],
        "video": ["permission": 1, "nestable": 1],
        "noscript": ["permission": 1, "nestable": 1],
        "form": ["permission": 1, "nestable": 1],
        "button": ["permission": 1, "nestable": 1],
        "a": ["permission": 1, "nestable": 1],
        "del": ["permission": 1, "nestable": 1],
        "dd": ["permission": 1, "nestable": 1],
        "fieldset": ["permission": 1, "nestable": 1],
        "iframe": ["permission": 1, "nestable": 1],
        "ins": ["permission": 1, "nestable": 1],
        "li": ["permission": 1, "nestable": 1],
        "object": ["permission": 1, "nestable": 1],
        "td": ["permission": 1, "nestable": 1],
        "th": ["permission": 1, "nestable": 1],
        "abbr": ["permission": 1, "nestable": 1],
        "acronym": ["permission": 1, "nestable": 1],
        "address": ["permission": 1, "nestable": 1],
        "b": ["permission": 1, "nestable": 1], 
        "bdo": ["permission": 1, "nestable": 1],
        "big": ["permission": 1, "nestable": 1], 
        "caption": ["permission": 1, "nestable": 1],
        "cite": ["permission": 1, "nestable": 1],
        "code": ["permission": 1, "nestable": 1],
        "dfn": ["permission": 1, "nestable": 1],
        "dt": ["permission": 1, "nestable": 1],
        "em": ["permission": 1, "nestable": 1],
        "font": ["permission": 1, "nestable": 1],
        "h1": ["permission": 1, "nestable": 1],
        "h2": ["permission": 1, "nestable": 1],
        "h3": ["permission": 1, "nestable": 1],
        "h4": ["permission": 1, "nestable": 1],
        "h5": ["permission": 1, "nestable": 1],
        "h6": ["permission": 1, "nestable": 1],
        "i": ["permission": 1, "nestable": 1],
        "kbd": ["permission": 1, "nestable": 1],
        "label": ["permission": 1, "nestable": 1],
        "legend": ["permission": 1, "nestable": 1],
        "pre": ["permission": 1, "nestable": 1],
        "q": ["permission": 1, "nestable": 1],
        "rb": ["permission": 1, "nestable": 1],
        "rt": ["permission": 1, "nestable": 1],
        "s": ["permission": 1, "nestable": 1],
        "samp": ["permission": 1, "nestable": 1],
        "small": ["permission": 1, "nestable": 1],
        "span": ["permission": 1, "nestable": 1],
        "strike": ["permission": 1, "nestable": 1],
        "sub": ["permission": 1, "nestable": 1],
        "sup": ["permission": 1, "nestable": 1],
        "tt": ["permission": 1, "nestable": 1],
        "script": ["permission": 0, "nestable": 1],
        "u": ["permission": 1, "nestable": 1],
        "var": ["permission": 1, "nestable": 1],
        "blockquote": ["permission": 1, "nestable": 1],
        "map": ["permission": 1, "nestable": 1],
        "table": ["permission": 1, "nestable": 1, "nests":["thead","tbody","tfooter","tr"]],
        "tr": ["permission": 1, "nestable": 1, "nests":["td","th"], "resides":["thead","tbody","tfooter","table"]],
        "thead": ["permission": 1, "nestable": 1, "nests":["tr"], "resides":["table"]],
        "tbody": ["permission": 1, "nestable": 1, "nests":["tr"], "resides":["table"]],
        "tfooter": ["permission": 1, "nestable": 1, "nests":["tr"], "resides":["table"]],
        "td": ["permission": 1, "nestable": 1, "resides":["tr"]],
        "th": ["permission": 1, "nestable": 1, "resides":["tr"]],
        "area": ["empty": 1, "permission": 1, "nestable": 0],
        "br": ["empty": 1, "permission": 1, "nestable": 0],
        "col": ["empty": 1, "permission": 1, "nestable": 0],
        "embed": ["empty": 1, "permission": 1, "nestable": 0],
        "hr": ["empty": 1, "permission": 1, "nestable": 0],
        "img": ["empty": 1, "permission": 1, "nestable": 0],
        "input": ["empty": 1, "permission": 1, "nestable": 0],
        "isindex": ["empty": 1, "permission": 1, "nestable": 0],
        "param": ["empty": 1, "permission": 1, "nestable": 0]
    ] {
        get, set
    };

    /**
     * Valid types of configurations allowed
     * {@inheritdoc}
     */
    protected validConfigurations = [
        self::IS_EMPTY,
        self::IS_ALLOWED,
        self::IS_NESTABLE,
        self::ELEMENT_ALLOWS_TO_NEST,
        self::ELEMENT_MAY_NEST_IN
    ];

    /**
     * Returns a the rule value for a specific rule and element
     *
     * @param string element
     * @param string rule
     *
     * @return mixed
     */
    public function getElementRule(string element, string rule)
    {
        if isset(this->validHtmlElements[strtolower(element)][rule]) {
            return this->validHtmlElements[strtolower(element)][rule];
        }

        return false;
    }

    /**
     * Verifies is a tag is in the whitelist of available tags
     * @param string tag
     *
     * @return boolean
     */
    public function isTagValid(const string tag) -> boolean
    {
        return isset(this->validHtmlElements[strtolower(tag)]);
    }

    /**
     * asserts if the element do not need/support a close tag
     *
     * @return boolean
     */
    public function isEmptyElement(string tag) -> boolean
    {
        return this->getElementRule(tag, self::IS_EMPTY) == 1;
    }

    /**
     * asserts if the element has close tag
     *
     * @return boolean
     */
    public function isElementAllowed(string tag) -> boolean
    {
        return this->getElementRule(tag, self::IS_ALLOWED) == 1;
    }

    /**
     * asserts if the element can be nested
     *
     * @return boolean
     */
    public function isElementNestable(string element) -> boolean
    {
        var nestable;
        let nestable = this->getElementRule(element, self::IS_NESTABLE) == 1;

        return nestable;
    }

    /**
     * Asserts the parent element can hold this specific child
     * (or any for that matter)
     *
     * @param string element
     * @param string parentElement
     *
     * @return boolean
     */
    public function isParentNestable(string element, string parentElement)
    {
        var nestable;
        let nestable = this->getElementRule(parentElement, self::IS_NESTABLE) == 1;

        if nestable
            && parentElement
            && isset(this->validHtmlElements[strtolower(parentElement)][self::ELEMENT_ALLOWS_TO_NEST])
        {
            let nestable = in_array(
                strtolower(element),
                this->validHtmlElements[strtolower(parentElement)][self::ELEMENT_ALLOWS_TO_NEST]
            );
        }

        if nestable
            && parentElement
            && isset(this->validHtmlElements[strtolower(element)][self::ELEMENT_MAY_NEST_IN])
        {
            let nestable = in_array(
                strtolower(parentElement),
                this->validHtmlElements[strtolower(element)][self::ELEMENT_MAY_NEST_IN]
            );
        }


        return nestable;
    }

    /**
     * Adds a new html tag to the valid element tags, or configure an existing one
     * Note: if permission is not set, will automatically be set to 1
     *
     * @param string tagName
     * @param array config
     *
     * @return boolean
     */
    public function addHtmlElement(string tagName, config=[]) -> boolean
    {
        if !this->validateConfiguration(config) {
            return false;
        }

        var value, key, element;
        let element = strtolower(tagName);
        for key, value in config {
            let this->validHtmlElements[element][key] = value;
        }

        if !isset(this->validHtmlElements[element][self::IS_ALLOWED]) {
            let this->validHtmlElements[element][self::IS_ALLOWED] = 1;
            let this->validHtmlElements[element][self::IS_NESTABLE] = 1;
        }

        return true;
    }
}