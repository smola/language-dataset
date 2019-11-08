namespace Aimo;
/**
 * Aimo\Model
 *
 * 模型基类
 */
class Model
{
    /**
     *@var string 模型名
     */
    protected modelName = "";

    /**
     *@var string 数据表名
     */
    protected table     = "";

    /**
     *@var array primary 模型主键
     */
    protected primary   = [];

    /**
     *@var array validateRules 验证规则
     */
    protected validateRules = [];

    /**
     *@var primary 模型主键
     */
    protected fields    = [];

    /**
     *@var array 数据
     */
    protected _data     = [];


    /**
     *@var array 更新的数据
     */
    protected _dirty     = [];
    /**
     *@var array 数据集
     */
    protected _rows     = [];

    /**
     *@var int 索引
     */
    protected _index    = 0;

    /**
     *@var array 模型错误信息
     */
    protected _error    = [];

    /**
     * Aimo\Model
     *
     * 模型基类
     *
     *<code>
     * use Aimo\Model;
     * class User extends Model {
     *     protected $table = 'user';
     *     protected $primary = ['uid'];
     *     protected $fields  = [
     *         'uid','username','password',
     *         'groupid','regtime'
     *     ];
     *     protected $validateRules = [
     *          'rules'  => [
     *              'comid'   => 'require|number',
     *              'uid'     => 'require|number',
     *              'content' => 'require',
     *              'time'    => 'require|number'
     *          ],
     *          'msg'   => [
     *              'comid.require'   => '必须指定企业ID',
     *              'comid.number'    => '必须指定企业ID',
     *              'uid.require'     => '登陆后操作',
     *              'uid.number'      => '登陆后操作',
     *              'content.require' => '笔记内容必须填写',
     *              'time.require'    => '记录时间必须设置',
     *              'time.number'     => '记录时间必须设置',
     *          ],
     *          'scene' => [
     *              'create' =>  ['comid','uid','content','time'],
     *              'update' =>  ['content','time']
     *          ]
     *     ];
     *     //设置器 输入的数据将经过此函数处理，函数名为 set字段名首字母大写
     *     protected function setPassword($value)
     *     {
     *         return md5($value);
     *     }
     *     //获取器 输出此字段之前数据将经过此函数处理，函数名为 get字段名首字母大写
     *     protected function getRegtime($value)
     *     {
     *         return date('Y-m-d H:i:s',$value);
     *     }
     * }
     * $user = new User();
     * $user->username = 'eric';
     * $user->password = '123456';
     * $user->save();
     *</code>
     */
    public function __construct()
    {
    }

    /**
     * 判断数据状态是新增还是更新
     *
     * @return boolean
     */
    protected function isNew()->boolean
    {
        var pks,pk;
        let pks = this->getPk();
        for pk in pks {
            if isset this->_data[pk] && !empty this->_data[pk] {
                return false;
            }
        }
        return true;
    }

    /**
     * 魔术方法设置模型数据
     *
     * <code>
     * $model->name = 'error';
     * //如果模型中定义了设置器
     * //数据将会先经过设置器处理
     * </code>
     *
     * @param string name
     * @param mixed value
     * @return void
     */
    public function __set(string! name, value) -> void
    {
        var method;
        if !property_exists(this,name) {
            let method = "set".ucfirst(name);
            if method_exists(this, method) {
                if isset this->_data[name] {
                    let this->_dirty[name] = this->{method}(value);
                }else{
                    let this->_data[name] = this->{method}(value);
                }
            }else{
                if isset this->_data[name] {
                    let this->_dirty[name]=value;
                }else{
                    let this->_data[name]=value;
                }
            }
        }
    }

    /**
     * 魔术方法获取模型数据
     *
     * <code>
     * echo $model->name;
     * //如果模型中定义了获取器
     * //数据将会先经过获取器处理
     * </code>
     *
     * @param string name
     * @return mixed
     */
    public function __get(string! name)
    {
        var method,value;
        if !property_exists(this, name){
            let method = "get".ucfirst(name);
            let value  = (isset this->_dirty[name]) ?
                    this->_dirty[name] : ( isset this->_data[name] ?
                    this->_data[name]  : null );
            if method_exists(this, method) {
                return this->{method}(value);
            }else{
                return value;
            }
        }
        return null;
    }

    public function __isset(string! name)->boolean
    {
        return isset this->_data[name];
    }

    /**
     * 获取模型原始数据
     *
     * 当需要获取定义了获取器的字段的原始值时，使用此方法
     *
     * <code>
     * echo $model->attr('time');
     * </code>
     *
     * @param string name
     * @return mixed
     */
    public function attr(string! name)
    {
        return isset this->_data[name] ? this->_data[name] : null;
    }

    /**
     * toString
     */
    public function __toString()
    {
        return get_called_class();
    }

    /**
     * 获取模型数据
     */
    public function toArray()->array
    {
        if (typeof this->_data=="array") && !empty this->_data {
            return (array) this->_data;
        }else{
            return [];
        }
    }

    /**
     * 获取模型验证的错误信息
     *
     * <code>
     * $user = new User();
     * $user->name = 'name';
     * if(!$user->save()){
     *     echo $user->getError();
     * }
     * </code>
     */
    public function getError()
    {
        return this->_error;
    }

    /**
     * 获取定义在模型中的主键
     *
     * 注意本函数暂时不会自动探测各种数据库的主键
     */
    public function getPk()
    {
        return this->primary;
    }

    /**
     * 模型验证
     *
     *<code>
     * //本验证参考了ThinkPHP5 以及 Laravel 的形式
     * //支持的验证规则：
     * $validateRules = [
     *     'passwd'  => 'require',
     *     'age'     => 'number',
     *     'height'  => 'integer',//alias int
     *     'weight'  => 'float',
     *     'email'   => 'email',
     *     'gender'  => 'alpha', //a-z
     *     'id'      => 'alphaNum', //a-z0-9
     *     'uname'   => 'alphaDash', //a-z0-9_
     *     'mobile'  => 'unique', //数据表中不允许重复
     *     'field10' => 'require|in:1,2,3',
     *     'arr10' => ['require',['in' => [1,2,3]]],
     *     'field11' => 'notin:1,2,3',
     *     'arr11' => [['notin' => [1,2,3]]],
     *     'full'    => 'between:1,100', //a-z
     *     'score'   => 'notBetween:0,60', //a-z
     *     'idcard'  => 'length:15,18', //'length:6' 也可以固定长度
     *     'eyes'    => 'max:2',
     *     'count'   => 'min:1',
     *     'birth'   => 'before:1990-01-01',//1990年以前出生
     *     'date'    => 'after:2017-01-01',//2017年以后的日期
     *     'insvr'   => 'expire:2010-01-01,2012-01-01',//在一个有效的时间段内
     *     'repass'  => 'confirm:passwd',//与当前验证中的passwd字段比较是否相等
     *     'phone'   => 'regex:^0\d{2,3}\-\d{7,8}$',//固话正则表达式
     *     //其他形式
     *     'allowedage' => 'require|int|between:16,99',
     *     'allowedage2' => ['require','int',['between'=>[16,99]]],//与上一句等效
     *     'characters'  => ['require',['regex'=>'^[a-z0-9:|\,\']+$']]
     * ];
     *</code>
     */
    public function validate()
    {
        if !isset this->validateRules {
            return true;
        }
        if empty this->validateRules {
            return true;
        }
        if !isset this->validateRules["rules"] {
            throw "error";
        }
        var messages,operate,key,value,validateFields,field,pks;
        var ruleString,rules,rule,v,c,kk,sc,tmp,vk;
        var ruleName,params,temp,length,minlength,maxlength,valid,begin,end;
        array cond;
        let messages = isset this->validateRules["msg"] ? this->validateRules["msg"] : [];
        let operate  = "update";
        let pks = this->getPk();
        if empty pks {
            throw "error";
        }
        for key in pks {
            let value = "";
            if isset this->_data[key] {
                let value = this->_data[key];
            }
            if empty value {
                let operate = "create";
                break;
            }
        }
        if isset this->validateRules["scene"] {
            if empty this->validateRules["scene"][operate] {
                let validateFields = array_keys(this->validateRules["rules"]);
            }else{
                let validateFields = this->validateRules["scene"][operate];
            }
        }else{
            let validateFields = array_keys(this->validateRules["rules"]);
        }
        let tmp = (operate == "create") ? this->_data : this->_dirty;
        for field in validateFields {
            let ruleString = isset this->validateRules["rules"][field] ? this->validateRules["rules"][field] : "";
            if empty ruleString {
                continue;
            }
            //'field' => 'require|regex:^(a|b)$'
            //TODO:正则表达式验证器中如果存在“|”字符将导致错误，稍后解决
            //'field' => ['require','regex:^(a|b)'],
            let rules = (typeof ruleString == "array") ? ruleString : explode("|", ruleString);
            let v = isset tmp[field] ? tmp[field] : null;
            for rule in rules {
                let vk = field.".".rule;
                if(typeof rule == "string"){
                    let sc = strpos(rule, ":");
                    if sc === false {
                        switch rule {
                            case "require":
                                if empty v && v!==0 && v!=="0" {
                                    let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                    return false;
                                }
                                break;
                            case "number":
                                if !is_numeric(v) {
                                    let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                    return false;
                                }
                                break;
                            case "integer":
                            case "int":
                                if (typeof v != "integer"){
                                    let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                    return false;
                                }
                                break;
                            case "float":
                                if !is_float(v) {
                                    let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                    return false;
                                }
                                break;
                            case "email":
                                if !filter_var(v, FILTER_VALIDATE_EMAIL) {
                                    let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                    return false;
                                }
                                break;
                            case "alpha":
                                if !preg_match("/^[a-z]+$/i",v) {
                                    let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                    return false;
                                }
                                break;
                            case "alphaNum":
                                if !preg_match("/^[a-z0-9]+$/i",v) {
                                    let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                    return false;
                                }
                                break;
                            case "alphaDash":
                                if !preg_match("/^[a-z0-9_]+$/i",v) {
                                    let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                    return false;
                                }
                                break;
                            case "unique":
                                if operate == "create" {
                                    let c = self::where(field,v)->count();
                                    if c > 0 {
                                        let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                        return false;
                                    }
                                } elseif operate == "update" {
                                    let cond = [field:v];
                                    for kk in pks {
                                        let cond[kk]=["<>", tmp[kk]];
                                    }
                                    let c = self::where(cond)->count();
                                    if c > 0 {
                                        let this->_error[]=isset messages[vk] ? messages[vk] : vk;
                                        return false;
                                    }
                                }
                                break;
                        }
                    }else{
                        let ruleName = substr(rule, 0, sc);
                        let temp     = substr(rule, sc+1);
                        if ruleName == "regex" {
                            let params = [temp];
                        } else {
                            let params = explode(",", temp);
                        }
                        let vk = field.".".ruleName;
                        switch ruleName
                        {
                            case "in":
                                if !in_array(v, params) {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                            case "notin":
                                if in_array(v, params) {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                            case "between":
                                if v < params[0] || v > params[1] {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                            case "notBetween":
                                if v >= params[0] && v <= params[1] {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                            case "length":
                                let length = strlen(v);
                                if count(params) == 1 {
                                    if length != intval(params[0]) {
                                        let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                        return false;
                                    }
                                }else{
                                    let minlength = intval(params[0]);
                                    let maxlength = intval(params[1]);
                                    if length < minlength || length > maxlength {
                                        let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                        return false;
                                    }
                                }
                                break;
                            case "max":
                                if v > params[0] {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                            case "min":
                                if v < params[0]{
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                            case "after":
                                let v = strtotime(v);
                                let valid = strtotime(params[0]);
                                if v < valid {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                            case "before":
                                let v = strtotime(v);
                                let valid = strtotime(params[0]);
                                if v > valid {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                            case "expire":
                                let v = strtotime(v);
                                let begin = strtotime(params[0]);
                                let end   = strtotime(params[1]);
                                if v < begin || v > end {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                            case "confirm":
                                let kk = params[0];
                                if isset tmp[kk] {
                                    if v != tmp[kk]{
                                        let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                        return false;
                                    }
                                }
                                break;
                            case "regex":
                                if !preg_match(params[0], v){
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                                break;
                        }
                    }
                }else{
                    let ruleName = reset(array_keys(rule));
                    let temp     = reset(array_values(rule));
                    if ruleName == "regex" {
                        let params = [temp];
                    } else {
                        let params = (typeof temp == "array") ? temp : explode(",", temp);
                    }
                    let vk = field.".".ruleName;
                    switch ruleName
                    {
                        case "in":
                            if !in_array(v, params) {
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                        case "notin":
                            if in_array(v, params) {
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                        case "between":
                            if v < params[0] || v > params[1] {
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                        case "notBetween":
                            if v >= params[0] && v <= params[1] {
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                        case "length":
                            let length = strlen(v);
                            if count(params) == 1 {
                                if length != intval(params[0]) {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                            }else{
                                let minlength = intval(params[0]);
                                let maxlength = intval(params[1]);
                                if length < minlength || length > maxlength {
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                            }
                            break;
                        case "max":
                            if v > params[0] {
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                        case "min":
                            if v < params[0]{
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                        case "after":
                            let v = strtotime(v);
                            let valid = strtotime(params[0]);
                            if v < valid {
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                        case "before":
                            let v = strtotime(v);
                            let valid = strtotime(params[0]);
                            if v > valid {
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                        case "expire":
                            let v = strtotime(v);
                            let begin = strtotime(params[0]);
                            let end   = strtotime(params[1]);
                            if v < begin || v > end {
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                        case "confirm":
                            let kk = params[0];
                            if isset tmp[kk] {
                                if v != tmp[kk]{
                                    let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                    return false;
                                }
                            }
                            break;
                        case "regex":
                            if !preg_match(params[0], v){
                                let this->_error[]=isset messages[vk] ? messages[vk]:vk;
                                return false;
                            }
                            break;
                    }
                }
            }
        }
        return true;
    }

    /**
     * 判断模型数据是否通过验证器验证
     */
    public function isValid()
    {
        return this->validate();
    }

    public function where(var a,var n=null,var c=null)
    {

    }

    /**
     * 新增模型数据
     *
     *<code>
     *$user = new User();
     *$user->username = 'eric';
     *$user->password = '123456';
     *$user->save();
     *</code>
     *
     * @access public
     * @return void
     */
    public function save()
    {
        
    }

    /**
     * 更新模型数据
     *
     *<code>
     *$user = User::get(6);
     *$user->lastlogin = time();
     *$user->update();
     *</code>
     *
     * @access public
     */
    public function update()
    {
        
    }

    /**
     * 软删除 soft delete
     *
     *<code>
     *$user = User::get(6);
     *$user->delete();
     *</code>
     *
     * @access public
     */
    public function delete(string! field="status",value=-1)
    {
        
    }

    /**
     * 硬删除/彻底删除
     *
     *<code>
     *$user = User::get(6);
     *$user->destroy();
     *</code>
     *
     * @access public
     */
    public function destroy()
    {
        
    }
}
