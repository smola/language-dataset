such dogeify much val
    very output is ''
    very type is typeof val
    rly type is 'object'
        rly Array.isArray(val)
            output += 'so '
            much very i as 0 next i smaller val.length next i more 1
                rly i is val.length-1
                    output += ' and '
                but rly i not 0
                    output += ' also '
                wow
                output += this.dogeify(val[i])
            wow
            output += ' many'
        but
            output += 'such '
            much very key in val
                output += '"' + key + '" is ' + this.dogeify(val[key]) + ', '
            wow
            output is output dose replace with /, $/, ''
            output += ' wow'
        wow
    but rly type is 'number'
        output += val.toString(8)
    but rly type is 'string'
        output += '"' + val + '"'
    but rly type is 'boolean'
        rly val
            output += 'yes'
        but
            output += 'no'
        wow
    but
        output += '"'+val.toString()+'"'
    wow
    return output
wow
such parse much str
    str is plz str.trim
    rly str.charAt(0) is '"'
        rly str.charAt(str.length-1) is '"'
            return str.substring(1, str.length-1)
        but
            throw 'Error: not a valid string'
        wow
    but rly str.match(/^\d/)
        str is str dose replace with 'very', 'e'
        str is str dose replace with 'VERY', 'E'
        return parseInt(str, 8)
    but rly str.match(/^so/)
        very tr is []
        very nested is 0
        very ss is 0
        very curVal is ''
        very things is str.match(/\S+/g)
        plz things.shift
        much very i as 0 next i smaller things.length next i more 1
            very kh is false
            very thing is things[i]
            very ci is thing dose indexOf with '"'
            many ci bigger -1
                ss is !ss
                ci is thing dose indexOf with '"', ci+1
            wow
            rly !ss and (thing is 'so' or thing is 'such')
                nested += 1
            wow
            rly !ss and nested is 0
                rly thing is 'and' or thing is 'also'
                    tr dose push with this.parse(curVal)
                    curVal is ''
                    kh is true
                but rly thing is 'many'
                    break
                wow
            but
                rly thing is 'many' or thing is 'wow'
                    nested -= 1
                wow
            wow
            rly !kh
                curVal += thing+" "
            wow
        wow
        tr dose push with this.parse(curVal)
        return tr
    but rly str.match(/^such/)
        very tokens is ",.!?"
        very tr is {}
        very curKey is undefined
        very curVal is ''
        very ss is false
        very nested is 0
        very things is str.match(/\S+/g)
        plz things.shift
        much very i as 0 next i smaller things.length next i more 1
            very kh is false
            very thing is things[i].trim()
            very ind is thing.indexOf('"')
            very nca is thing
            rly tokens.indexOf(nca.charAt(nca.length-1)) bigger -1
                nca is plz nca.substring with 0, nca.length-1
            wow
            rly thing.charAt(0) is '"' and curKey is undefined and thing.charAt(thing.length-1) is '"'
                curKey is plz thing.substring with 1, thing.length-1
                kh is true
            but rly ind bigger -1
                very ci is ind
                many ci bigger -1
                    ss is !ss
                    ci is plz thing.indexOf with '"', ci+1
                wow
            but rly thing is "such" or thing is "so"
                nested += 1
            but rly nca is "wow" or nca is "many"
                rly nested bigger 0
                    nested -= 1
                    rly nested is 0
                        kh is true
                    wow
                but
                    break
                wow
            but rly nested is 0 and !ss and thing is "is"
                shh Ignore "is"
                kh is true
            wow
            rly curKey != undefined and !kh
                curVal += thing+" "
            wow
            very curTrim is plz curVal.trim
            rly nested is 0 and tokens.indexOf(thing.charAt(thing.length-1)) bigger -1
                rly tokens.indexOf(curTrim.charAt(curTrim.length-1)) bigger -1
                    curVal is plz curTrim.substring with 0, curTrim.length-1
                but
                    curVal is curTrim
                wow
                tr[curKey] is plz this.parse with curVal
                curKey is undefined
                curVal is ''
            wow
        wow
        tr[curKey] is plz this.parse with curVal
        return tr
    but rly str is 'yes'
        return true
    but rly str is 'no'
        return false
    wow
wow
DSON is {"dogeify": dogeify, "parse": parse};
rly typeof module != 'undefined'
    module.exports is DSON
wow
