module wg

class Download
    var opt: String
    var n: Bool
    private var w: String = "wget -c --timeout=5"
    fun wget(s: String) do
        var cmd: String = "{w}"
        if not n and opt != "" then
            cmd = "{w} -O \"{opt}\" "
        end
        if s.has_prefix("https") then
            cmd = "{cmd} --no-check-certificate {s}"
        else if not s.has_prefix("http") then
            cmd = "{cmd} --no-check-certificate --user=enderseed --password=enderseed 'https://enderseed.buzz.seedhost.eu/enderseed/downloads/{s}'"
        else
            cmd = "{cmd} \"{s}\""
        end
        var sys = new Sys
        loop 
            print("downloading {s}... {cmd}")
            var status = sys.system(cmd)
            if status == 0 then 
                break
            else if status == 2 then
                exit status
            end
            print("retrying in 5 secs ({status})...")
            sys.system("sleep 5")
        end
    end
end

if args.length < 1 then
	print "Usage: wg <file or url(http/https)> [output|rarfiles number]"
    exit 1
end

var arg1 = args.shift
var arg2 = ""

if args.length > 0 then arg2 = args.shift

var n = ((arg2 != "") and (arg2.to_i > 0))
var d = new Download(arg2, n)

if not n then
    d.wget(arg1)
else 
    print("retrieving rar from {arg2} parts...")
    d.wget("{arg1}.rar")
    var l = arg2.to_s.length
    if l == 1 then l = 2
    var t = ""
    for i in [0..l[ do t += "0"
    for i in [0..arg2.to_i] do
        var s = "{t}{i}".substring_from(i.to_s.length)
        print("retrieving part {i}/{arg2}...")
        d.wget("{arg1}.r{s}")
    end
end

