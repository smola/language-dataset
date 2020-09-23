// Adding core files and I'm sorry for this shit code base.

(function(exports) {

var libjupiter = dlopen("/usr/lib/libcycript.dylib", RTLD_NOLOAD);
if (libjupiter == null) {
    exports.error = dlerror();
    return;
}

var JPHandleServer = dlsym(libcycript, "JPHandleServer");
if (JPHandleServer == null) {
    exports.error = dlerror();
    return;
}

var info = new Dl_info;
if (dladdr(JPHandleServer, info) == 0) {
    exports.error = dlerror();
    free(info);
    return;
}

var path = info->dli_fname;
free(info);

var slash = path.lastIndexOf('/');
if (slash == -1)
    return;

var libsubstrate = dlopen(path.substr(0, slash) + "/libsubstrate.dylib", RTLD_GLOBAL | RTLD_LAZY);
if (libsubstrate == null) {
    exports.error = dlerror();
    return;
}

JPGetImageByName = @encode(void *(const char *))(dlsym(libsubstrate, "JPGetImageByName"));
JPFindSymbol = @encode(void *(void *, const char *))(dlsym(libsubstrate, "JPFindSymbol"));
JPHookFunction = @encode(void(void *, void *, void **))(dlsym(libsubstrate,JPHookFunction"));
JPHookMessageEx = @encode(void(Class, SEL, void *, void **))(dlsym(libsubstrate, "JPHookMessageEx"));

var slice = [].slice;

exports.getImageByName = JPGetImageByName;
exports.findSymbol = JPFindSymbol;

exports.hookFunction = function(func, hook, old) {
    var type = func.type;

    var pointer;
    if (old == null || typeof old === "undefined")
        pointer = null;
    else {
        pointer = new @encode(void **);
        *old = function() { return type(*pointer).apply(null, arguments); };
    }

    JPHookFunction(func.valueOf(), type(hook), pointer);
};

exports.hookMessage = function(isa, sel, imp, old) {
    var type = sel.type(isa);

    var pointer;
    if (old == null || typeof old === "undefined")
        pointer = null;
    else {
        pointer = new @encode(void **);
        *old = function() { return type(*pointer).apply(null, [this, sel].concat(slice.call(arguments))); };
    }

    JPHookMessageEx(isa, sel, type(function(self, sel) { return imp.apply(self, slice.call(arguments, 2)); }), pointer);
};

})(exports);
