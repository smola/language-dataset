(function(utils) {

    utils.constants = {
        APPID:  	 NSBundle.mainBundle.bundleIdentifier, //id
        APPPATH:     NSBundle.mainBundle.bundlePath,  //资源路径
        APPHOME:	 NSHomeDirectory(), //沙盒
        APPDOC:      NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES)[0],
        APPLIBRARY:  NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask, YES)[0],
        APPCACHE:    NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES)[0]
    };

    utils.pviews = function(){
        return UIApp.keyWindow.recursiveDescription().toString(); //打印视图层次
    };

    utils.pvcs = function(){ //打印当前控制器
        return UIWindow.keyWindow().rootViewController._printHierarchy().toString();
    };

    utils.rp = function(target){//打印响应者 nextResponder
        var result = "" + target.toString();
        while(target.nextResponder){
            result += "\n" + target.nextResponder.toString();
            target = target.nextResponder;
        }
        return result;
    };

      utils.pactions = function(target){ //打印actionsForTarget;
        var result = '';
        var objs = target.allTargets.allObjects();
        for(var i = 0; i < objs.length; i++){
            var actions = [target actionsForTarget:objs[i] forControlEvent:0];
            result += objs[i] + " " + [actions componentsJoinedByString:@","];
        }
        return result;
    }


    utils.loadFramework = function (target) { //加载资源路径
        var h="/System/Library/",t="Frameworks/"+target+".framework";
        return [[NSBundle bundleWithPath:h+t]||
        [NSBundle bundleWithPath:h+"Private"+t] load];
    }


    utils.tryPrintIvars = function tryPrintIvars(a){ //打印属性 或者*实例对象
        var x={}; 
        for(i in *a)
            { 
                try{ x[i] = (*a)[i]; } catch(e){} 
            } 
        return x; 
        } 


    utils.printMethods = function printMethods(className, isa) { //打印方法,第一个传类对象字符串,第二个可不传。
        var count = new new Type("I");
        var classObj = (isa != undefined) ? objc_getClass(className)->isa :     
        objc_getClass(className); 
        var methods = class_copyMethodList(classObj, count); 
        var methodsArray = [];
        for(var i = 0; i < *count; i++) { 
            var method = methods[i]; 
            methodsArray.push({selector:method_getName(method),     
            implementation:method_getImplementation(method)});
    }
        free(methods); 
        return methodsArray;
}


  

    for(var k in utils.constants) { //引入时打印对象变量
        Cycript.all[k] = utils.constants[k];
    }

    for(var k in utils) {//引入时打印对象方法
        if(utils.hasOwnProperty(k)) {
            var f = utils[k];
            if(typeof f === 'function') {
                Cycript.all[k] = f;
            }
        }
    }
})(exports);
