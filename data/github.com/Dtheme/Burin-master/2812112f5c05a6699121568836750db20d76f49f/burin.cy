(function(exports){

	//帮助文档
	help = function(){
		return {
			"指令：功能",
			"BRBundleId" : "当前调试的APP的bundle ID",
			"BRMainBundlePath" : "",
			"BRDocumentPath":"",
			"BRCachesPath":"",
			"redColor('view')":"调试：view为red",
			"blackColor('view')":"调试：view为black",
			"whiteColor('view')":"调试：view为white",
			"hiddenOrNot(view,willHidden)":"是否隐藏view",
			"CGPointMake(x,y)":"构造一个CGPoint",
			"CGSizeMake(w,h)":"构造一个CGSize",
			"CGRect(x,y,w,h)":"构造一个CGRect",
			"BRKeyWindow":"打印根控制器",
			"BRFrontVc":"打印当前控制器",
			"BRSubViewcontrollers":"打印控制器层级结构",
			"BRSubclasses(vc)":"打印vc子类",
			"GetMethods":"查看所有的方法",
			"getMethodNames":"获取所有方法名",
			"NSLog":"正常使用NSLog",
			"BRRunLoopDescrip":"打印当前runloop信息",
			"BRInstanceMethodNames":"查看所有实例方法",
			"BRClassMethods","查看所有类方法",
			"BRClassMethodNames","查看所有类方法名",
			"BRIvars":"查看所有实例变量",
			"BRIvarNames":"查看所有实例变量名"
		}
	}


    //当前调试的APP的bundle ID
    BRBundleId = [NSBundle mainBundle].bundleIdentifier;

    // 沙盒路径
	BRMainBundlePath = [NSBundle mainBundle].bundlePath;

	// 沙盒 document 
	BRDocumentPath = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES)[0];

	// 沙盒 caches 
	BRCachesPath = NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES)[0]; 

    //调试用的颜色
    redColor = function(view)){
        return view.backgroundColor = [UIColor redColor];
    }

    blackColor = function(view){
        return view.backgroundColor = [UIColor blackColor];
    }

    whiteColor = (view){
        return view.backgroundColor = [UIColor whiteColor];
    }
    
    //隐藏显示 willhidden传 YES/NO （oc语法）
    hiddenOrNot = function(view,willHidden){
        return view.hidden = willHidden;
    }

    // CGPoint、CGRect
	CGPointMake = function(x, y) { 
		return {0 : x, 1 : y}; 
	};

	CGSizeMake = function(w, h) { 
		return {0 : w, 1 : h}; 
	};

	CGRectMake = function(x, y, w, h) { 
		return {0 : CGPointMake(x, y), 1 : CGSizeMake(w, h)}; 
    };
    
	// 根控制器
    BRKeyWindow = function() {
		return UIApp.keyWindow;
    };
    
	// 当前显示的控制器
	var _FrontVc = function(vc) {
		if (vc.presentedViewController) {
        	return _FrontVc(vc.presentedViewController);
	    }else if ([vc isKindOfClass:[UITabBarController class]]) {
	        return _FrontVc(vc.selectedViewController);
	    } else if ([vc isKindOfClass:[UINavigationController class]]) {
	        return _FrontVc(vc.visibleViewController);
	    } else {
	    	var count = vc.childViewControllers.count;
    		for (var i = count - 1; i >= 0; i--) {
    			var childVc = vc.childViewControllers[i];
    			if (childVc && childVc.view.window) {
    				vc = _FrontVc(childVc);
    				break;
    			}
    		}
	        return vc;
    	}
    };
    BRFrontVc = function() {
		return _FrontVc(UIApp.keyWindow.rootViewController);
    };
   
    //控制器层级结构
    BRSubViewcontrollers = function(vc) { 
		if (![vc isKindOfClass:[UIViewController class]]) throw new Error(invalidParamStr);
		return vc.view.recursiveDescription().toString(); 
    };
    
	
    //view的层级结构
	BRSubviews = function(view) { 
		if (![view isKindOfClass:[UIView class]]) throw new Error(invalidParamStr);
		return view.recursiveDescription().toString(); 
    };
    
    var ClassInfo = function(className) {
		if (!className) throw new Error(missingParamStr);
		if (MJIsString(className)) {
			return NSClassFromString(className);
		} 
		if (!className) throw new Error(invalidParamStr);
		// 对象或者类
		return className.class();
	};

	// 查看所有的子类
	BRSubclasses = function(className, reg) {
		className = ClassInfo(className);

		return [c for each (c in ObjectiveC.classes) 
		if (c != className 
			&& class_getSuperclass(c) 
			&& [c isSubclassOfClass:className] 
			&& (!reg || reg.test(c)))
			];
	};

	// 查看所有的方法
	var GetMethods = function(className, reg, clazz) {
		className = ClassInfo(className);

		var count = new new Type('I');
		var classObj = clazz ? className.constructor : className;
		var methodList = class_copyMethodList(classObj, count);
		var methodsArray = [];
		var methodNamesArray = [];
		for(var i = 0; i < *count; i++) {
			var method = methodList[i];
			var selector = method_getName(method);
			var name = sel_getName(selector);
			if (reg && !reg.test(name)) continue;
			methodsArray.push({
				selector : selector, 
				type : method_getTypeEncoding(method)
			});
			methodNamesArray.push(name);
		}
		free(methodList);
		return [methodsArray, methodNamesArray];
	};

	var MethodsInfo = function(className, reg, clazz) {
		return GetMethods(className, reg, clazz)[0];
	};

	// 查看方法名
	var getMethodNames = function(className, reg, clazz) {
		return GetMethods(className, reg, clazz)[1];
	};

	// 查看实例方法信息
	BRInstanceMethods = function(className, reg) {
		return MethodsInfo(className, reg);
	};

	// 查看实例方法名字
	BRInstanceMethodNames = function(className, reg) {
		return getMethodNames(className, reg);
	};

	// 查看所有的类方法
	BRClassMethods = function(className, reg) {
		return MethodsInfo(className, reg, true);
	};

	// 查看所有的类方法名字
	BRClassMethodNames = function(className, reg) {
		return getMethodNames(className, reg, true);
	};

	// 查看所有的成员变量
	BRIvars = function(obj, reg){ 
		if (!obj) throw new Error(missingParamStr);
		var x = {}; 
		for(var i in *obj) { 
			try { 
				var value = (*obj)[i];
				if (reg && !reg.test(i) && !reg.test(value)) continue;
				x[i] = value; 
			} catch(e){} 
		} 
		return x; 
	};

	// 查看所有的成员变量名字
	BRIvarNames = function(obj, reg) {
		if (!obj) throw new Error(missingParamStr);
		var array = [];
		for(var name in *obj) { 
			if (reg && !reg.test(name)) continue;
			array.push(name);
		}
		return array;
	};

	// 在控制台使用log eg：NSLog("value: %@", value)
	NSLog = function(){
	 	var types = 'v', 
		args = [], 
		count = arguments.length; 
		for (var i = 0; i != count; ++i) {
			 types += '@'; args.push(arguments[i]); 
		}
		new Functor(NSLog_, types).apply(null, args); 
	}
	NSLog_ = dlsym(RTLD_DEFAULT, "NSLog")

	//打印当前runloop信息
	BRRunLoopDescrip = NSRunLoop.messages['description']
	NSRunLoop.messages[‘description’] = function() {
		 return original_NSRunLoop_description.call(this).toString().substr(0, 80)+”, etc.”; 
	}
})(exports);