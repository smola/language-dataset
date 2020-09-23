(function(exports) {
	var YJInvalidParamMsg = 'Invalid parameter';
	var YJMissingParamMsg = 'Missing parameter';

  // 1.APP Base Info, PS: BundleId, Version, BuildVersion, AppPath
	YJAppInfo = function() {
		var appId = [NSBundle mainBundle].bundleIdentifier;
		var appversion = [NSBundle mainBundle].infoDictionary[@"CFBundleShortVersionString"];
		var appbuildversion = [NSBundle mainBundle].infoDictionary[@"CFBundleVersion"];
		var screenWidth = [[UIScreen mainScreen] bounds][1][0];
		var screenHeight = [[UIScreen mainScreen] bounds][1][1];
		var apppath = [NSBundle mainBundle].bundlePath;

		// 13
		var result = "BundleId     = " + appId + "\n" +
		            " AppVersion   = " + appversion + "\n" +
								" BuildVersion = " + appbuildversion + "\n" +
								" ScreenWidth  = " + screenWidth + "\n" +
								" ScreenHeight = " + screenHeight + "\n" +
								" AppPath      = " + apppath;
		return result;
	};

	// 2.获取 keyWindow
	YJKeyWindow = function () {
		return UIApp.keyWindow;
	}

	// 3.根控制器
	YJRootVc = function () {
		return UIApp.keyWindow.rootViewController;
	}

	// 递归找到当前显示控制器
	var __YJTopViewController = function(vc) {
		if (vc.presentedViewController) {

					return __YJTopViewController(vc.presentedViewController);
			} else if ([vc isKindOfClass:[UITabBarController class]]) {

					return __YJTopViewController(vc.selectedViewController);
			} else if ([vc isKindOfClass:[UINavigationController class]]) {

					return __YJTopViewController(vc.visibleViewController);
			} else {

				var count = vc.childViewControllers.count;
				for (var i = count - 1; i >= 0; i--) {
					var childVc = vc.childViewControllers[i];
					if (childVc && childVc.view.window) {
						vc = __YJTopViewController(childVc);
						break;
					}
				}
					return vc;
			}
	};

	// 4.当前显示的控制器
	YJTopVc = function() {
		return __YJTopViewController(UIApp.keyWindow.rootViewController);
	};

	// 5.获取控制器 vc 的 view 层级结构
	YJViewsOfVc = function(vc) {
		if (![vc isKindOfClass:[UIViewController class]]) throw new Error(YJInvalidParamMsg);
		return vc.view.recursiveDescription().toString();
	};

	// 6.获取当前显示的 控制器的 View 层级结构
	YJViewsOfTopVc = function() {
		return YJViewsOfVc(__YJTopViewController(UIApp.keyWindow.rootViewController));
	};

	 // 7.获取控制器的层级
	 YJChildVcs = function(vc) {
		 if (![vc isKindOfClass:[UIViewController class]]) throw new Error(YJInvalidParamMsg);
		 return [vc _printHierarchy].toString();
	 };

	 // 8.获取View的层级
	 YJSubviews = function(view) {
		 if (![view isKindOfClass:[UIView class]]) throw new Error(YJInvalidParamMsg);
		 return view.recursiveDescription().toString();
	 };

	 // 9.CG函数 point
	 YJPointMake = function(x, y) {
		 return {0 : x, 1 : y};
	 };

	 // 10.CG函数 size
	 YJSizeMake = function(w, h) {
		 return {0 : w, 1 : h};
	 };

	 // 11.CG函数 rect
	 YJRectMake = function(x, y, w, h) {
		 return {0 : YJPointMake(x, y), 1 : YJSizeMake(w, h)};
	 };

	 // 判断是否为字符串 "str" @"str"
	 YJIsString = function(str) {
		 return typeof str == 'string' || str instanceof String;
	 };

	 // 判断是否为数组 []、@[]
	 YJIsArray = function(arr) {
		 return arr instanceof Array;
	 };

	 // 判断是否为数字 666 @666
	 YJIsNumber = function(num) {
		 return typeof num == 'number' || num instanceof Number;
	 };

	 // 获取类
	 var _YJClass = function(className) {
		 if (!className) throw new Error(YJMissingParamMsg);
		 if (YJIsString(className)) {
			 return NSClassFromString(className);
		 }
		 if (!className) throw new Error(YJInvalidParamMsg);
		 return className.class();
	 };

	 // 12. 打印所有的子类
 	YJSubclasses = function(className, reg) {
 		className = _YJClass(className);
 		return [c for each (c in ObjectiveC.classes)
 		if (c != className
 			&& class_getSuperclass(c)
 			&& [c isSubclassOfClass:className]
 			&& (!reg || reg.test(c)))
 			];
 	};

	// 打印所有的方法
	var _YJGetMethods = function(className, reg, clazz) {
		className = _YJClass(className);

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

	var _YJMethods = function(className, reg, clazz) {
		return _YJGetMethods(className, reg, clazz)[0];
	};

	// 打印所有的方法名字
	var _YJMethodNames = function(className, reg, clazz) {
		return _YJGetMethods(className, reg, clazz)[1];
	};

	// 13.打印所有的对象方法
	YJInstanceMethods = function(className, reg) {
		return _YJMethods(className, reg);
	};

	// 14.打印所有的对象方法名字
	YJInstanceMethodNames = function(className, reg) {
		return _YJMethodNames(className, reg);
	};

	// 15.打印所有的类方法
	YJClassMethods = function(className, reg) {
		return _YJMethods(className, reg, true);
	};

	// 16.打印所有的类方法名字
	YJClassMethodNames = function(className, reg) {
		return _YJMethodNames(className, reg, true);
	};

	// 17.打印所有的成员变量
	YJIvars = function(obj, reg){
		if (!obj) throw new Error(YJMissingParamMsg);
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

	// 18.打印所有的成员变量名字
	YJIvarNames = function(obj, reg) {
		if (!obj) throw new Error(YJMissingParamMsg);
		var array = [];
		for(var name in *obj) {
			if (reg && !reg.test(name)) continue;
			array.push(name);
		}
		return array;
	};

	// 19.获取按钮绑定的所有TouchUpInside事件的方法名
	YJBtnTouchUpEvent = function(btn) {
		var events = [];
		var allTargets = btn.allTargets().allObjects()
		var count = allTargets.count;
			for (var i = count - 1; i >= 0; i--) {
				if (btn != allTargets[i]) {
					var e = [btn actionsForTarget:allTargets[i] forControlEvent:UIControlEventTouchUpInside];
					events.push(e);
				}
			}
		 return events;
	};

	// 20.document path
	YJDocPath = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES)[0];

	// 21.caches path
	YJCachesPath = NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES)[0];

	// 22.加载系统动态库
	YJLoadFramework = function(name) {
		var head = "/System/Library/";
		var foot = "Frameworks/" + name + ".framework";
		var bundle = [NSBundle bundleWithPath:head + foot] || [NSBundle bundleWithPath:head + "Private" + foot];
  		[bundle load];
  		return bundle;
	};

	// 帮助选项
	help = function () {

		var helpTitle 					= "Contains the following Cycript encapsulation:\n";
		var appInfo  					  = "  1. YJAppInfo()                         APP Base Info, PS: BundleId, Version, BuildVersion, AppPath\n";
		var keyWindow 					= "  2. YJKeyWindow()                       KeyWindow\n";
		var rootVc    					= "  3. YJRootVc()                          rootViewController\n";
		var topVc     					= "  4. YJTopVc()                           topViewController\n";
		var viewsOfVc 					= "  5. YJViewsOfVc(vc)                     Hierarchical structure of vc\n";
		var viewsOfTopVc 				= "  6. YJViewsOfTopVc()                    Hierarchical structure of topvc\n";
		var childVcs 						= "  7. YJChildVcs(vc)                      All subclasses of controllers\n";
		var subviews 						= "  8. YJSubviews(view)                    All subclasses of view\n";
		var pointMake 					= "  9. YJPointMake(x,y)                    CGPoint\n";
		var sizeMake 						= " 10. YJSizeMake(w,h)                     CGSize\n";
		var rectMake 						= " 11. YJRectMake(x,y,w,h)                 CGRect\n";
		var subclasses 					= " 12. YJSubclasses(clsName, reg)          All subclasses of clsName\n";
		var instanceMethods 		= " 13. YJInstanceMethods(clsName, reg)     All InstanceMethod of clsName\n";
		var instanceMethodNames = " 14. YJInstanceMethodNames(clsName, reg) All InstanceMethod name of clsName\n";
		var classMethods 				= " 15. YJClassMethods(clsName, reg)        All classMethod of clsName\n";
		var classMethodNames 		= " 16. YJClassMethodNames(clsName, reg)    All classMethod name of clsName\n";
		var ivars 							= " 17. YJIvars(obj, reg)                   All ivar of obj\n";
		var ivarNames 					= " 18. YJIvarNames(obj, reg)               All ivar name of obj\n";
		var btnTouchUpEvent 		= " 19. YJBtnTouchUpEvent(btn)              Method names of all TouchUpInside events bound by buttons\n";
		var docPath 						= " 20. YJDocPath                           Document Path\n";
		var cachesPath					= " 21. YJCachesPath                        Caches Path\n";
		var loadFramework 			= " 22. YJLoadFramework(name)               Load System Dynamic Library";

		var result = helpTitle + appInfo + keyWindow + rootVc + topVc + viewsOfVc + viewsOfTopVc + childVcs +
		             subviews + pointMake + sizeMake + rectMake + subclasses + instanceMethods +
								 instanceMethodNames + classMethods + classMethodNames + ivars + ivarNames + btnTouchUpEvent +
								 docPath + cachesPath + loadFramework;
		return result;
	}

})(exports);
