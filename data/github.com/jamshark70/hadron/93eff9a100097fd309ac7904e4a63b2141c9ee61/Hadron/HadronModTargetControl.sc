HadronModTargetControl
{
	var parentApp, <parentPlug, <currentSelPlugin, <currentSelParam, <paramNames, loadHolder;

	*new
	{|argParentView, argBounds, argParentApp, argParentPlug|
	
		^super.new.init(argParentView, argBounds, argParentApp, argParentPlug);
	}
	
	init
	{|argParentView, argBounds, argParentApp, argParentPlug|
		parentApp = argParentApp;
		parentPlug = argParentPlug;

		defer { HadronModTargetControlView(argParentView, argBounds).model_(this) };
	}

	currentSelPlugin_ { |plug|
		var oldplug = currentSelPlugin;
		currentSelPlugin = plug;
		this.getParams.changed(\plugin, plug);  // this is for the gui
		if(currentSelPlugin !== oldplug) {
			// and this is for the owner plug
			this.changed(\currentSelPlugin, currentSelPlugin, oldplug, currentSelParam);
		};
	}

	currentSelParam_ { |param|
		var oldparam;
		if(param.isNil or: { paramNames.detect { |item| item == param }.notNil }) {
			oldparam = currentSelParam;
			currentSelParam = param;
			this.changed(\param, param);
			if(currentSelParam !== oldparam) {
				this.changed(\currentSelParam, currentSelParam, currentSelPlugin, oldparam);
			}
		}
	}
	
	plugList { ^parentApp.alivePlugs }

	getParams {
		var numChannels = parentPlug.tryPerform(\targetControlSize) ? 1;
		paramNames = (currentSelPlugin.tryPerform(\modSets) ?? { () }).select({ |func, key|
			try {
				max(1, func.def.prototypeFrame.asArray[0].size) == numChannels
			} { |err|
				if(err.isKindOf(Exception)) {
					"%:% has an invalid modSet for %: %\n"
					.format(
						currentSelPlugin,
						currentSelPlugin.ident,
						key, func
					).warn;
					err.reportError;
				};
				false  // reject this modSet if the func is not valid
			}
		}).keys.asArray.sort;
		this.changed(\paramNames, paramNames);
	}

	plugAdded
	{
		this.refreshAppMenu;
	}
	
	plugRemoved { |argPlug|
		// // qt may supercalifragilistically clear targetAppMenu.value
		// // before I get to finish up, so save the value now while I still can
		// var plugIndex;
		this.refreshAppMenu(argPlug);
		if(currentSelPlugin === argPlug,
		{
			this.currentSelPlugin = nil;
			this.currentSelParam = nil;
		},
		{
			this.changed(\plugin, currentSelPlugin)
		});
	}
	
	refreshAppMenu { |argRejectPlug|
		this.changed(\refreshAppMenu, argRejectPlug);
	}
	
	modulateWithValue
	{|argNormalizedValue|
	
		if((currentSelPlugin != nil) and: { currentSelParam != nil },
		{
			currentSelPlugin.modSets.at(currentSelParam.asSymbol).value(argNormalizedValue);
		});
	}

	updateMappedGui { |argRealValue|
		if((currentSelPlugin != nil) and: { currentSelParam != nil }) {
			currentSelPlugin.modMapSets.at(currentSelParam.asSymbol).value(argRealValue);
		};
	}

	map { |ctlBus|
		var mapped;
		if(mapped = (currentSelPlugin != nil) and: { currentSelParam != nil },
		{
			currentSelPlugin.mapModCtl(currentSelParam, ctlBus, parentPlug);
		});
		^mapped
	}
	unmap { |oldplug(currentSelPlugin), oldparam(currentSelParam)|
		if((oldplug != nil) and: { oldparam != nil },
		{
			oldplug.mapModCtl(oldparam, -1, parentPlug);
		});
	}
	
	getSaveValues
	{
		 ^[
			 // assumes you will never delete a plugin after mapping a modulator
			 // give me a break! Obviously you would want to use the unique ID here!
			 // COME ON....... *think* before coding...
			 // 1 + (parentApp.alivePlugs.indexOf(currentSelPlugin) ? -1),
			 currentSelPlugin.tryPerform(\uniqueID),
			 if(paramNames.size > 0) {
				 currentSelParam
				 // 1 + (paramNames.indexOf(currentSelParam) ? -1);
			 } { 0 }
		 ];
	}
	
	putSaveValues
	{|argValArray|
	
		loadHolder = argValArray;
	}
	
	doWakeFromLoad
	{
		if(loadHolder.notNil) {
			this.currentSelPlugin = if(loadHolder[0].notNil) {
				// backward compatibility: try to use plug index if a low number
				if(loadHolder[0].abs <= parentApp.alivePlugs.size) {
					parentApp.alivePlugs[loadHolder[0] - 1]
				} {
					parentApp.pluginFromID(loadHolder[0])
				}
			} { nil };
			if(loadHolder[1].isNumber) {
				this.currentSelParam = paramNames[loadHolder[1] - 1];
			} {
				this.currentSelParam = loadHolder[1].asSymbol;
			};
			loadHolder = nil;
		}
	}

	remove {
		this.changed(\didRemove);
	}

	pollRate { ^parentPlug.tryPerform(\pollRate) }
	pollRate_ { |rate|
		var plug = currentSelPlugin;
		// we might need to do this during loading as part of a saveSets[] item
		// currentSelPlugin isn't populated at that time! but loadHolder is
		if(plug.isNil and: { loadHolder.notNil and: { loadHolder[0].notNil } }) {
			if(loadHolder[0].abs <= parentApp.alivePlugs.size) {
				plug = parentApp.alivePlugs[loadHolder[0] - 1]
			} {
				plug = parentApp.pluginFromID(loadHolder[0])
			}
		};
		plug.tryPerform(\pollRate_, rate)
	}
}

HadronModTargetControlView : SCViewHolder {
	var <model;
	var targetAppMenu, targetParamMenu;

	*new { |parent, bounds|
		^super.new.init(parent, bounds)
	}

	model_ { |newModel|
		model.removeDependant(this);
		model = newModel;
		model.addDependant(this);
		defer {
			this.refreshAppMenu;

			if(model.notNil and: { model.currentSelPlugin.notNil }) {
				model.getParams;
				this.update(model, \plugin, model.currentSelPlugin);
				if(model.currentSelParam.notNil) {
					this.update(model, \param, model.currentSelParam);
				}
			};
		};
	}

	viewDidClose {
		model.removeDependant(this);
		super.viewDidClose;
	}

	init { |parent, bounds|
		this.view = CompositeView(parent, bounds.copy.height_(20));

		targetAppMenu = PopUpMenu(view, Rect(0, 0, (bounds.width/2)-5, 20))
		.action_
		({|menu|
			if(menu.value == 0,
			{
				model.currentSelPlugin = nil;
			},
			{
				model.currentSelPlugin = this.plugList[menu.value - 1];
			});
		});
		
		this.refreshAppMenu;
		
		targetParamMenu = PopUpMenu(view, Rect((bounds.width/2)+5, 0, (bounds.width/2)-5, 20))
		.items_(["Nothing."])
		.action_
		({|menu|
			if(menu.value == 0,
			{
				model.currentSelParam = nil;
			},
			{
				model.currentSelParam = menu.item.asSymbol;
			});
		});
	}

	refreshAppMenu { |argRejectPlug|
		var oldval = max(0, targetAppMenu.value ? 0);
		targetAppMenu.items_(["Nothing."] ++ this.plugList.reject({|item| item === argRejectPlug; })
			.collect({|item| item.class.asString + item.ident }))
			.value_(oldval);
	}

	update { |obj, what, argument|
		defer {
			switch(what)
			{ \plugin } {
				targetAppMenu.value = 1 + (this.plugList.indexOf(argument) ? -1)
			}
			{ \param } {
				targetParamMenu.value = 1 + (model.paramNames.indexOf(argument) ? -1);
			}
			{ \refreshAppMenu } {
				this.refreshAppMenu(argument)
			}
			{ \paramNames } {
				targetParamMenu.items = ["Nothing"] ++ argument;
			}
			{ \didRemove } {
				this.remove;
			}
		};
	}

	plugList {
		^if(model.notNil) {
			model.plugList
		} {
			Array.new
		};
	}
}