(
	local tentacleExpression = "if Enabled==true or Enabled==1 then (if Inverted==false or Inverted==-1 then (cos(-F*Speed+Frequency*(id+Phase))*((Amplitude/100)*(id^(Falloff/50)))) else (cos(F*Speed+Frequency*(id+Phase))*((Amplitude/100)*(id^(Falloff/50))))) else 0"
	-- Define Custom Attributes
	local XAxis = attributes XAxis (
		parameters x_params rollout:x_rollout (
			Amplitude type:#float UI:amplitude_spn default:0
			Frequency type:#float UI:frequency_spn default:0
			Speed type:#float UI:speed_spn default:3.6
			Falloff type:#float UI:falloff_spn default:0
			Phase type:#float UI:phase_spn default:0
			Period type:#integer UI:period_spn default:0
			
			on Period set val do ( if val>0 then (Speed = 360/(val as float)) )
			on Speed set val do ( if val>0 then (Period = 360/(val as float)) )
		)
		rollout x_rollout "X Axis" (
			label amplitude_lbl "Amplitude................" pos:[8,8]
			label frequency_lbl "Frequency..........." pos:[8,24]
			label speed_lbl "Speed................" pos:[8,40]
			label falloff_lbl "Falloff................." pos:[8,56]
			label phase_lbl "Phase................" pos:[8,72]
			label period_lbl "Period........." pos:[8,88]
			spinner amplitude_spn "" pos:[104,8] width:48 height:16 range:[-999,999,0]
			spinner frequency_spn "" pos:[104,24] width:48 height:16 range:[-999,999,0]
			spinner speed_spn "" pos:[104,40] width:48 height:16 range:[0,999,0] type:#float
			spinner falloff_spn "" pos:[104,56] width:48 height:16 range:[0,100,0]
			spinner phase_spn "" pos:[104,72] width:48 height:16 range:[-999,999,0]
			spinner period_spn "" pos:[104,88] width:48 height:16 range:[1,999,100] type:#integer
		)
	)
	local YAxis = YAxis = attributes YAxis (
		parameters y_params rollout:y_rollout (
			Amplitude type:#float UI:amplitude_spn default:9
			Frequency type:#float UI:frequency_spn default:20
			Speed type:#float UI:speed_spn default:3.6
			Falloff type:#float UI:falloff_spn default:30
			Phase type:#float UI:phase_spn default:0
			Period type:#integer UI:period_spn default:100
			
			on Period set val do ( if val>0 then (Speed = 360/(val as float)) )
			on Speed set val do ( if val>0 then (Period = 360/(val as float)) )
		)
		rollout y_rollout "Y Axis" (
			label amplitude_lbl "Amplitude................" pos:[8,8]
			label frequency_lbl "Frequency..........." pos:[8,24]
			label speed_lbl "Speed................" pos:[8,40]
			label falloff_lbl "Falloff................." pos:[8,56]
			label phase_lbl "Phase................" pos:[8,72]
			label period_lbl "Period........." pos:[8,88]
			spinner amplitude_spn "" pos:[104,8] width:48 height:16 range:[-999,999,0]
			spinner frequency_spn "" pos:[104,24] width:48 height:16 range:[-999,999,0]
			spinner speed_spn "" pos:[104,40] width:48 height:16 range:[0,999,0] type:#float
			spinner falloff_spn "" pos:[104,56] width:48 height:16 range:[0,100,0]
			spinner phase_spn "" pos:[104,72] width:48 height:16 range:[-999,999,0]
			spinner period_spn "" pos:[104,88] width:48 height:16 range:[1,999,100] type:#integer
		)
	)
	local ZAxis = attributes ZAxis (
		parameters z_params rollout:z_rollout (
			Amplitude type:#float UI:amplitude_spn default:0
			Frequency type:#float UI:frequency_spn default:0
			Speed type:#float UI:speed_spn default:3.6
			Falloff type:#float UI:falloff_spn default:0
			Phase type:#float UI:phase_spn default:0
			Period type:#integer UI:period_spn default:0
			
			on Period set val do ( if val>0 then (Speed = 360/(val as float)) )
			on Speed set val do ( if val>0 then (Period = 360/(val as float)) )
		)
		rollout z_rollout "Z Axis" (
			label amplitude_lbl "Amplitude................" pos:[8,8]
			label frequency_lbl "Frequency..........." pos:[8,24]
			label speed_lbl "Speed................" pos:[8,40]
			label falloff_lbl "Falloff................." pos:[8,56]
			label phase_lbl "Phase................" pos:[8,72]
			label period_lbl "Period........." pos:[8,88]
			spinner amplitude_spn "" pos:[104,8] width:48 height:16 range:[-999,999,0]
			spinner frequency_spn "" pos:[104,24] width:48 height:16 range:[-999,999,0]
			spinner speed_spn "" pos:[104,40] width:48 height:16 range:[0,999,0] type:#float
			spinner falloff_spn "" pos:[104,56] width:48 height:16 range:[0,100,0]
			spinner phase_spn "" pos:[104,72] width:48 height:16 range:[-999,999,0]
			spinner period_spn "" pos:[104,88] width:48 height:16 range:[1,999,100] type:#integer
		)
	)
	local propertiesData = attributes Properties (
		parameters properties_params rollout:properties_rollout (
			Enabled type:#boolean UI:enabled_chk default:true invisibleInTV:true
			Inverted type:#boolean UI:inverted_chk default:false invisibleInTV:true
			Dummy_size type:#integer UI:dummy_size_spn default:5						
		)
		rollout properties_rollout "Properties" (
			label lbl2 "Enabled........................." pos:[8,8] width:128 height:16
			label lbl1 "Inverted....................." pos:[8,24] width:128 height:16
			label lbl3 "Helper size................" pos:[8,40] width:96 height:16
			checkbox enabled_chk "" pos:[140,8] width:16 height:16 checked:true
			checkbox inverted_chk "" pos:[140,24] width:16 height:16 checked:false
			spinner dummy_size_spn "" pos:[104,40] width:48 height:16 range:[0,100,5] type:#worldunits
		)
	)
	
	-- Collect bone chain
	fn collectBones headBone = (
		local childBones = #(headBone)
		for child in headBone.children do ( join childBones (collectBones child); exit ) -- Collect only the first child. 
		childBones
	)
	
	-- Process start
	local mySelection = for o in getCurrentSelection() where o.children.count > 0 collect o
	if mySelection.count >= 1 then with undo label:"Apply Tentacles To Selection" on (
		local dummies = #()
		for headBone in mySelection do (
			local bonesArray = collectBones headBone
			local tentacleCtrl = dummy name:"Tentacle_ctrl" rotation:headBone.rotation pos:headBone.pos boxsize:[5,5,5]
			tentacleCtrl.parent = headBone.parent
			headBone.parent = tentacleCtrl
			append dummies tentacleCtrl
			
			-- Add Custom Attributes
			custAttributes.add tentacleCtrl XAxis
			custAttributes.add tentacleCtrl YAxis
			custAttributes.add tentacleCtrl ZAxis
			custAttributes.add tentacleCtrl propertiesData
			
			-- Assign Default Controllers
			for propName in #(#XAxis, #YAxis, #ZAxis) do (
				custAttributes.add tentacleCtrl ( if propName == #XAxis then XAxis else if propName == #YAxis then YAxis else if propName == #ZAxis then ZAxis )
				local data = getProperty tentacleCtrl propName
				data.Amplitude.controller = Bezier_Float()
				data.Frequency.controller = Bezier_Float()
				data.Speed.controller = Bezier_Float()
				data.Falloff.controller = Bezier_Float()
				data.Phase.controller = Bezier_Float()
			)
			tentacleCtrl.Properties.Dummy_size.controller = Bezier_Float()
			tentacleCtrl.Properties.Enabled.controller = on_off()
			tentacleCtrl.Properties.Inverted.controller = on_off()
			
			-- Assign Script controllers
			for i=1 to bonesArray.count do (
				local obj = bonesArray[i]
				
				for propName in #(#XAxis, #YAxis, #ZAxis) do (
					local data = getProperty tentacleCtrl propName
					local ctrl = float_script ()
					local list = float_list ()
					if propName == #XAxis then obj.rotation.controller.X_Rotation.controller = list else if propName == #YAxis then obj.rotation.controller.Y_Rotation.controller = list else if propName == #ZAxis then obj.rotation.controller.Z_Rotation.controller = list
					list.Available.controller = ctrl
					
					-- Create Script Controller Variables
					ctrl.addTarget "Amplitude" data.Amplitude.controller
					ctrl.addTarget "Frequency" data.Frequency.controller
					ctrl.addTarget "Speed" data.Speed.controller
					ctrl.addTarget "Falloff" data.Falloff.controller
					ctrl.addTarget "Phase" data.Phase.controller
					
					ctrl.addTarget "Enabled" tentacleCtrl.Properties.Enabled.controller
					ctrl.addTarget "Inverted" tentacleCtrl.Properties.Inverted.controller
					ctrl.addConstant "id" i
					
					-- Assign Script
					if i==1 then (
						ctrl.AddObject "parent" tentacleCtrl
						ctrl.AddTarget "size" tentacleCtrl.Properties.Dummy_size.controller
						ctrl.SetExpression ("parent.boxsize.x = size\nparent.boxsize.y = size\nparent.boxsize.z = size\n"+tentacleExpression)
					)
					else (
						ctrl.SetExpression tentacleExpression
					)
				)
			)
		)
		-- Select Head Dummies
		select dummies
	)
	else ( messagebox "You need to select at least one \"Head\" object that has at least one child." )
)