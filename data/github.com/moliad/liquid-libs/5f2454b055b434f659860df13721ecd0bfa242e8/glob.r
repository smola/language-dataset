REBOL [
	; -- Core Header attributes --
	title: "Glob | Graphic layered objects"
	file: %glob.r
	version: 1.0.1
	date: 2013-9-10
	author: "Maxim Olivier-Adlhoch"
	purpose: {Core Canvas library to build interactive graphics, structured visual documents and more.  Has layers, uses lazy dataflow, allows arbitrary shaped input regions.}
	web: http://www.revault.org/modules/glob.rmrk
	source-encoding: "Windows-1252"
	note: {slim Library Manager is Required to use this module.}

	; -- slim - Library Manager --
	slim-name: 'glob
	slim-version: 1.2.1
	slim-prefix: none
	slim-update: http://www.revault.org/downloads/modules/glob.r

	; -- Licensing details  --
	copyright: "Copyright © 2013 Maxim Olivier-Adlhoch"
	license-type: "Apache License v2.0"
	license: {Copyright © 2013 Maxim Olivier-Adlhoch

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at
	
		http://www.apache.org/licenses/LICENSE-2.0
	
	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.}

	;-  / history
	history: {
		v0.0.1 - 27-Dec-2006/19:24:59 (MOA)
			-after a lot of prototyping with combining AGG, view and liquid...
			-create core !glob

		v0.0.2 - 16-Feb-2007/20:44:02 (MOA)

		v0.0.2 - 15-Apr-2007/21:36:50 (Unknown)
			-add layers to glob engine


		v0.0.3 - 15-Apr-2007/23:47:41 (Unknown)


		v0.0.4 - 29-Apr-2007/7:55:51 (Unknown)


		v0.0.5 - 30-Apr-2007/16:56:30 (Unknown)


		v0.0.6 - 30-Apr-2007/22:07:39 (Unknown)
			-fixed glob/unlink
			-fully implemented all destruction of all glob nodes.


		v0.0.7 - 11-May-2007/0:46:55 (Unknown)


		v0.0.9 - 11-May-2007/2:30:21 (Unknown)


		v0.0.11 - 15-Jul-2007/0:58:44 (Unknown)

		v0.1.0  - 23/04/2009/1:31AM (MOA)
			-added reflection system.
			
		v0.2.0  - 23/04/2009/1:31AM (MOA)
			-refurbishing feel system.
			-building generic glob event handler
			-integrated ON-OVER() 
			
		v0.2.1  - 30/04/2009/10:02PM (MOA)
			-several on-event are integrated within handler
			-added !word input type.
		
		v0.2.2  - 05/05/2009/1:30PM (MOA)
			-added /reset to !glob's LINK() method, solves a discrepancy with newer liquid versions, which added /reset.
			
		v1.0.0  - 02/02/2010 (MOA)
			-just a milestone release since this is now a publicly distributed file, I'm signing it off as a v1 reference release.

		v1.0.1 - 2013-9-10 (MOA)
			-license change to Apache v2}
	;-  \ history

	;-  / documentation
	documentation: {
		globs are a very low-level layer of the GLASS framework.
		
		They have been used in many applications and are very robust so far.
		
		The nice thing about globs is that you can link them up directly so that you could create your own
		custom globs and display them in a GLASS display without requiring any special code.
		
		The only complication will be the mapping of the bitmask plane to Glass marbles within
		the stream engine.  Some analysis and development will be required in order to use
		globs directly within a GLASS display and not create errors when handling the input
		related those globs.
		
		we might simply add handler support to globs via an intermediate layer.  or make a glob 
		marble which is only usable within a special canvas frame or viewport.
		
		globs as such are independent from GLASS but they where developped with GLASS in mind.
		you may use globs directly for your own graphic needs.
	}
	;-  \ documentation
]




;--------------------------------------
; unit testing setup
;--------------------------------------
;
; test-enter-slim 'glob
;
;--------------------------------------

slim/register [

	; these are declared, in order for slim to set them within the glob lib. 
	; otherwise they pollute the global context.
	liquify: none
	content: none
	!plug: none
	fill: none
	link: none
	retrieve-plug: none

	;core: slim/open 'core none
	
	;- LIBS
	;gl: slim/open 'glayout 0.4.16
	lqd: slim/open/expose 'liquid none [ !plug  liquify  fill  content  link  retrieve-plug ]
	;glue: slim/open 'glue none


	;- FUNCTIONS
;	;--------------------
;	;-    --init--()
;	;--------------------
;	--init--: func [
;		""
;	][
;		vin/tags ["Glob/--init--()"]
;		vout/tags [tt]
;		true
;	]
	
	
	
	;--------------------
	;-    to-color()
	;--------------------
	to-color: func [
		""
		value [integer!]
	][
		; the 'REVERSE is for litle endian system...
		0.0.0.0 + to-tuple next reverse third make struct! [int [int]] reduce [value ]

		;<FIXME>
		; return big endian value on big-endian system
	]
	
	;--------------------
	;-    to-sid()
	;--------------------
	to-sid: func [
		""
		value [tuple! none!]
	][
		if tuple? value [
			if 4 <= length? value [value: make tuple! reduce [value/1 value/2 value/3]]
			to-integer to-binary value
		]
	]

	;--------------------
	;-    vlength()
	;--------------------
	vlength: func [v] [v: v * v square-root v/x + v/y]

	
	
	;--------------------
	;-    point()
	;--------------------
	point: func [
		{compute the specified point on the line}
		start [pair!]
		end [pair!]
		at [decimal! integer!]
		/bounded {truncates at if it ends up being larger than vector, note does not support negative at}
		/local vector points 
	] [
		; solve "degenerate case" first
		if equal? start end [return start]
		vector: end - start
		 
		if integer? at [
			; convert AT to decimal
			at: at / (to-integer vlength vector)
		]
		
		if all[
			bounded 
			at > 1.0
		][
			return end
		]
		
		; compute from end (instead of start)
		if negative? at [
			at: 1 + at
		]
		
		start + to-pair reduce [to-integer vector/x * at to-integer vector/y * at] 
	]
	
	;--------------------
	;-    segment()
	;--------------------
	segment: func [
		{compute the specified segment from the line}
		start [pair!]
		end [pair!]
		from [decimal! integer! none!]
		to  [decimal! integer! none!]
		/local vector points length
	][
		; solve "degenerate case" first
		if equal? start end [return reduce [start start]]
		vector: end - start
		length: vlength vector
		
		;---------- 
		;  FROM
		if integer? from [
			; convert from to decimal
			from: from / length
		]
		if none? from [
			from: 0
		]
		if negative? from [
			; this case lets us define a segment using the end as the reference
			from: 1 + from ; from is negative, so this actually substracts from length
		]
		
		;---------- 
		;  TO
		if integer? to [
			; convert to, to decimal
			to: to / length 
		]
		if none? to [
			to: 1
		]
		if negative? to [
			; this case lets us define a segment using the end as the reference
			to: 1 + to ; to is negative, so this actually substracts from length
		]
		reduce [start + to-pair reduce [to-integer vector/x * from to-integer vector/y * from] start + to-pair reduce [to-integer vector/x * to to-integer vector/y * to] ]
	]
	
	sizer-face: make face [ size: 1000x200 edge: none para: none font: none]
	;?? sizer-face
	;ask "..."
	
	;--------------------
	;-    sizetext()
	;--------------------
	sizetext: func [
		""
		text 
		font
	][
		sizer-face/font: font
		sizer-face/text: text
		
		;?? sizer-face
		
		size-text sizer-face
	]
	
	;-  
	;------------------------
	;- intypes
	;------------------------
	intypes: context [
		;linked-container?: true
		;-    !any
		!any: make lqd/!plug [
			valve: make valve [
				type: 'glob-intype-any
				
				linked-container?: false
				
				datatype: none
				
				;--------------------
				;-       purify()
				;--------------------
				purify: func [
					""
					plug [object!]
				][
					vin/tags ["glob/intypes["plug/sid"]/purify()"] [purify]
					if datatype [
						unless (type? plug/liquid) = datatype [
							;print "GLOB TYPE MISMATCH"
							;probe plug/liquid
							;probe datatype
							plug/liquid: switch/default to-word plug/valve/datatype [
								string! [
									;print "STRING!"
									either none? plug/liquid [
										""
									][
										mold plug/liquid
									]
								]
							][
								;probe plug/valve/datatype
								any [
									attempt [make datatype plug/liquid]
									attempt [make datatype none]
								]
							]
						]
					]
					
					vout/tags [purify]
					false
				]
				
				;--------------------
				;-    cleanse()
				;--------------------
				cleanse: func [
					""
					plug
				][
					vin/tags ["!intypes/[" plug/sid "]cleanse()"] [cleanse]
					;new-pipe plug
					if datatype [
						fill plug switch to-word datatype [
							pair! [
								0x0
							]
							tuple! [
								random 255.255.255
							]
							integer! [
								1
							]
							block! [
								copy []
							]
							string! [
								copy ""
							]
						]
					]
					vout/tags [cleanse]
				]
				
			]
		]
		
		;-    !pair
		!pair: make !any [
			valve: make valve [
				type: 'glob-intype-pair
				datatype: pair!
				purify: func [
					plug 
				][
					;print ["pair: " plug/liquid]
					false
				]
			]
		]
		
		
		
		;-    !color
		!color: make !any [
			valve: make valve [
				type: 'glob-intype-color
				datatype: tuple!
			]
		]
		
		;-    !integer
		!integer: make !any [
			valve: make valve [
				type: 'glob-intype-integer
				datatype: integer!
			]
		]
		
		;-    !decimal
		!decimal: make !any [
			valve: make valve [
				type: 'glob-intype-decimal
				datatype: decimal!
			]
		]
		
		;-    !bool
		!bool: make !any [
			valve: make valve [
				type: 'glob-intype-bool
				datatype: logic!
			]
		]
		
		;-    !block
		!block: make !any [
			valve: make valve [
				type: 'glob-intype-block
				datatype: block!
			]
		]
		;-    !state
		!state: make !any [
			valve: make valve [
				type: 'glob-intype-state
				datatype: logic!
			]
		]
		;-    !time
		!time: make !any [
			valve: make valve [
				type: 'glob-intype-time
				datatype: time!
			]
		]
		;-    !date
		!date: make !any [
			valve: make valve [
				type: 'glob-intype-date
				datatype: date!
			]
		]
		;-    !string
		!string: make !any [
			valve: make valve [
				type: 'glob-intype-string
				datatype: string!
			]
		]
		;-    !word
		!word: make !any [
			valve: make valve [
				type: 'glob-intype-word
				datatype: word!
			]
		]
	]
		
	
	
	
	;-  
	;------------------------
	;- !GEL
	;------------------------
	!gel: make lqd/!plug [
		draw-spec: none
		
		plug-sid: none
		
		; points to the glob this gel is included in.
		glob: none
		
		valve: make valve [
			type: '!gel
			category: '!gel
			
			;--------------------
			;-    cleanse()
			;--------------------
			cleanse: func [
				""
				gel [object!]
			][
				vin/tags ["!gel[" gel/sid "]/cleanse()"] [cleanse]
				gel/liquid: copy []
				;plug/input/offset/valve/fill plug/input/offset 0x0
				vout/tags [cleanse]
			]
			
			
			;--------------------
			;-    destroy()
			;--------------------
			destroy: func [
				""
				plug
			][
				vin/tags ["!gel/destroy()"] [destroy]
				plug/draw-spec: none ; this is a shared value with the glob. do not clear it.
				plug/glob: none
				
				!plug/valve/destroy plug
				vout/tags [destroy]
			]


						
			
			;--------------------
			;-    process()
			;--------------------
			process: func [
				""
				gel [object!]
				data [block!]
				/local plug tmp value blk offset pos size clr
			][
				vin/tags ["!gel[" gel/sid "]/process()"] [process]
				clear gel/liquid
				
				append gel/liquid compose/deep bind/copy gel/draw-spec 'data
				
				vout/tags [process]
			]
			


;			;--------------------
;			;-    blocking-state()
;			;--------------------
;			blocking-state: func [
;				"This allows you to define states in which the node will not propagate dirtyness.  This can be usefull to prevent useless computing when some input is not actually contributing to output in some specific states.  A possible future api extension being researched "
;				plug [object!]
;			][
;				vin/tags ["blocking-state()"] [ !glob blocking-state]
;				
;				
;				vout/tags [!glob blocking-state]
;				
;				;return true if we are in a blocking state.  This might even be a manually locked eventually.
;				; defaults to false, unless we really have a reason to block.
;				false
;			]
			
			;blocking-state: none ; just a faster alternative nop... but keep the spec above.
;			;--------------------
;			;-    instigate()
;			;--------------------
;			instigate: func [
;				""
;				plug
;			][
;				print "INSTIGATING!"
;				probe !plug/valve/instigate plug
;			]

;			;---------------------
;			;-    propagate
;			;---------------------
;			; cause observers to become dirty
;			;---------------------
;			propagate: func [
;				plug [object!]
;			][
;				vin/tags ["glob/"  type  "[" plug/sid "]/propagate" ] [ !glob propagate]
;				either blocking-state plug [
;					print "BLOCKING!"
;				][
;					!plug/valve/propagate plug
;				]
;				vout/tags [!glob propagate]
;			]
		]
	]		
	
		
	;-  
	;------------------------
	;;- !STACK
	;------------------------
	!stack: make lqd/!plug [
		linked-container?: true  ; the container data is the !stack parameters if they are not linked
		
		; store compiled layers
		layers: none
		
		valve: make valve [
			type: '!stack
			category: '!stack
			

			;--------------------
			;-    cleanse()
			;--------------------
			cleanse: func [
				""
				plug [object!]
			][
				vin/tags ["!glob[" plug/sid "]/cleanse()"] [cleanse]
				plug/liquid: copy []
				plug/layers: copy []
				;plug/input/offset/valve/fill plug/input/offset 0x0
				vout/tags [cleanse]
			]
			
			
			
			;--------------------
			;-    destroy()
			;--------------------
			destroy: func [
				""
				plug
			][
				vin/tags ["!stack/destroy()"] [destroy]
				clear head plug/layers
				plug/layers: none
				!plug/valve/destroy plug
				vout/tags [destroy]
			]


			
			;--------------------
			;-    get-items()
			;--------------------
			get-items: func [
				""
				blk
				lbl
				/local rblk
			][
				if rblk: find blk lbl [
					rblk: copy/part next rblk any [
						find next rblk word!
						tail rblk
					]
				]
				rblk
			]

		
		
			;--------------------
			;-    process()
			;--------------------
			process: func [
				""
				plug [object!]
				data
				/local blk layer
			][
				vin/tags ["!stack[" plug/sid "]/process()"] [process]
				clear head plug/liquid
				foreach layer data [
					append plug/liquid layer
				]
				vout/tags [process]
			]

			
		]
	]	
	
	;-  
	;------------------------
	;- !GLOB
	;------------------------
	!glob: make lqd/!plug [
		;----
		;-  input:
		input: none
		
		;----
		;-  linked-container?:
		; <TO DO> set this to false by default... should drastically improve load performance for large networks...
		linked-container?: true  ; this contains a single graphic definition which is then output in two blocks
		
		;----
		;-  clr-bak:
		clr-bak: red

		;----
		;-  drag-origin:
		drag-origin: 0x0
		
		;----
		;-  layers:
		; this is a list of !gel nodes, one for each layer
		; stack will link their own layer nodes to these nodes.
		; the glob will no long connect itself to the inputs, but rather will connect its layers to them.
		layers: none
		
		;----
		;-  draw-spec:
		; use the reflect() in the valve to set layers to use as outputs.
		draw-spec: none
		
		;----
		;-  reflection:
		; intermediate node which pipes dirty messages and compiles draw blocks from internal layers.
		reflection: none
	
		;----
		;-   VALVE:
		valve: make valve [
			type: '!glob
			category: '!glob
			
			; 
			;-    input-spec:
			input-spec: none
			
			;-    gel-spec:
			gel-spec: none
			
			
			;-----------------
			;-    reflect()
			;-----------------
			; setup the glob so that its content is set to reflect the contents of
			; selected layers.  this is sort of a hack, since an internal plug is created
			; which will propagate dirty messages to the glob.
			;
			; the glob, when asked for its content, will then actually refer to the reflection plug
			; and set itself to that data.
			;
			; this setup allows a glob to autonomously output its content, without the need for a complex
			; external viewport-like node.
			;-----------------
			reflect: func [
				glob [object!] "plug to reflect"
				layers [block! integer! none!] "a list or single layer to reflect, none removes the reflection"
				/local glob* layer
			][
				vin ["!glob[" glob/sid "]/reflect()"]
				
				either layers: all [layers compose [(layers)]] [
					vprint "Setting up reflection"
					unless object? glob/reflection [
						vprint "Creating new reflection"
						glob*: glob 
						glob/reflection: liquify/with !plug [
							;stainless?: true
							glob: glob*
							valve: make valve [
								type: '!reflection
								;-----------------
								;-        propagate?()
								;-----------------
								; we are using this function outside of its intended use, as a callback.
								; since we return true, its still valid.
								;
								; this function will propagate the dirtyness of glob's reflected gels or stacks 
								; to the glob itself!
								;-----------------
								propagate?: func [
									plug [object!]
								][
									vin ["!glob/reflection/[" plug/sid "]/propagate?()"]
									vprobe plug/valve/type
									plug/glob/valve/dirty plug/glob
									vout
									true
								]
								
								;-----------------
								;-        process()
								;-----------------
								process: func [
									plug
									data
								][
									plug/liquid: copy []
									vin ["!glob/reflection/[" plug/sid "]/process()"]
									forall data [
										append plug/liquid first data
									]
									vout
								]
							]
						]
					]
					glob/reflection/valve/unlink glob/reflection
					foreach layer layers [
						vprint ["linking to layer: " layer]
						either layer: pick glob/layers layer [
							glob/reflection/valve/link glob/reflection layer
						][
							vprint "This layer isn't set in glob"
						]
					]
				][
					; layers set to none, deconstruct reflection if one is currently set.
					
					; <TO DO>
				]
				vout
			]
			
			
			
			
			;--------------------
			;-    process()
			;--------------------
			process: func [
				""
				plug [object!]
				data [block!]
			][
				vin/tags ["!glob[" plug/sid "]/process()"] [process]
;				clear first plug/liquid
;				clear second plug/liquid
;				
;				append first  plug/liquid compose/deep bind/copy specs/1 'data
;				append second plug/liquid compose/deep bind/copy specs/2 'data
				
				;probe first plug/liquid
				;probe second plug/liquid
				;append/only plug/liquid head insert (copy/deep first plug/liquid) compose [pen (to-color plug/sid)]
				;plug/liquid: data
				if plug/reflection [
					plug/liquid: plug/reflection/valve/content plug/reflection
				]
				vout/tags [process]
			]
			
			
			;--------------------
			;-    pre-allocate-layers()
			; this creates a number of stack nodes ready for use.  so observers can be 
			; connected before the glob connects to other globs.
			;--------------------
			pre-allocate-layers: func [
				glob [object!]
				count [integer!]
				;/local stack
			][
				if none? glob/layers [
					glob/layers: copy []
				]
				loop count [
					append glob/layers  liquify !stack
				]
			]
			
			
			;--------------------
			;-    add-layers()
			;--------------------
			add-layers: func [
				""
				glob [object!]
				/local words spec paren ext val
			][
				vin/tags ["!glob[" glob/sid "]/add-layers()"] [add-layers]
				
				if gel-spec [
					parse gel-spec [
						some [
							;(words: copy [])
							copy val some word! (words: val if words/1 = 'none [words: none])
							|
							copy val paren! (ext: bind/copy to-block val/1 'process )
							|
							copy val block! (spec: val  add-layer/with glob  words spec/1 ext  ext: none)
						]
					]
				]
				vout/tags [add-layers]
			]
			
			;--------------------
			;-    add-layer()
			;--------------------
			add-layer: func [
				""
				glob [object!]
				words [block! none!]
				spec  [block!]
				/with extension
				/local gel word input
			][
				vin/tags ["!glob[" glob/sid "]/add-layer()"] [add-layer]
				vprint ["adding layer: " words]
				;probe extension
				;probe words
				extension: any [extension []]
				gel: liquify/with !gel extension
				
				gel/glob: glob
				
				;probe get in gel 'blocking-state
				
				append glob/layers gel
				;lqd/von
				; link appropriate layers to it
				if words [
					foreach word words [
						unless input: select glob/input word [
							to-error rejoin ["GLOB/add-layer(): glob has no input named '" word]
						]
						gel/valve/link/label gel input word
					]
				]
				
				;link our plug sid, this never changes, so don't make a plug for this
				gel/plug-sid: glob/sid
				
				;lqd/voff
				
				; share appropriate effects blk
				
				gel/draw-spec: spec
				
				
				vout/tags [add-layer]
			]
		

			;--------------------
			;-    add-inputs()
			;--------------------
			add-inputs: func [
				""
				glob
				/local  item type spec input default
			][
				vin/tags ["!glob[" glob/sid "]/add-inputs()"] [add-inputs]
				;probe glob/valve/input-spec
				if glob/valve/input-spec [
				
					; new flexible dialect
					until [
						;probe ">"
						; input spec is a value of the valve, so its bound to it
						item: pick glob/valve/input-spec 1
						switch type?/word item [
							word! [
								either  #"!" = first to-string item [
									type: item
									
									; reset default, to prevent data type errors
									; note: if you don't change types between inputs, all will 
									; use the same default (this is a feature)
									default: none
								][
									; add input if we have everything we need
									if all [input type][
										add-input/preset glob input type default
									]
									
									; name of following node
									input: item ; name of the input
									
									;---
									; note: even if the last thing of a input block is a word (and 
									; not allocated here, the end of the loop will create it)
								]
							]
;							block! [
;								probe item
;							]
							
							paren! [
								; parens allows the user to supply programmable default values
								;probe item
								default: do item
							]
						]
						
						glob/valve/input-spec: skip glob/valve/input-spec 1
						tail? glob/valve/input-spec 
					]
					glob/valve/input-spec: head glob/valve/input-spec
					; at the end of the loop, we generally have one input left to allocate... (unless missing data)
					if all [input type][
						add-input/preset glob input type default
					]
				]
				vout/tags [add-inputs]
			]
			
			;--------------------
			;-    add-input()
			;--------------------
			add-input: func [
				""
				glob [object!]
				name [word!]
				type [word!]
				/preset value "data you wish to fill the input with at allocation"
				/local plug
			][
				vin/tags ["!glob[" glob/sid "]/add-input()"] [add-input]
				; allocate new input
				;print ["new: " type " named: " name " filled with: " value]
				
				plug:  liquify intypes/:type
				
				; put the node in our store of inputs, for proper handling in other tasks
				append glob/input reduce [name plug]
				
				; listen to that input.
				; glob/valve/link/label glob plug name ; no more, the gels now listen directly.
				
				unless none? value [
					;probe value
					plug/valve/fill plug value
					;probe plug/valve/content plug
				]
				
				vout/tags [add-input]
			]
					
			;--------------------------------------------------------
			; the state managers are a test within the scope of liquid
			; these do not fill the data in the normal way, they actuall
			; modify the block in-place.
			; 
			; the state input is a simple container... not piped.
			;--------------------------------------------------------
			;--------------------
			;-    set-state()
			;--------------------
			set-state: func [
				""
				plug [object!]
				state [word!]
				;/local blk
			][
				vin/tags ["!glob[" plug/sid "]/set-state()"] [set-state]
				if plug: select plug/input state [
					; prevent state propagation, if state does not actually change
					;unless plug/liquid [
						fill plug true
					;]
				]
				vout/tags [set-state]
			]
			
			;--------------------
			;-    clear-state()
			;--------------------
			clear-state: func [
				""
				plug [object!]
				state [word!]
			][
				vin/tags ["!glob[" plug/sid "]/clear-state()"] [clear-state]
				if plug: select plug/input state [
					; prevent state propagation, if state does not actually change
					;if plug/liquid [
						fill plug false
					;]
				]
				vout/tags [clear-state]
			]
			
			;--------------------
			;-    setup()
			;--------------------
			setup: func [
				"allocate non recyclable glob data. scans the inputs block and allocates inputs based on that."
				glob [object!]
				/local  item type spec input default
			][	
				vin/tags ["!glob[" glob/valve/type":"glob/sid "]/setup()"] [setup]
				glob/input: copy []
				glob/layers: copy []
				add-inputs glob
				add-layers glob
				
				glob/valve/setup-glob-type glob ; 
				vout/tags [setup]
			]
			
			
			;--------------------
			;-    setup-glob-type()
			;--------------------
			setup-glob-type: func [
				"just a handy way to allow special glob inits after internal glob setup"
				glob [object!]
			][
				
			]
			
			
			;--------------------
			;-    destroy()
			;--------------------
			destroy: func [
				""
				glob [object!]
				/local item dummy
			][
				;von
				vin/tags ["!glob/destroy()"] [destroy]
				vprint "destroy glob-type stuff"
				glob/valve/destroy-glob-type glob
				
				vprint "destroy gels or layers"
				foreach item glob/layers [
					if item [
						item/valve/destroy item
					]
				]
				clear head glob/layers
				glob/layers: none
				
				vprint "destroy inputs"
				foreach [dummy item] glob/input [
					item/valve/destroy item
				]
				clear head glob/input
				glob/input: none
				
				
				!plug/valve/destroy glob
				
				if object? glob/reflection [
					glob/reflection/valve/destroy glob/reflection
				]
				
				glob/draw-spec: none
				
				vout/tags [destroy]
				;voff
			]
			
			;--------------------
			;-    destroy-glob-type()
			;--------------------
			destroy-glob-type: func [
				"Destroy what was created by setup-glob-type()"
				glob
			][
				vin/tags ["destroy-glob-type()"] [destroy-glob-type]
				
				vout/tags [destroy-glob-type]
			]
			
			;--------------------
			;-    link()
			;--------------------
			; note, current version of glob only supports (and expects) links to other globs
			link: func [
				observer [object!]
				subordinate [object!]
				/head "setting this refinement, tells the engine to put the link at the head of all subordinates"
				/reset "unlink and unpipe node prior to link"
				/local layer stack i
			][
				vin ["!glob/link()"]

				; first start by doing liquid link of globs themselves.
				any [
					all [reset head (!plug/valve/link/head/reset observer subordinate true)]
					all [reset (!plug/valve/link/reset observer subordinate true)]
					all [head (!plug/valve/link/head observer subordinate true)]
					!plug/valve/link observer subordinate
				]
				
				if none? observer/layers [
					observer/layers: copy []
				]
				
				vprobe length? subordinate/layers
				
				; here we assume your are providing proper plugs. 
				; you can only link empty globs or stack globs. 
				; If you link (the observer is) a gel glob, you are effectively 
				; corrupting your display, or an error might be (eventually) raised.
				i: 0
				foreach layer subordinate/layers [
					; make sure we have enough layers.
					i: i + 1
					unless stack: pick observer/layers i [
						vprint "ADDING LAYER"
						append observer/layers stack: liquify !stack
					]
					;probe length? observer/layers
					
					; let the stack observe the layer.
					any [
						;all [reset head stack/valve/link/reset/head stack layer true]
						;all [reset stack/valve/link/reset stack layer true]
						all [head (stack/valve/link/head stack layer true)]
						stack/valve/link stack layer
					]
				]
				
				;probe length? subordinate/layers
				
				; then link a stack for each element in the subordinate
				;probe length? subordinate/layers
				vout
			]
				
			
			;--------------------
			;-    unlink()
			;--------------------
			unlink: func [
				""
				plug [object!]
				/only oplug [object!]  "only supports glob pointers."
				/local layer i
			][
				vin/tags ["!glob/unlink()"] [unlink]
				either only [
				;	print type? plug
					;print type? plug/layers
					;probe length? plug/layers
					
					if oplug/layers [
						if plug/layers [
							i: 0
							foreach layer plug/layers [
								i: i + 1
								
								if oplug/layers/:i [
									layer/valve/unlink/only layer oplug/layers/:i
								]
							]
						]
					]
					!plug/valve/unlink/only plug oplug
				][
					if plug/layers [
						foreach layer plug/layers [
							layer/valve/unlink layer
						]
					]
					!plug/valve/unlink plug
				]
				
				vout/tags [unlink]
			]
			
			
			;--------------------
			;-    cleanse()
			;--------------------
			cleanse: func [
				""
				plug [object!]
			][
				vin/tags ["!glob:" plug/valve/type"[" plug/sid "]/cleanse()"] [cleanse]
				plug/liquid: copy/deep []
				;plug/input/offset/valve/fill plug/input/offset 0x0
				vout/tags [cleanse]
			]
;
;			;--------------------
;			;-    feel[]
;			;--------------------
;			feel: context [
;				;--------------------
;				;-        on-key()
;				;--------------------
;				; triggered if focus is false or if it returns false and the mouse is over the item 
;				;--------------------
;				on-key: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					; does this consume the event? (some keys could be considered control keys later on... needs more reflection)
;					true
;				]
;
;
;				;--------------------
;				;-        on-type()
;				;--------------------
;				; this is used when glob has focus
;				;--------------------
;				on-type: func [
;					glob [object!]
;					;canvas [object!]
;					event
;					;offset  [pair!]  "precalculated position removing any offset, origin"
;				][
;					
;					; does this consume the event? (some keys could be considered control keys later on... needs more reflection)
;					true
;				]
;				
;				
;				
;				;-----------------
;				;-        on-scroll()
;				;-----------------
;				on-scroll: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					vin [{"" glob/valve/type "[" glob/sid "]/ON-SCROLL()}]
;					vout
;				]
;				
;				
;				
;				
;	
;				
;				;--------------------
;				;-        on-over()
;				;--------------------
;				; when the mouse goes from elsewhere TO this item
;				;--------------------
;				on-over: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					vprint ["" glob/valve/type "[" glob/sid "]/ON-OVER()"]
;					;false
;					vout
;				]
;				
;				
;				;--------------------
;				;-        on-top()
;				;--------------------
;				; when the mouse continues hovering over the item
;				;--------------------
;				on-top: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					vprint ["" glob/valve/type "[" glob/sid "]/ON-TOP()"]
;					;false
;					vout
;				]
;				
;				
;				;--------------------
;				;-        on-away()
;				;--------------------
;				; when the hover leaves the item
;				; on-away() and on-over() are ALWAYS called in pair.
;				;--------------------
;				on-away: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					vprint ["" glob/valve/type "[" glob/sid "]/ON-AWAY()"]
;					;false
;					vout
;				]
;				
;				
;				;--------------------
;				;-        pick?()
;				;--------------------
;				; return true IF this item can be picked for drag and drop.
;				; 
;				; if so, drag events occur.
;				; 
;				; otherwise normal hover continues and on select is called instead
;				;--------------------
;				pick?: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					false
;				]
;				
;
;
;				;--------------------
;				;-        on-select()
;				;--------------------
;				; only triggered when pick? isn't true
;				;--------------------
;				on-select: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					vin/tags ["glob/valve/feel/on-select()"] [on-drag]
;					
;					vout/tags [on-drag]
;				]
;				
;
;				;--------------------
;				;-        on-release()
;				; occurs when mouse was released but no drag occured
;				;--------------------
;				on-release: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					vin/tags ["glob/valve/feel/on-select()"] [on-drag]
;					
;					vout/tags [on-drag]
;				]
;				
;				
;				;-----------------
;				;-        on-pick()
;				;-----------------
;				; called when mouse is pressed over item and pick? returned true
;				;-----------------
;				on-pick: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					vin [{glob/valve/feel/on-pick()}]
;					vout
;				]
;				
;				
;				
;				;--------------------
;				;-        on-drag()
;				;--------------------
;				on-drag: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					vin "glob/valve/feel/on-drag()"
;					
;					vout
;					false
;				]
;				
;				
;				
;				;--------------------
;				;-        on-drop()
;				;--------------------
;				; only called when mouse is released and drag occured (complements on-pick)
;				;--------------------
;				on-drop: func [
;					glob "the glob object itself"
;					canvas "face this glob was viewed from (a glob can be used on several canvases)"
;					event "original view event, note event/offset is window related"
;					offset "offset relative to canvas"
;				][
;					vin [{glob/valve/feel/on-drop()}]
;					vout
;					false
;				]
;				
;				
;				
;				;--------------------
;				;-        on-down()
;				;--------------------
;				on-down: func [
;					""
;					gadget
;					canvas
;					event
;				][
;					false
;				]
;
;
;				;--------------------
;				;-        on-up()
;				; up is called everytime a mouse button is released, in all cases.
;				;--------------------
;				on-up: func [
;					""
;					gadget
;					canvas
;					event
;				][
;					false
;				]
;
;
;				;--------------------
;				;-        on-alt-down()
;				;--------------------
;				on-alt-down: func [
;					""
;					gadget
;					canvas
;					event
;				][
;					false
;				]
;
;
;				;--------------------
;				;-        on-alt-up()
;				;--------------------
;				on-alt-up: func [
;					""
;					gadget
;					canvas
;					event
;				][
;					false
;				]
;
;
;
;				
;				
;			]

		]
		
		

	]
	
	
	;-  
	;------------------------
	;;- !GLOB-EX
	;------------------------
	; quick example of a glob
	;------------------------
	!glob-ex: make !glob [
		valve: make valve [
			;-    input-spec:
			input-spec: [
				; list of inputs to generate automatically on setup  these will be stored within the instance under input
				start !pair (random 30x30)
				end !pair ((random 30x30) + 30x30)
				offset !pair
				color !color
				hi !state
			] 
			
			
			;-    gel-spec:
			gel-spec: [
				; mask
				start offset end 
				[line-width 3 pen (to-color gel/plug-sid) line (data/start= + data/offset=) (data/end= + data/offset=)]
				
				; line
				color start offset end hi
				[line-width 2 pen (either data/hi= [data/color= * 1.25 + 30.30.30][data/color=]) line (data/start= + data/offset=) (data/end= + data/offset=)]
				
				; dots
				color start offset end
				[line-width 2 pen (data/color=) circle (data/start= + data/offset=) 5  circle (data/end= + data/offset=) 5]
				
			]
		]
	]
	
	

	
	;-  
	;- !rasterizer:
	;
	;
	; inputs
	;     size:     size of output image
	;     offset:   offset to apply to draw block
	;     bg-color: the image is flushed with this color at each refresh.
	;     drawing:  an AGG draw block to rasterize
	;
	; optional inputs
	;     drawing:  any number of AGG draw blocks to rasterize
	;       ...
	;
	; notes:
	;     inputs MUST be of proper type.
	;     usually we connect the drawing to a layer of a glob directly.
	;
	!rasterizer: make !plug [
		; plug/liquid : image!
		
		;-     image:
		; we store the image to use so we can refer to it later and re-allocate it
		; when image size changes.
		image: none
		
		valve: make valve [
			;-----------------
			;-     process()
			;-----------------
			process: func [
				plug
				data
				/local img-size img-offset drawing drawings bg
			][
				vin [{!rasterizer/process()}]
				img-size: pick data 1
				img-offset: pick data 2
				bg: pick data 3
				
				; compose drawing
				drawings: at data 4
				drawing: clear []
				foreach item drawings [
					append drawing item
				]
				
				; create or reuse image
				plug/image: any [
					all [
						image? plug/image
						img-size = plug/image/size
						plug/image
					]
					make image! img-size
				]
				
				;print "================================================="
				;print "rasterizer/process()"
				;print type? plug/image
				;view/new layout [image plug/image]
				;print "================================================="
				
				
				;<TO DO> we might support IMAGE! bg at some point.
				
				; reset image background
				plug/image/rgb: bg
				
				plug/image/alpha: any [pick bg 4 0]
				
				; render image
				draw plug/image compose [translate (img-offset) (drawing)]
;				draw plug/image compose [merge rotate (img-offset) (drawing)]
				
				
				
				; assign liquid
				plug/liquid: plug/image
				
				vout
			]
			
		
		]
	]
	
	
	
	;-  
	;-  !IMGSTORE
	; use this liquid to always have an up to date image representation of a !glob network.
	; nice thing is that you can link it at any stage and on any layer  :-)
	;
	; since it is on demand, causing massive amounts of change to stack, will not
	; bog down the imgstore, since it will basically do nothing, until the img really
	; is needed.
	!imgstore: make !plug [
		buffer: none
		draw-blk: none ; stores the draw effect we are rendering
		
		
		valve: make valve [
			type: '!imgstore
			
			;--------------------
			;-    setup()
			;--------------------
			setup: func [
				""
				plug [object!]
			][
				vin/tags ["!imgstore/setup()"] [setup]
				plug/buffer: make face copy/deep [
					size: 0x0
					edge: none
					color: white ; reaching white is almost impossible (16 million nodes) 
					feel: none
					font: none 
					text: none 
					effect: [draw []]
				]
				
				; preset our block... just for speed.
				plug/draw-blk: plug/buffer/effect/2
				append plug/draw-blk [anti-alias #[false]]
				vout/tags [setup]
			]
			
			;--------------------
			;-    cleanse()
			;--------------------
			cleanse: func [
				""
				plug [object!]
			][
				vin/tags ["!imgstore/cleanse()"] [cleanse]
				plug/liquid: make image! 10x10
				vout/tags [cleanse]
			]
			
			;--------------------
			;-    process()
			;--------------------
			process: func [
				""
				plug [object!]
				data [block!]
				/local img size blk
			][
				vin/tags ["!imgstore/process()"] [process]
				blk: plug/draw-blk
				
				; keep the anti-alias word
				clear at blk 3
				insert data/3 [ IMAGE-FILTER NEAREST ]
				
				append blk reduce [ 'translate data/3]
				
				if all [
					pair? pick data 1  ; the size of the canvas 
				][
					foreach item next data [
						if block? item [
							append blk item
						]
					]
					plug/liquid: draw make image! reduce [first data white] blk
				]
				
				vout/tags [process]
				plug/liquid
			]
		]
	]
]


;------------------------------------
; We are done testing this library.
;------------------------------------
;
; test-exit-slim
;
;------------------------------------

