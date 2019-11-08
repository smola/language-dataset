REBOL [
	; -- Core Header attributes --
	title: "crypto | making encryption simple"
	file: %crypto.r
	version: 1.0.1
	date: 2013-9-12
	author: "Maxim Olivier-Adlhoch"
	purpose: {Methods which automatically encrypt/decrypt data/files/paths.}
	web: http://www.revault.org/modules/crypto.rmrk
	source-encoding: "Windows-1252"
	note: {slim Library Manager is Required to use this module.}

	; -- slim - Library Manager --
	slim-name: 'crypto
	slim-version: 1.2.1
	slim-prefix: none
	slim-update: http://www.revault.org/downloads/modules/crypto.r

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
		2013-01-03 - v1.0.0
			-creation.
			-added basic encryption routines supporting the strongest encryption by default allowing alternatives if required.

		v1.0.1 - 2013-09-12
			-License changed to Apache v2}
	;-  \ history

	;-  / documentation
	documentation: ""
	;-  \ documentation
]









;--------------------------------------
; unit testing setup
;--------------------------------------
;
; test-enter-slim 'crypto
;
;--------------------------------------

slim/register [
	
	slim/open/expose 'utils-codecs none [ bit  i32-to-binary load-i32 ]
	
	
	;--------------------------
	;- hash-key()
	;--------------------------
	; purpose:  a simple but cryptographically secure key hashing mechanism.
	;
	; inputs:   
	;
	; returns:  
	;
	; notes:    -returns a 128-bit key by default
	;           -we include this before GLOBALS section because we use it to setup the globals.
	;           -you may replace this function if you want your own key hashing algorithm.
	;
	; tests:    
	;--------------------------
	hash-key: funcl [
		data [string!]
		/bits b "number of significant bits required... output will be sized to closed factor of 8 which can hold this number of significant bits (max 160 bits)"
	][
		if all [
			bits
			b > 160
		][
			to-error "cannot generate keys larger than 160 bits"
		]
		
		b: any [
			all [ bits   min ( to-integer ((bits + 7) / 8) ) 20  ]
			
			16
		]
		
		copy/part checksum/secure data 16
	]
	
	
	
	
	
	;-----------------------------------------------------------------------------------------------------------
	;
	;- GLOBALS
	;
	;-----------------------------------------------------------------------------------------------------------
	
	;---
	; These below values are used by all methods in this library, so you only need to setup once and call all crypto
	; stuff repeatedly without fear of mangling things.
	;---
	
	;--------------------------
	;-     lib-crypto-private-key:
	;
	;  A string to use as your encryption key.
	;
	;  Note that crypto lib will join the following salt to all key generation or use.
	;
	;  Also note that you should always use HASH-KEY()  (or improved key hashing) to generate better
	;  strength keys.
	;--------------------------
	lib-crypto-private-key:  hash-key "kjkjg 438394gh slkg jfg5sdg f./ga';[]'	;/'.[."
	
	
	;--------------------------
	;-     lib-crypto-salt-key:
	;
	;  The second part of the cryptographic key is always joined to the private key when using symmetrical encryption.
	;
	;  notes: 
	;     -when using public/private keys, this is used when GENERATING the public/private key pair
	;     -you should change this within your application/workplace so that Salts are not the same for every user of
	;      this module.
	;
	;  Also note that you should always use HASH-KEY()  (or improved key hashing) to generate better
	;  strength keys.
	;--------------------------
	lib-crypto-salt-key:  hash-key "huig34984gth.';';.` sng guio 040 983ew9g8h aq /'; ." 
	
	
	;--------------------------
	;-     crypt-method:
	;
	;  what encryption to use by default for all library methods
	;  value can be  [   blowfish   |   rijndael  ]
	;
	; note that rijndael is also known as AES (it is the same encryption algorythm)
	;--------------------------
	crypt-method: 'rijndael
	
	
	
	
	
	
	;--------------------------
	;-     flag enum
	;--------------------------
	FLAG-COMPRESSED: bit 1  
	FLAG-MOLDED:     bit 2
	FLAG-BLOWFISH:   bit 3
	FLAG-STRING:     bit 4
	
	
	
	
	
	
	;-----------------------------------------------------------------------------------------------------------
	;
	;- GLOBALS
	;
	;-----------------------------------------------------------------------------------------------------------
	
	
	;--------------------------
	;- encrypt()
	;--------------------------
	; purpose:  given data, will return a binary which can then be used through decrypt.
	;
	;           note that the returned binary may in fact include extra rebol information
	;           which was used to compress the given data before encrypting it.
	;
	;           this is why encrypt may return output which is significantly smaller than the input
	;
	; inputs:   
	;
	; returns:  
	;
	; notes:    when data is not string! or binary!, mold/all is used on it, which may destroy
	;           some datatypes.  
	;
	;           always make sure you test decrypt with your data.
	;
	;           at the head of the returned data there is a header which is added to know how to decrypt it.
	;
	; tests:    
	;--------------------------
	encrypt: funcl [
		data
		/rijndael "explicitely use rijndael encryption"
		/AES "alternate name for /rijndael refinement"
		/blowfish "use blowfish encryption"
		/no-compress "prevents compression when it may be done"
		/key ekey "provide the key manually, should be 128, 196 or 256 bits (16, 24 or 32 bytes) in length "
	][
		vin "encrypt()"
		start: now/precise
		
		
		; turn refinements into logic! values.
		rijndael: true? rijndael
		AES: true? AES
		blowfish: true? blowfish
		
		
		if blowfish AND (rijndael or AES) [
			to-error "cannot specify two encryption methods at once"
		]
		
		flags: 0
		
		
		
		;----
		; remember to decrypt the data back as a string
		if string? data [
	;		vprint "source was a string"
			flags: flags OR FLAG-STRING
		]
		
		;----
		; mold data since it wasn't a string.
		unless any [
			string? data
			binary? data
		][
	;		vprint "molding..."
			; remember to load it back after.
			flags: flags OR FLAG-MOLDED
			data: mold/all data
		]
		
	;	vprint ["length of input: " bytecount data]
		
		;----
		; compress data?
		unless no-compress [
	;		vin "compressing..."
			d-len: length? data
			cstart: now/precise
			cdata: compress data
	;		vprint ["took: " difference now/precise cstart]
			c-len: length? cdata
	;		v?? d-len
	;		v?? c-len
			either d-len <= c-len [
	;			vprint "compressed value is larger, compression disabled"
			][
	;			vprint ["compressed size: " bytecount cdata]
				data: cdata
				cdata: none ; GC friendly.
				
				; remember to de-compress it
				flags: flags OR FLAG-COMPRESSED
			]
	;		vout
		]
		
		data: as-binary data
		
		;----
		; select encryption method
		cmethod: either any [
			AES or rijndael 
			crypt-method = 'rijndael
		][
			'rijndael
		][
			'blowfish
			flags: flags OR FLAG-BLOWFISH
		]
		
		
		
		
		;----
		; setup cryptographic port
		cstart: now/precise
		
		crypt-port: make port! [
			scheme: 'crypt
			strength: 256
			padding: true
			algorithm: cmethod
			direction: 'encrypt
			key: any [ 
				ekey 
				join lib-crypto-private-key lib-crypto-salt-key 
			]
		]
		
		;----
		; turn flags into a binary so we can embed it within the output
		flags: i32-to-binary flags
		
	;	v?? flags
		
		;----
		; encrypt data
		open crypt-port
		insert crypt-port data
		update crypt-port 
		data: join flags copy crypt-port
		close crypt-port
	;	vprint ["took: " difference now/precise cstart]
	;	vout	
		
	;	vprint ["timing of whole encryption: " difference now/precise start]
	;	vprint ["length of output: " bytecount data]
		
		vout
		first reduce [data data: none]
	]
	
	
	
	;--------------------------
	;- decrypt()
	;--------------------------
	; purpose:  
	;
	; inputs:   
	;
	; returns:  
	;
	; notes:    required data encrypted by encrypt()
	;
	; tests:    
	;--------------------------
	decrypt: funcl [
		data [binary!]
		/key ekey "provide the key manually, should be 128, 196 or 256 bits (16, 24 or 32 bytes) in length "
	][
		vin "decrypt()"
		start: now/precise
	;	vprint ["length of input: " bytecount data]
		
		;----	
		; load and skip the flags 
		flags: load-i32 copy/part data 4
		;data: at data 5
		
		data: at data 5 
		
		;----
		; analyse the flags
		cmethod: pick  [ 'blowfish 'rijndael ] not (0 = (flags AND FLAG-BLOWFISH))
		molded?:      not 0 = (flags AND FLAG-MOLDED)
		compressed?:  not 0 = (flags AND FLAG-COMPRESSED)
		was-string?:  not 0 = (flags AND FLAG-STRING)
		
	;	v?? cmethod
	;	v?? molded?
	;	v?? compressed?
	;	v?? was-string?
		
			
		;----
		; setup cryptographic port
	;	crypt-port/algorithm: cmethod
	;	crypt-port/direction: 'decrypt
	;	crypt-port/key: any [
	;		ekey 
	;		join lib-crypto-private-key lib-crypto-salt-key 
	;	]
		
		
		;----
		; decrypt the data
		crypt-port: make port! [
			scheme: 'crypt
			direction: 'decrypt
			algorithm: cmethod
			strength: 256
			padding: true
			key: any [ 
				ekey 
				join lib-crypto-private-key lib-crypto-salt-key 
			]
		]
	
	;	vin "decrypting..."
		cstart: now/precise
	
		open crypt-port
		insert crypt-port data
		update crypt-port 
		data: copy crypt-port
		close crypt-port
	;	vprint ["took: " difference now/precise cstart]
	;	vout
		
	
		;----
		; decompress data
		if compressed? [
	;		vin "decompressing..."
			cstart: now/precise
			data: decompress data
	;		vprint ["took: " difference now/precise cstart]
	;		vout
		]
		
		;----
		; return data as was given to encrypt
		either molded? [
			help reloading data
			data: load as-string data
		][
			either was-string? [
				data: as-string data
			][
				data: as-binary data
			]
		]	
	
	;	vprint ["timing of whole decryption: " difference now/precise start]
	;	vprint ["length of output: " bytecount data]
	
		vout
		
		first reduce [data data: none]
	]
	
	
	
	
	;--------------------------
	;- crypto-path()
	;--------------------------
	; purpose:  given a normal file path, return an obfuscated path
	;
	; inputs:   
	;
	; returns:  
	;
	; notes:    only the filename part of the path is changed.
	;
	; tests:    
	;--------------------------
	crypto-path: funcl [
		path [file!]
	][
		vin "crypto-path()"
		v?? path
		
		
		if none? file-part path [
			to-error "Crypto path cannot be used on directory paths"
		]
		
		
		dir: any [
			dir-part path   
			%./
		]
		
		file: file-part path
		v?? dir
		v?? file
		if file  [
			; for production, the file name is completely obfuscated
			epath: copy/part extract (at encrypt form file 9) 2 20
			
			
			cfile: replace/all rejoin [
				;-----
				; remove following comment to put the file name at head of path
				; (used for debugging)
				;-----
				;form file "_" 
	
				%cwapi- form enbase/base epath 16
			] "^/" ""
			v?? cfile
		]
		
		vout
		
		join dir any [ cfile "" ]
	]
	
	
	
	;--------------------------
	;- store()
	;--------------------------
	; purpose:  
	;
	; inputs:   
	;
	; returns:  
	;
	; notes:    
	;
	; tests:    
	;--------------------------
	store: funcl [
		path [file!]
		data
	][
		vin "store()"
		path: crypto-path path
		vout
		write/binary path encrypt data
	]
	
	
	
	;--------------------------
	;- retrieve()
	;--------------------------
	; purpose:  
	;
	; inputs:   
	;
	; returns:  
	;
	; notes:    
	;
	; tests:    
	;--------------------------
	retrieve: funcl [
		path [file!]
	][
		vin "retrieve()"
		path: crypto-path path
		v?? path
		vprobe clean-path path
		vout
		decrypt read/binary path
	]
	
	
]


;------------------------------------
; We are done testing this library.
;------------------------------------
;
; test-exit-slim
;
;------------------------------------

