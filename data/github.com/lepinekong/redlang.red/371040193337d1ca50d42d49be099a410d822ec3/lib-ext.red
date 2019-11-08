Red [
    Title: "lib-ext.red"
]

.do: function[.src [file! block! url!] /root-path .root-path [file!] /dependencies .dependencies-vars [block!]
/read-only /get-folder

][
    {
        ## .do execution flow:
        - [X] [0]. Init root-path to (.system.path)
            - [X] [0.1]. Unless root-path is being changed (/root-path)
                - [X] [0.1.1]. Memorize root-path
            - [X] [0.2]. Unless there is a previous root-path memorized (see [0.1.1])
        - [x] [1]. if dependency file is found, execute it
        - [x] [2]. if not found
            - [ ] [2.1]. if depency file exists in (.system.path), execute it ; TODO: 2. MEDIUM: Forgot to implement flow 2.1
            - [x] [2.2]. otherwise
                - [x] [2.2.1]. if file exists in .code/.core/, execute it
                - [x] [2.2.2]. Otherwise
                    - [x] [2.2.2.1]. if fileName contains any keyword in dependencies-core: [override | utils | facilities], 
                        look for file into .code/(system)/
                    - [x] [2.2.2.2]. if fileName contains any keyword in dependencies-domains: [kms | pm | apps], 
                        look for file into .domains/(system)/  
                    - [x] [2.2.2.3]. For all cases 1. and 2. just above         
                        - [x] [2.2.2.3.1]. if found in domain, execute it
                        - [x] [2.2.2.3.2]. if not found in domain, look for it into domain subfolders of the file
                            - [x] [2.2.2.3.2.1]. if found in any domain subfolders, execute it in each subfolder it is found
                            - [x] [2.2.2.3.2.2]. Otherwise, alert of error unless .system.silence.error is true
        #### Example:     

            .do %.system.utils.red  

        #### TODO:
        - [x] [1]. HIGH: Fix bug like: git/.system.apps.markdown.typora.red could not be found.
        - [x] [2]. MEDIUM: Forgot to implement flow 2.1
        - [ ] [3]. LOW: case .src [block!]
        - [x] [4]. HIGH: Fix BUG .system.apps.internet.favorites.red  ☞ 58e2b881-46e0-4aee-9f76-127ee68625f8

        #### HISTORY:
        - [x] [1]. supporting /root-path for supporting multiple systems (.system.user etc.)
        - [X] [2]. making root-path persistent
    
    }

    if block? .src [
        .src: rejoin .src 
    ]
    .src: to-red-file .src

    .do-read: function [filepath /read-only /get-folder][

        either get-folder [
            set [folder=>] split-path filepath
            return folder=>
        ][
            if error? try [

                src: read filePath ;[2.2.2.3.2.1] execute it in each subfolder it is found
                .system/context/script: filePath
                
                either read-only [
                    return src
                ][
                    do src
                ]
                
            ][

                msg: rejoin ["error executing " fileName]
                print msg
                if error? try [
                    .speak msg
                ][
                    ask ""
                ]
            ]
        ]


    ]

    .do-file-in-domains: function[filepath][

        ;/C/rebol/.system.user/.code/.domains/.apps/.system.user.apps.countdown.red
        filePath: to-red-file filePath ; [2.2.2.3]
        either exists? filePath  [ ; [2.2.2.3.1]. if found,

            get-static-previous-file-path (filePath)

            either get-folder [
                return .do-read/get-folder filepath
            ][

                either read-only [
                    return .do-read/read-only filePath ; [2.2.2.3.1]. execute it
                ][

                    .do-read filePath ; [2.2.2.3.1]. execute it
                ]
            ]
            flag-found: true ;!!!!!!!!!!!!!!!  Should not have been there
            get-static-file-path (filePath)
            return true
        ][ 
            ; [2.2.2.3.2]. if not found in domain
            flag-found: false ; for [2.2.2.3.2.2]. if NOT found in any domain subfolders, alert of error unless .system.silence.error is true           
            
            ; [apps]
            forall domains-found [ 

                domain: domains-found/1 
                ;%/C/rebol/.system.user/.code/.domains/.apps/
                folder: rejoin [root-path ".code/" system "/" "." domain "/" ] ; [2.2.2.3.2]

                ;[%.system.user.apps.clock.red %.system.user.apps.red %clock/ %libRedRT-defs.r %libRedRT-extras.r %libRedRT-include.red %libRedRT.dll %vscode/]
                subfolders: read folder ; [2.2.2.3.2]

                forall subfolders [ ; [2.2.2.3.2]. look for it into domain subfolders of the file
                    subfolder: subfolders/1 
                    if dir? subfolder [

                        use [filePath msg][
                            filePath: rejoin [folder subfolder fileName]                         

                            if exists? filePath [ ;[2.2.2.3.2.1]. if found in any domain subfolders,

                                either get-folder [
                                    return .do-read/get-folder filepath
                                ][
                                    either read-only [
                                        return .do-read/read-only filePath ;[2.2.2.3.2.1] execute it in each subfolder it is found 
                                    ][
                                        .do-read filePath ;[2.2.2.3.2.1] execute it in each subfolder it is found 
                                    ]  
                                ]

                                flag-found: true
                                get-static-file-path (filePath)
                                return true
                            ] 
                        ]

                    ]

                ] 

            ]

            if flag-found = false [ ; [2.2.2.3.2.2]. otherwise, 

                unless .system.silence.error [; [2.2.2.3.2.2] unless .system.silence.error is true
                    print rejoin [fileName " could not be found."]  ; [2.2.2.3.2.2]. alert of error
                    probe get-static-file-path ""
                    ask "Press a key..."   
                ]
                
            ]                             

        ]    
    ]    

    do-trace-file: function [line-number [integer!] block [block!] file [file! word!] ][

        {
            ##  execution flow:
                - [x] [1]. 

            #### Example:
                do-trace-file %.system.coder.vscode.install.red 212 [
                ]

            #### TODO:
            1. 

        }

        if word? file [
            file: to-red-file form file
        ]

        if (.src = file) [
            probe file
            do block
            ;probe system/options/path
            ;probe system/script/path
            ask line-number
        ]    
    ]   

    -root-path: false

    either root-path [
        -root-path: true
        root-path: .root-path
        get-static-root-path (root-path) 
    ][
        static-root-path: get-static-root-path ""
        either none? static-root-path [
            either value? '.system.path [
                root-path: .system.path
            ][
                root-path: %/c/rebol/.system/.code/.domains/.apps/installers/
            ]
            
        ][
            root-path: static-root-path
        ]

    ]

    either dependencies [
        get-static-dependencies-vars (.dependencies-vars)

        <=dependencies-vars: .dependencies-vars
    ][
        
        <=dependencies-vars: get-static-dependencies-vars ""
        if <=dependencies-vars = none [
            <=dependencies-vars: [.system.dependencies-core .system.dependencies-domains]
        ]
    ]

    switch/default type?/word get/any '.src [
    file! url! [

        either exists? .src [ ; [1]. if dependency file is found,

            either read-only [
                return src
            ][
                
                do .src ; [1] execute it
            ]
            
        ][

            ; [2]. if not found

            ; weird impossible to trace see git 7ffd12f916512cbefd5ed21f53083ecff392945f
            ; do-trace-file %.system.apps.internet.favorites.red 116 [
            ; ]

            fileName: form .src    

            ; flow 2.1: if depency file exists in (.system.path), execute it
            ; TODO: 2. MEDIUM: Forgot to implement flow 2.1

            filePath: to-red-file rejoin [root-path ".code/" ".core/" fileName] ; [2.2.1]

            either exists? filePath [ ; [2.2.1]. if file exists in .code/.core/

                either get-folder [
                    return .do-read/get-folder filePath
                ][
                    either read-only [
                        return .do-read/read-only filePath ; ; [2.2.1]. execute it
                    ][
                        .do-read filePath ; ; [2.2.1]. execute it
                    ]
                ]
                
                get-static-file-path (filePath)       

            ][

                dependencies-vars: <=dependencies-vars
                dependencies-types: copy [] ; [2.2.2.3]. For all cases 

                ;[.system.coder.dependencies-core .system.coder.dependencies-domains]
                foreach var dependencies-vars [
                    ; [ languages apps pm kms ] 
                    dependencies-list: do var
                    append dependencies-types dependencies-list                                     
                ]

                ; ["" "system" "user" "apps" "countdown" "red"]
                splitted-fileName: split fileName "." ; [2.2.2.1] & [2.2.2.2]            

                flag-found: false
                ;[ libraries factories codeops override utils facilities languages apps pm kms ]
                forall dependencies-types [ ; [2.2.2.3]. For all cases 1. and 2. just above 
                    ; libraries 
                    keyword: dependencies-types/1 ; [2.2.2.1] & [2.2.2.2]
                    keyword-string: to-string keyword ; [2.2.2.1] & [2.2.2.2]                                                    

                    ; Example: if find ["" "system" "coder" "languages" "red"] "languages"
                    if find splitted-fileName keyword-string [ ; [2.2.2.1] & [2.2.2.2]                                           
                        system: copy "" ; [2.2.2.1] & [2.2.2.2]
                        domains-found: copy []          


                        ; [.system.coder.dependencies-core .system.coder.dependencies-domains]
                        foreach var dependencies-vars [
                            
                            ;[ languages apps pm kms ]
                            keywords-list: (do var)                       
                            
                            ; [ languages apps pm kms ] languages                             
                            if find keywords-list keyword [

                                ;".domains"
                                system: rejoin ["." last (split (to-string var) "-")]
                                append domains-found keyword
                                ;%/C/rebol/.system.coder/.code/.domains/.languages/.system.coder.languages.red
                                thefilePath: rejoin [root-path ".code/" system "/" "." keyword-string "/" fileName]

                                executed: .do-file-in-domains thefilePath
                                flag-found: true

                            ]
                        ]                       
                    
                        
                    ]

                ]               

                either flag-found = false [

                    ;%/C/rebol/.system.coder/.code/.domains/.languages/
                    context-path: get-static-previous-file-path ""     

                    ; do-trace-file %.system.coder.languages.redlang.red 422 [
                    ;     probe context-path
                    ; ] 

                    if (context-path not none) [
                        
                            ;%/C/rebol/.system.coder/.code/.domains/.languages/.system.coder.languages.redlang.red
                            thefilepath: to-red-file rejoin [context-path fileName]
                            ; do-trace-file %.system.coder.languages.redlang.red 430 [
                            ;     probe thefilepath
                            ; ]   
            
                            either (exists? thefilepath) [

                                either get-folder [
                                    .do-read/get-folder thefilepath
                                ][
                                    .do-read thefilepath
                                ]
                                
                                ;get-static-file-path (filepath)
                            ][
                                folder: context-path
                                subfolders: read folder ; [2.2.2.3.2]
                                
                                forall subfolders [ ; [2.2.2.3.2]. look for it into domain subfolders of the file
                                    subfolder: subfolders/1 
                                    if dir? subfolder [

                                        use [filePath msg][
                                            filePath: to-red-file rejoin [folder subfolder fileName]                       
                                            if exists? filePath [ ;[2.2.2.3.2.1]. if found in any domain subfolders,  

                                                either get-folder [
                                                    return .do-read/get-folder filePath
                                                ][
                                                    either read-only [
                                                        return .do-read/read-only filePath
                                                    ][
                                                        .do-read filePath ;[2.2.2.3.2.1] execute it in each subfolder it is found 
                                                    ] 
                                                ]

                                            ] 
                                        ]

                                    ]

                                ]                                 
                            ]
                    ]
                ][

                    ; ret: get-static-previous-file-path (get-static-file-path "")
                    ; do-trace-file %.system.coder.languages.red 419 [
                    ;     probe ret
                    ; ]                     

                ]
                
            ]           
        ]
    ]
    block! [

        forall .src [
            .do (.src/1)       
        ]

    ]
    ][
        throw-error 'script 'expect-arg .src
    ]
]

; dependencies

get-static-root-path:  function['.root-path [word! string! file! unset!] /local static-root-path][

    static-root-path: []

    switch/default type?/word get/any '.root-path [
        unset! string! word! [
            either 0 = length? static-root-path [
                return none
            ][
                return static-root-path/1
            ]
        ]
        file! [
            either 0 = length? static-root-path [
                append static-root-path .root-path
            ][
                static-root-path/1: .root-path
            ]

            return static-root-path/1
        ]
    ] [
        throw error 'script 'expect-arg .root-path
    ]
]

get-static-dependencies-vars:  function['.dependencies-vars [block! string! word! unset!] 
/local static-dependencies-vars static-counter][

    static-dependencies-vars: []

    static-counter: []

    either 0 = length? static-counter [
        append static-counter 0
    ][
        static-counter/1: static-counter/1 + 1
    ]    

    switch/default type?/word get/any '.dependencies-vars [
        unset! word! string! [
            either 0 = length? static-dependencies-vars [
                return none
            ][
                return static-dependencies-vars
            ]
        ]
        block! [
            static-dependencies-vars: copy .dependencies-vars
            return static-dependencies-vars
        ]
    ] [
        throw error 'script 'expect-arg .dependencies-vars
    ]
]