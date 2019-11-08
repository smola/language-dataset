//////////////////////////////////////////////////////////////////////////////////////////////
//qinfra.q - contains functions for Q infrastructure
///
//

.qinfra.loadDep:{[m;p]
    if[exec count i from .qinfra.priv.dependStack where module=m, path like p;
        delete from `.qinfra.priv.dependStack;
        '`$"cyclying dependency";
        ];

    `.qinfra.priv.depend upsert (m;p);
    `.qinfra.priv.dependStack insert (m;p); // enqueue
    dependTxt:`$p, "/", "depends.txt";
    if[not () ~ key hsym dependTxt;
        dep:("SS"; " ") 0:dependTxt;
        .z.s'[first dep;string last dep]; // recursive stack
        ];
    delete from `.qinfra.priv.dependStack where module = m, path like p; // dequeue
    };

.qinfra.cleanDep:{
    delete from `.qinfra.priv.depend;
    };

.qinfra.listDep:{
    .qinfra.priv.depend
    };

.qinfra.addDep:{[m;p]
    `.qinfra.priv.dependStack upsert (m;p);
    };

.qinfra.getDep:{
    exec first path from .qinfra.priv.depend where module = x
    };

.qinfra.load:{[m]
    .qinfra.include[m;"module.q"];
    };

.qinfra.import:{[ns]
    emptyNS:enlist[`]!enlist (::);
    $[() ~ key ns; ns set emptyNS;
        if[not ` in key ns; ns set emptyNS, value ns;]
        ];
    };

.qinfra.include:{[m;s]
    m:$[-11h=type m; m; `$m];
    s:$[-11h=type s; s; `$s];
    s:$[null m; string s; .qinfra.getDep[m], "/", string s];
    .qinfra.priv.include[m;s];
    };

.qinfra.clean:{[ns]
    delete from ns;
    };

.qinfra.clear:{
    delete from `.;
    };

.qinfra.lsg:{
    globals:value `.;
    ([] vars:key globals; values:value globals)
    };

.qinfra.lsn:{
    flip enlist[`ns]!enlist key `
    };

.qinfra.listModule:{
    .qinfra.priv.module
    };

.qinfra.reload:{
    exec .qinfra.priv.include'[module;script] from .qinfra.priv.module;
    };

.qinfra.priv.include:{[m;s]
    value "\\l ", s;
    $[0 = exec count i from .qinfra.priv.module where module=m, script like s;
        `.qinfra.priv.module insert (m;s;.z.p);
        update time:.z.p from `.qinfra.priv.module where module=m, script like s
        ];
    };

.qinfra.init:{
    .qinfra.import[`.qinfra];

    if[()~key `.qinfra.priv.module;
        .qinfra.priv.module:([] module:`$(); script:(); time:"p"$());
        ];

    if[()~key `..qinfra.priv.depend;
        .qinfra.priv.depend:([module:`$()] path:());
        .qinfra.priv.dependStack:([] module:`$(); path:());
        ];

    if[`depend in key .Q.opt .z.x;
        .qinfra.loadDep[`;ssr[(raze/) .Q.opt[.z.x]`depend;"\\";"/"]];
        ];

    if[`init in key .Q.opt .z.x;
        .qinfra.include[`;ssr[(raze/) .Q.opt[.z.x]`init;"\\";"/"]];
        ];
    };

.qinfra.init[];