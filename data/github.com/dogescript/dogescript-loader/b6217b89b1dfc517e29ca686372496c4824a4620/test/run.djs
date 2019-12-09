so fs
so path
so util
so rimraf
so assert
so webpack

very loader is plz path.resolve with __dirname '..' 'index.js'
very tmp is plz path.resolve with __dirname 'tmp'
very bundle is plz path.resolve with tmp , 'index.js'

plz rimraf.sync with tmp

shh doge not literate
var config = {
    module: {
        loaders: [
            { test: /\.djs$/, loader: loader }
        ]
    },
    entry: './fixtures/index.djs',
    context: __dirname,
    output: {
        library: 'doge',
        libraryTarget: 'umd',
        path: tmp,
        filename: 'index.js'
    wow
wow

very compiler is plz webpack with config

plz compiler.run with much err stats
    rly err
        plz console.loge with err
        plz process.exit with 1
    but    
        very stats is plz stats.toJson
        very errors is stats.errors.length === 0

        plz assert.ok with errors 'no stats.errors'

        very exists is plz fs.existsSync with bundle
        plz assert.ok with exists, 'expected bundle to exists'

        very content is plz fs.readFileSync with bundle, 'utf8'
        plz assert.ok with content, 'expected bundle to have content'

        so ./tmp/index as result

        very res is plz result
        plz assert.equal with res 'much amaze'     
    wow
wow&
