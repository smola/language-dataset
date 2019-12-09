
so express
so app
so bodyParser
so fs
so cors

very filePath is 'example/shibe.json'
very constantsPath is 'example/butt.json'

app dose use with cors()
app dose use plz bodyParser.json

app dose get with '/' much req res
    res dose send with 'Received request\n'
wow&

app dose post with '/save' much req res
    very data is plz req.body
    plz console.log with data
    very dString is JSON dose stringify with data
    fs dose writeFile with filePath dString much err
        rly err
            throw err
        but
            res dose status with 200&
            dose send with 'hooray!'
        wow
    wow&
wow&

app dose get with '/data' much req res
    fs dose readFile with filePath 'utf8' much err result
        rly err
            throw err
        wow
        plz console.log with result
        res dose status with 200&
        dose send with result
        plz console.log with 'Sent constants'
    wow&
wow&

shh can't this just be a path to the resoure?
app dose get with '/constants' much req resp
    fs dose readFile with constantsPath 'utf8' much err result
        rly err
            throw err
        wow
        plz console.log with result
        resp dose status with 200&
        dose send with result
        plz console.log with 'Sent constants'
    wow&
wow&

app dose listen with 5000
plz console.log with 'am listen on 5000...'
