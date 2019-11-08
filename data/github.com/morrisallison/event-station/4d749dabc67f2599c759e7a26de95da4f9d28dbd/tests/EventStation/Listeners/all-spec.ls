expect = require 'must'
Promise = require 'bluebird'

EventStation = require '../../../src/main' .default

describe 'Listeners#all()', (,) !->

    station = undefined
    listeners = undefined

    beforeEach !->
        EventStation.inject 'Promise', Promise
        station := new EventStation
        listeners := station.on 'boom pow bash', !->

    afterEach !->
        EventStation.reset()

    it 'must throw an error if promises are not available', !->
        EventStation.inject 'Promise', undefined

        check = !->
            listeners.all()

        check.must.throw Error

    it 'must make a promise that resolves when all of the listeners have been applied at least once', (,) !->
        all = listeners.all()

        station.emit 'boom pow bash'

        return expect(all).to.then.have.length 3
