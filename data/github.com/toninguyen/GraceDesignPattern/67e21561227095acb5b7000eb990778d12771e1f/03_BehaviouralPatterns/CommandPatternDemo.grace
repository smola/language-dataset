//The command pattern is a design pattern that enables all of the information for a request to be contained within a single object. 
//The command can then be invoked as required, often as part of a batch of queued commands with rollback capabilities.
//Do I need to implement Command ? Method execute from Command is never called 

import "/Users/if0814/Dropbox/01_Master_Studium/04_Semester_NZ/03_Thesis/02_Implementation/list" as list

class Command {
    method execute {}
    var product
}

class Invoker {
    var commandList := list.with []
    
    //print "Object from Invoker created"
    
    method takeComamnd (command) {
        commandList.add (command)
    }

    method placeCommands{
        
        for (commandList) do { command ->
            command.execute 
        }
        //for (1 .. (commandList.size)) do {edgeNum: Number ->
        //    commandList.at(edgeNum).execute
        //}
    }
}

class Product (productName) {
    var name := productName
    print "product {name} created"
    method store{
        print "Store product {name}"
    }
    method remove{
        print "Remove product {name}"
    }
}

class RemoveProduct (aProduct) {
    inherit Command
    product := aProduct
    print "Object from RemoveProduct created"

    method execute() {
        product.remove
    }
}

class StoreProduct (aProduct) {
    inherit Command
    product := aProduct
    print "Object from StoreProduct created"
    method execute {
        product.store
    }
}



var product := Product ("Smartphone")
var removeCommand := RemoveProduct (product)
var storeComamnd := StoreProduct (product)
var invoker := Invoker
invoker.takeComamnd (storeComamnd)
invoker.takeComamnd (removeCommand)
invoker.placeCommands 

