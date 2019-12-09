import "collectionsPrelude" as collect

factory method node<K,T>{
    var key:K is public := 0
    var data:T is public := 0
    var left is public := 0
    var right is public := 0
    var null:Boolean is public := true
    
    method isNull {return null}
    
    method setNode (newKey:K,newData:T){
      key := newKey
      data := newData
      left := node
      right := node
      null := false
    }
    
    method setTo (newNode:node) {
      key := newNode.key
      data := newNode.data
      left := newNode.left
      right := newNode.right
      null := newNode.null
    }
    
    method selectChild(target:K){
      if(target.hashcode < key.hashcode)then{return left}
      else{return right}
    }
    
    method childrenSubtree{
      if(left.isNull && right.isNull)then{return node}
      if(left.isNull && !right.isNull)then{return right}
      if(!left.isNull && right.isNull)then{return left}
      else {return left.addRightSubtree(right)}
    }
    
    method addRightSubtree(subtreeHead:node){
      if(right.isNull)then{
        right := subtreeHead
        return self
      }
      else{right.addRightSubtree(subtreeHead)}
    }
    
    method asString{
      if(null == false)then{return "{left.asString}({key},{data}){right.asString}"}
      if(null == true)then{return ""}
    }
}

factory method BST<K,T>{
  
  var head := node
  
  method add(key:K,data:T){
    addNode(key,data,head)
  }
  
  method addNode(key:K,data:T,current:node){
    if(current.isNull)then{
      //current := node
      current.setNode(key,data)
    }
    else{addNode(key,data,current.selectChild(key))}
  }
  
  method isEmpty{
    return head.isNull
  }
    
  method searchKey(target:K){
    return checkKeyNode(target, head)
  }
  
  method checkKeyNode(target:K, current){
    if(current.isNull)then{
      NoSuchObject.raise "does not contain {target}"
    }
    if(current.key == target)then{
      return current.data
    }
    else{checkKeyNode(target,current.selectChild(target))}
  }
  
  method remove(key:K){
    removeNode(key,head)
  }
  
  method removeNode(key:K, current){
    if (current.isNull) then {
      NoSuchObject.raise "does not contain {key}"
    }
    if (current.key == key) then {
      current.setTo(current.childrenSubtree)
    }
    else{removeNode(key,current.selectChild(key))}
  }
  
  method containsKey(key:K){
    try{
      searchKey(key)
      return true
    }
    catch {e:NoSuchObject -> return false}
  }
  
  method asString{
    return "({head.asString})"
  }
} 
  
factory method Dictionary<K,T>{
  inherits collect.collectionFactory.trait<T>
  
  def internalTree = BST
  
  method withAll(initialBindings:Collection<Binding<K,T>>) -> Dictionary<K,T>{
    object{
      for(initialBindings)do {b -> internalTree.add(b.key, b.value)}
    
      method at(key:K)put(value:T){
        internalTree.add(key, value)
        return self
      }

      method []:=(key:K,value:T){
        internalTree.add(key, value)
      }
  
      method at(key:K)->T{
        return internalTree.searchKey(key)
      }
      
      method [](key:K)->T{
        return internalTree.searchKey(key)
      }
      
      method at(key:K)ifAbsent(action){
        try{return internalTree.searchKey(key)}
        catch{e:NoSuchObject -> return action.apply}
      }
      
      method containsKey(key:K){
        return internalTree.containsKey(key)
      }
      
      method removeAllKeys(keys){
        for (keys) do {k -> internalTree.remove(k.key)}
      }
      
      method removeKey(*keys){
        removeAllKeys(keys)
      }
    }
  }
  

}

