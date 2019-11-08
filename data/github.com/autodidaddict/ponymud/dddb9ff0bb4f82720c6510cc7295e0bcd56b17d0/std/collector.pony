
interface CollectionReceiver[A]
  be receivecollection(coll: Array[A] val)
  
actor Collector[A: Any #send]  
  var _coll: Array[A] iso
  let _max: USize
  let _target: CollectionReceiver[A] tag
  
  new create(max: USize, target: CollectionReceiver[A] tag) =>    
    _coll = recover Array[A] end
    _max = max
    _target = target
    
  be receive(s: A) =>
    _coll.push(consume s)    
    if _coll.size() == _max then      
      let output = _coll = recover Array[A] end
      _target.receivecollection(consume output)
    end