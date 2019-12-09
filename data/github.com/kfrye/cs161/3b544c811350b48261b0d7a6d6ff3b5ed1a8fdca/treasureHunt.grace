//                  +-------------------------+
//                  ¦ 34 ¦ 21 ¦ 32 ¦ 41 ¦ 25  ¦
//                  +----+----+----+----+-----¦
//                  ¦ 14 ¦ 42 ¦ 43 ¦ 14 ¦ 31  ¦
//                  +----+----+----+----+-----¦
//                  ¦ 54 ¦ 45 ¦ 52 ¦ 42 ¦ 23  ¦
//                  +----+----+----+----+-----¦
//                  ¦ 33 ¦ 15 ¦ 51 ¦ 31 ¦ 35  ¦
//                  +----+----+----+----+-----¦
//                  ¦ 21 ¦ 52 ¦ 33 ¦ 13 ¦ 23  ¦
//                  +-------------------------+

var map := list [ list [34, 21, 32, 41, 25],
                  list [14, 42, 43, 14, 31],
                  list [54, 45, 52, 42, 23],
                  list [33, 15, 51, 31, 35],
                  list [21, 52, 33, 13, 23] ]

method calculateFirstDigit(num) {
    (num / 10).truncated
}

method calculateSecondDigit(num) {
    num % 10
}

method followTreasure(treasureMap, start) {
    var current := start
    var first := calculateFirstDigit(start)
    var second := calculateFirstDigit(start)
    var keepGoing := true
    
    while {keepGoing} do {
        print("Checking treasure map at " ++ current)
        var new := treasureMap.at(first).at(second)
        if(new == current) then {
            keepGoing := false
            print("Found treasure at " ++ current)
        }
        else {
            print("Next clue is at " ++ new)
            first := calculateFirstDigit(new)
            second := calculateFirstDigit(new)
            current := new
        }
    }
}

followTreasure(map, 11)