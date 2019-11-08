var cost_item_1, cost_item_2, cost_item_3 : real
var amt1, amt2, amt3 : int
var subtotal, total_cost : real

put "What is the cost of the first item: $" ..
get cost_item_1
put "How many of these would you like? " ..
get amt1
put "What is the cost of the second item: $" ..
get cost_item_2
put "How many of these would you like? " ..
get amt2
put "What is the cost of the third item: $" ..
get cost_item_3
put "How many of these would you like? " ..
get amt3
subtotal := (cost_item_1 * amt1) + (cost_item_2 * amt2) + (cost_item_3 * amt3)
total_cost := subtotal * 1.13
put "The total cost of these items after taxes is $" ..
put total_cost: 2 : 2 ..
put "."
