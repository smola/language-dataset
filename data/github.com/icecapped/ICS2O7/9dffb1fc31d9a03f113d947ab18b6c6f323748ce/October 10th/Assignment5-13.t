const adult := 10.00
const child := 5.00
const senior := 7.50
var totalAdult : int
var totalChild : int
var totalSenior : int
var currentAdult : int
var currentChild : int
var currentSenior : int
totalAdult := 0
totalChild := 0
totalSenior := 0

put "Enter -1 when finished."
loop
    put "Enter number of adult tickets: "..
    get currentAdult
    put "Enter number of child tickets: "..
    get currentChild
    put "Enter number of senior tickets: "..
    get currentSenior
    
    exit when currentAdult = -1 or currentChild = -1 or currentSenior = -1
    
    totalAdult += currentAdult
    totalChild += currentChild
    totalSenior += currentSenior
    
    put "Total sum: $", currentAdult * adult + currentChild * child + currentSenior * senior, "\n"
end loop

put "\nNumber of adult tickets: ", totalAdult, " = $", totalAdult * adult
put "Number of child tickets: ", totalChild, " = $", totalChild * child
put "Number of senior tickets: ", totalSenior, " = $", totalSenior * senior
put "\nTotal profit: $", totalAdult * adult + totalChild * child + totalSenior * senior
