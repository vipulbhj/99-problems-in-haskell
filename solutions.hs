-- Problem 01
myLast []   = error "Are you dumb ..??"
myLast [x]  = x
myLast (_:xs) = myLast xs

-- Problem 02
mySecondLast [] = error "Yoo, you dumb...?"
mySecondLast [x] = error "Error .. ?"
mySecondLast [x, _] = x
mySecondLast (_:xs) = mySecondLast xs

-- Problem 03

