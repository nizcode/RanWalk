make sure the cpp file and R file are in the same working directory.
use setwd() to make sure
then source the code using sourceCpp() this will act as the compiler.

the funciton in the cpp script is called walk()
it accepts one input n, which is the number of random movements the person takes.
It returns a data matrix containing the x and y coordinates of his movements.



Have added another function called destination. returns a bool lweather walker has reached destination after N roads.\
input is destination vector vector, and N - max number of roads taken


