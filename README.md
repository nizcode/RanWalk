Firstly download Rstudio:
https://www.rstudio.com/products/rstudio/download/

Then open up rstudio 
open the randomwalk.R file.
run the command install.packages("Rcpp") (you can highlight the code on the script and do ctrl+enter and that will only run the hghlighted code.)
then do library(Rcpp) 

next make sure the cpp file and R file are in the same working directory.
use setwd() to make sure
then source the code using sourceCpp() this will act as the compiler.

the funciton in the cpp script is called walk()
it accepts one input n, which is the number of random movements the person takes.
It returns a data.matrix containing the x and y coordinates of his movements.



