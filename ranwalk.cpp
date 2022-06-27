#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
  
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]

NumericMatrix walk( int n){ //n walks, n different postion, mat[n,] = final position
  NumericMatrix mat(n,2);//rows is each walk, col0=coordinate x, col1=coordinate y
  
  //initializing first movement
  int coor;//x or y axis
  int mov; //forward or back
  if(rand()%2==0){//50:50 chance to go x or y axis
    coor = 0;
  }else{
    coor = 1;
  }
  if(rand()%2==0){//50:50 chance to go forward or back
    mov= 1;
  }else{
    mov= -1;
  }
  mat(0,coor) = mov;
  
  for(int i = 1; i<n;i++){
    if(rand()%2==0){//50:50 chance to go x or y axis
      coor = 0;
    }else{
      coor = 1;
    }
    if(rand()%2==0){//50:50 chance to go forward or back
      mov= 1;
    }else{
      mov= -1;
    }
    
    if(coor == 0){
      mat(i,coor) = mat(i-1,coor) + mov;// add the movement to the preivous postioon to get new position
      mat(i,1) = mat(i-1,1); // other axis stays the same because can only move in one direction a time and cannot move diagonally
    }if(coor == 1){
      mat(i,coor) = mat(i-1,coor) + mov;
      mat(i,0) = mat(i-1,0);
    }
  }
  return(mat);
}

