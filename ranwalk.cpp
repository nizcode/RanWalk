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

//the line below has to present before function other wise won't be able to call the function in R
// [[Rcpp::export]] 

IntegerMatrix walk( int n){ //n walks, n different postion, mat[n,] = final position
  IntegerMatrix mat(n,2);//rows is each walk, col0=coordinate x, col1=coordinate y
  
  //initializing first movement
  int coor;//x or y axis
  int mov; //forward or back
  
//  start at (0,0) origin
  mat(0,0)=0;
  mat(0,1)=0;
  
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

//this function will look at to see if the distination has been reached and how many goes it takes to reach the distination
//input is distination and the umber of roads taken
// so if input is (1,-3) and N =100 roads.
//output will say whether the destination has been reached at any point during the 100 roads taken

//the line below has to present before function other wise won't be able to call the function in R
// [[Rcpp::export]] 

bool Destination(IntegerVector dest, int N){
  //cout<<dest[0]<<dest[1]<<endl;//for debugging
  IntegerMatrix m = walk(N); 
  bool reached = false;
  int road;
  for(int i =0;i<N;i++){
    //cout<<m(i,0)<<m(i,1)<<endl;//for debugging
    if((m(i,0)==dest[0]&&m(i,1)==dest[1])){
      reached = true;
      road = i;
    }
  }
  //cout<<road<<endl; shows index, ie after how many roads do they find the destination
  return(reached);
}
  
  
  


