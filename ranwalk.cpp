#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
  
//The way I have structured my program is to create the RandomWalk algorithm which returns a matrix of positions
// Then to answer questions 1,2 and 4 I've written a separate function that calls the algorthmic walk function
// and then the outputs is specific for answering questions 1,2 and 4.
//Just thought it was a nicer/ clearer way of organise the code rather than have than copying and pasting the algorithm twice
//also allows for some flexibility, ie. I can graph the positions of the algorithmic walk



//the line below has to present before function other wise won't be able to call the function in R
// [[Rcpp::export]] 

//Algorithmic random walk function - not used for question 1 and 4
IntegerMatrix walk( int n, int seed){ //n walks, n different position, mat[n,] = final position
  IntegerMatrix mat(n,2);//rows is each walk, col0=coordinate x, col1=coordinate y
  
  //initializing first movement
  int coor;//x or y axis
  int mov; //forward or back
  
//  start at (0,0) origin
  mat(0,0)=0;
  mat(0,1)=0;
  
  srand(seed);//for reproducibility set seed
  for(int i = 1; i<n;i++){
    int ran1=rand()%2;
    //cout<<ran1<<endl;
    if(ran1==0){//50:50 chance to go x or y axis
      coor = 0;
    }else{
      coor = 1;
    }
    int ran2 =rand()%2;
    //cout<<ran2<<endl;
    if(ran2==0){//50:50 chance to go forward or back
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

//------------------------
//My second and improved algorithmic function which is used for  questions and 1,2 and 4
// [[Rcpp::export]] 
IntegerMatrix walk2( int n, int seed){ //n walks, n different postion, mat[n,] = final position
  IntegerMatrix mat(n,2);//rows is each walk, col0=coordinate x, col1=coordinate y
  
  IntegerVector possib = {0,1,0,-1,1,0,-1,0}; //possibility matrix
  possib.attr("dim") = Dimension(4,2);//4 possibility = 4 rows, 2 axis = 2 cols
  IntegerMatrix pm = as<IntegerMatrix>(possib);//change vector to matrix
 
  //  start at (0,0) origin
  mat(0,0)=0;
  mat(0,1)=0;
  IntegerVector ps(n);
  srand(seed);//for reproducibility set seed
  for(int i = 1; i<n;i++){
    int p;//p can take 4 possibilities 
    int ran = rand()%100;
    //cout<<ran<<endl;
    if(ran <= 25 && ran > 0){ //1 through 25
      p =0;
    } else if(ran > 75){ //76 through 100
      p =1;
    } else if(ran >25 && ran <=50){
      p=2;
    } else{
      p=3;
    }
    mat(i,0) = mat(i-1,0)+pm(p,0);//change postion of x axis according to possibility p
    mat(i,1) = mat(i-1,1)+pm(p,1);//change postion of y axis according to possibility p
    ps[i-1] = p;
    
  }
  return(mat);
}
//--------------------
//--------------------


//**QUESTION 1 and QUESTION 2**

//this function will look at to see if the destination has been reached and how many goes it takes to reach the destination
//input is destination and the umber of roads taken
// so if input is (1,-3) and N =100 roads.
//output will say whether the destination has been reached at any point during the 100 roads taken

//the line below has to present before function other wise won't be able to call the function in R
// [[Rcpp::export]] 
LogicalVector Destination(IntegerVector dest, int N, int seed){
  //cout<<dest[0]<<dest[1]<<endl;//for debugging
  IntegerMatrix m = walk2(N, seed); 
  LogicalVector reached(N,false);
  int road;
  for(int i =0;i<N;i++){
    //cout<<m(i,0)<<m(i,1)<<endl;//for debugging
    if((m(i,0)==dest[0]&&m(i,1)==dest[1])){
      reached[i]=true;
      road = i;
    }
  }
  //cout<<road<<endl; shows index, ie after how many roads do they find the destination
  return(reached);
}

// [[Rcpp::export]] 
//**QUESTION 4**
//manhattan distance is defined as the min number of "roads" taken to get to a coordinate destination
int ManhattanD(int n, int seed){  //finds the Manhatan distance after n amount of moves
  IntegerMatrix m = walk2(n, seed); 
  int dist =  abs(m(n-1,0))+abs(m(n-1,1));
  return(dist);
}
  
  
  


