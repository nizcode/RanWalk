#include <Rcpp.h>
#include <bitset>
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
  
  //IntegerVector possib = {0,1,0,-1,1,0,-1,0}; //possibility matrix
  //possib.attr("dim") = Dimension(4,2);//4 possibility = 4 rows, 2 axis = 2 cols
  //IntegerMatrix pm = as<IntegerMatrix>(possib);//change vector to matrix
  
  //  start at (0,0) origin
  mat(0,0)=0;
  mat(0,1)=0;
  srand(seed);//for reproducibility set seed
  int j = 0;
  int i;
  int ii, x;
  int starti;
  //IntegerVector ps(n);
  do{
    //cout<<i<<endl;
    int ran = rand();//iniating random number
    cout<<bitset<32>(ran)<<endl;
    i = (j==0)?1:0;
    while(i<16&&(i+j)<n){
      //for(i=0; i<16;i++){//does i start on 0 or 1.
      //cout<<i<<endl;
      //int p;//p can take 4 possibilities 
      ii = (i-1)*2;
      //>> shift right ii times. 
      //& bit maski/ng 0x03 is a hexadecimal, 0000,0011 
      //cout<<bitset<32>(ran>>ii)<<endl;
      //cout<<bitset<32>(0x03)<<endl;
      x = (ran>>ii)&0x03;//bit operations
      cout<<bitset<32>(x)<<endl;
      //cout<<ran<<endl;
      switch(x){//switch cases
      case 0x00://1st case
        //cout<<"yes1"<<endl;
        mat((i+j),0)=mat((i+j)-1,0)+1;mat((i+j),1)=mat((i+j)-1,1);break; //move 1 in x direction, +(1,0)
        
      case 0x01://2nd case
        //cout<<"yes2"<<endl;
        mat((i+j),0)=mat((i+j)-1,0)-1;mat((i+j),1)=mat((i+j)-1,1);break;//move -1 in x direction -(1,)
        
      case 0x02://3rd case
        //cout<<"yes3"<<endl;
        mat((i+j),0)=mat((i+j)-1,0);mat((i+j),1)=mat((i+j)-1,1)+1;break;//move 1 in y direction
        
      case 0x03://4th case
        //cout<<"yes4"<<endl;
        mat((i+j),0)=mat((i+j)-1,0);mat((i+j),1)=mat((i+j)-1,1)-1;break; //move -1 in y direction
        
      }//end of switch
      i++;
    }//end of inner loop
    
    j += 16;
  }//end of outer loop
  while(j < n);
  
  
  return(mat);
}//end of walk2 function

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
  


// [[Rcpp::export]] 
// the test function to see if algo works
NumericVector test(IntegerMatrix mat){
  IntegerMatrix data = mat;
  int n = data.size()/2;
  IntegerVector x = data(_,0);
  IntegerVector y = data(_,1);
  double x1, x2, y1, y2;
  int diffx, diffy;
  //std::cout<<n<<std::endl;
  for(int i = 0; i<n; i++){
    //std::cout<<data(i,1)<<std::endl;
    //data(i,0)=data(i,0)
    diffx = x[i+1]-x[i];
    diffy = y[i+1]-y[i];
    
    //  std::cout<<diffx<<std::endl;
    switch(diffx){
    case(1):
      x1++; 
      break;
    case(-1):
      x2++;
      //std::cout<<66<<std::endl;
      break;
    case(0):
      x1=x1;
      x2=x2;
      break;
    }
    switch(diffy){
    case(1):
      y1 ++; break;
    case(-1):
      y2 ++; break;
    case(0):
      y1=y1;
      y2=y2;
      break;
    }
  }
  
  NumericVector v {x1,x2,y1,y2};
  v = v / (data.size()/2);
  //std::cout<< diff<<std::endl;
  
return(wrap(v));
}

  
  


