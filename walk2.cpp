//------------------------
#include <Rcpp.h>
#include <bitset>
using namespace Rcpp;
using namespace std;
//My second and improved algorithmic function which is used for  questions and 1,2 and 4
// [[Rcpp::export]] 
IntegerMatrix walk3( int n, int seed){ //n walks, n different postion, mat[n,] = final position
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
    //cout<<bitset<32>(ran)<<endl;
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
      //cout<<bitset<32>(x)<<endl;
      //cout<<ran<<endl;
      switch(x){//switch cases
        case 0x00://1st case
          cout<<"yes1"<<endl;
          mat((i+j),0)=mat((i+j)-1,0)+1;mat((i+j),1)=mat((i+j)-1,1);break; //move 1 in x direction, +(1,0)
        
        case 0x01://2nd case
          cout<<"yes2"<<endl;
          mat((i+j),0)=mat((i+j)-1,0)-1;mat((i+j),1)=mat((i+j)-1,1);break;//move -1 in x direction -(1,)
  
        case 0x02://3rd case
          cout<<"yes3"<<endl;
          mat((i+j),0)=mat((i+j)-1,0);mat((i+j),1)=mat((i+j)-1,1)+1;break;//move 1 in y direction
        
        case 0x03://4th case
          cout<<"yes4"<<endl;
          mat((i+j),0)=mat((i+j)-1,0);mat((i+j),1)=mat((i+j)-1,1)-1;break; //move -1 in y direction
    
        }//end of switch
      i++;
    }//end of inner loop
   
    j += 16;
  }//end of outer loop
  while(j < n);

    
  return(mat);
}//end of walk2 function
