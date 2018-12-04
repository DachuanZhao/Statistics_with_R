ss<- function(n){
  z=n-1
  if(any(n%%(2:z)==0)==F){
    print('质数')
  }
  else{
    print('合数')
  }
}