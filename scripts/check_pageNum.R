check_pageNum <- function(x, y){
  if(x==7){
      if(y<1 & y> 159){
        return(FALSE)
      } 
      
 } else if (x==8){
     if(y<1 & y> 208){
       return(FALSE)
     }

 } else if (x == 9){
     if(y<1 & y> 206){
       return(FALSE)
     }

  } else if (x == 27){
    if(y<1 & y> 28){
      return(FALSE)
    }
  }
}


