myFunction<-function(roads, car, packages){
  #initializing the parameters
  dim=ncol(roads$hroads)
  togo=0
  offset=0
  nextMove=0
  #set for destination
  if(car$load==0){
    maytogo=which(packages[,5]==0) #return value of 0 in package [0,5]
    mindManhattanRow=maytogo[1]
    mindManhattan=(dim-1)*(dim-1)+1
    for(i in 1:length(maytogo)){
      dManhattan=abs(packages[maytogo[i],1]-car$x)+abs(packages[maytogo[i],2]-car$y)
      if (dManhattan<mindManhattan){
        mindManhattan=dManhattan
        mindManhattanRow=maytogo[i]
      }
    }
    togo=mindManhattanRow #find the target package package not loaded,working on the first two column???offset stays 0
  }else{
    togo=car$load
    offset=2              #loaded the package,working on third and fourth column;offset is 2
  }
  
  
  #apply A* algorithm for the shortest path for car to go to destination; frontier=openlist expanded=closelist
  
  end=list(x=packages[togo,1+offset],y=packages[togo,2+offset])#choose destination
  start=list(x=car$x,y=car$y,g=0)
  start<-c(start,h=abs(end$x-start$x)+abs(end$y-start$y))
  start<-c(start,f=start$g+start$h)#pick the start point for car 
  start<-c(start,parentX=0)
  start<-c(start,parentY=0)#assume start point and parent point as (0,0), otherwise error
  
  frontier=list()
  expanded=list()
  
  frontier<-c(frontier,start=list(start)) #set frontier as a list named start
  
  
  if (start$x==end$x&&start$y==end$y){
    nextMove = 5
  }else {
    
    while(length(frontier)!=0){
      
      F=sapply(frontier,function(item)item$f)
      fminindex=which.min(F)
      currentpoint=frontier[[fminindex]]#use the smallest point in F
      
      frontier=frontier[-fminindex] #pop from frontier
      expanded<-c(expanded,list(currentpoint))#push into expanded
      
      if (currentpoint$x==end$x && currentpoint$y==end$y){break}#break if current point is destination
      
      #loop through the vertical and horizontal path
      for (i in -1:1){
        tempX=currentpoint$x+i
        if (tempX<1 | tempX>dim)next#skip the current iteration if exceed the dimension
        
        for (j in -1:1){
          if (i*j!=0)next #skip the current iteration if i or j does not equal to zero 
          if (i+j==0)next #skip the current iteration if sum of i and j equal to zero
          tempY=currentpoint$y+j
          if (tempY<1 | tempY>dim){next}#skip the current iteration if y exceed the dimension
          
          xe=sapply(expanded,function(item)item$x)
          ye=sapply(expanded,function(item)item$y)
          indexxe=which(xe==tempX)
          indexye=which(ye==tempY)
          resulte=intersect(indexxe,indexye)
          if(length(resulte)!=0){next}#skip the current iteration if within expanded 
          
          
          xf=sapply(frontier,function(item)item$x)
          yf=sapply(frontier,function(item)item$y)
          indexxf=which(xf==tempX)
          indexyf=which(yf==tempY)
          resultf=intersect(indexxf,indexyf)
          if(length(resultf)!=0){ #if within frontier and length not 0
            
            k=resultf
            if (i==0){
              tempg=currentpoint$g+roads$vroads[currentpoint$x,currentpoint$y-0.5+0.5*j]
              
            }else {
              tempg=currentpoint$g+roads$hroads[currentpoint$x-0.5+0.5*i,currentpoint$y]
              
            }
            
            tempf=tempg+frontier[[k]]$h
            
            if(tempf<frontier[[k]]$f){
              frontier[[k]]$g<-tempg
              frontier[[k]]$f<-tempf
              frontier[[k]]$parentX=currentpoint$x
              frontier[[k]]$parentY=currentpoint$y
            }else next
            
          }else {
            temppoint=list(x=tempX,y=tempY)
            if (i==0){
              #g=currentpoint$g+abs(j)*roads$vroads[currentpoint$x,currentpoint$y-0.5+0.5*j]  
              temppoint<-c(temppoint,g=currentpoint$g+abs(j)*roads$vroads[currentpoint$x,currentpoint$y-0.5+0.5*j])
            }else {
              #g=currentpoint$g+abs(i)*roads$hroads[currentpoint$x-0.5+0.5*i,currentpoint$y]  
              temppoint<-c(temppoint,g=currentpoint$g+abs(i)*roads$hroads[currentpoint$x-0.5+0.5*i,currentpoint$y])
            }
            temppoint<-c(temppoint, h=abs(end$x-temppoint$x)+abs(end$y-temppoint$y))
            temppoint<-c(temppoint, f=temppoint$g+temppoint$h)
            temppoint<-c(temppoint, parentX=currentpoint$x)
            temppoint<-c(temppoint, parentY=currentpoint$y)
            frontier<-c(frontier,list(temppoint))} 
        }
      }
      
      
    }
    #start from destination, find the path for parent point
    
    path=list()
    path<-c(path,list(currentpoint))
    i=1
    while (path[[i]]$x != start$x || path[[i]]$y != start$y){
      for(item in expanded){
        if(item$x==path[[i]]$parentX && item$y==path[[i]]$parentY){
          path<-c(path,list(item)) #complete path within "path", first parameter is destination and the second is start
          i=i+1
        }
      }
    }
    #make the car's movement
    if(i>1){ 
      if (start$x < path[[i-1]]$x) {
        nextMove = 6
      }else if (start$x > path[[i-1]]$x) {
        nextMove = 4
      }else if (start$y < path[[i-1]]$y) {
        nextMove = 8
      }else if (start$y > path[[i-1]]$y) {
        nextMove = 2
      }
    }
    
  }
  
  
  
  car$nextMove = nextMove
  car$mem = list()
  #print(nextMove)
  return(car)   
}
