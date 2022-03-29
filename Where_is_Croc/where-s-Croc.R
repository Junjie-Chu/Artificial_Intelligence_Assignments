myFunction<-function(moveInfo, readings, positions, edges, probs){
  if (moveInfo$mem$status==0||moveInfo$mem$status==1){
    moveInfo$mem$alpha<- NULL
  }
  
  ##calculating State transition probability; the time now is at t; state is Si; previous time is t-1;
  ##And state transition matrix is A.
  if (moveInfo$mem$status==0){
    A=matrix(0,nrow=40,ncol=40)#Initiate state transition matrix
    Neighbor=matrix(nrow=40,ncol=7)#Initiate 
    for (k in 1:40){
      indexneighbor1=which(edges[,1]==k) 
      neighbor1=edges[indexneighbor1,2]  
      indexneighbor2=which(edges[,2]==k) 
      neighbor2=edges[indexneighbor2,1]  
      neighbor=union(neighbor1,neighbor2) #node's neighbors
      Neighbor[k,1:length(neighbor)]<-neighbor 
      
      n=length(neighbor)
      temA=1/(n+1)
      
      for (item in neighbor) {
        A[k,item] = temA
        A[k,k] = temA
      }
    }
    
    moveInfo$mem<-c(moveInfo$mem,count=0)
    moveInfo$mem<-c(moveInfo$mem,A=list(A))# only needs to calculate  once, save it in mem for later game
    moveInfo$mem<-c(moveInfo$mem,Neighbor=list(Neighbor))
    # The nodes's neighbors only needs to calculate once, save for later
    
  }else{
    A=moveInfo$mem$A
    Neighbor=moveInfo$mem$Neighbor #Reading from mem for further info
    
  }
  
  ##count the number of readings
  if (moveInfo$mem$status==1){
    moveInfo$mem$count<-0 
  }
  count<-moveInfo$mem$count
  count=count+1
  moveInfo$mem$count<-count
  
  
  
  ##initial probability in this game
  Pi=1/40
  ############# improve
  tourists<-c(positions[1],positions[2])
  tourists<-tourists[-which(is.na(tourists))]
  if ( length(tourists)!=0 && tourists[which.min(tourists)]<0 ){
    croc = -1*tourists[which (tourists<0)]
  }else {
  
  ##When state is Si, reading's probability, observation matrix B
  temB=matrix(nrow=40,ncol=1)
  for (i in 1:40){
    #dnorm returns the value of the probability density function for 
    #the normal distribution given parameters for observation, mean and sd
    temB1=dnorm(readings[1],probs$salinity[i,1],probs$salinity[i,2])
    temB2=dnorm(readings[2],probs$phosphate[i,1],probs$phosphate[i,2])
    temB3=dnorm(readings[3],probs$nitrogen[i,1],probs$nitrogen[i,2])
    temB[i,]=temB1*temB2*temB3
  }
  #Normalization procedure
  sumB=sum(temB[,1])
  temB=temB/sumB
  B=temB
  
  ##using forward algorithm for probability alpha
  temalpha=matrix(nrow=40,ncol=1)
  t=count
    #calculating the initial values
  if (moveInfo$mem$status==0||moveInfo$mem$status==1){
    for (i in 1:40){
      temalpha[i,]=Pi*B[i] #getting the probability in the observation matrix B  
    }
    alpha=temalpha
    moveInfo$mem<-c(moveInfo$mem,alpha=list(alpha)) #append the alpha into mem vector
  }else{
    #recursive calculation
    alpha=moveInfo$mem$alpha
    for (i in 1:40){
      suma=0
      for (j in 1:40){
        suma=suma+alpha[j,t-1]*A[j,i]
      }
      temalpha[i,1]=suma*B[i]    
    }
    temalpha[positions[1],1]=0
    temalpha[positions[2],1]=0
    alpha=cbind(alpha,temalpha) 
    moveInfo$mem$alpha<-alpha  #put the updated forward probability alpha in the mem
  }
  
  #find the maximum probability for croc
  croc=which.max(alpha[,t])
  }
  ##########above is improve
  
  ##path finding
  start<-list(location=positions[3],parent=0)
  #cat("start",positions[3])
  end=croc
  #cat("end",end)
  frontier=list()
  expanded=list()
  frontier<-c(frontier,list(start))
  while(length(frontier)!=0){
    currentpoint=frontier[[1]]
    frontier=frontier[-1]
    expanded<-c(expanded,list(currentpoint))
    if (currentpoint$location==end){break} 
    #for neighboring point
    for(neib in Neighbor[currentpoint$location,]){
      vectorlocation=sapply(expanded,function(item)item$location)
      if (!neib %in% vectorlocation && !is.na(neib)){
        neib<-list(location=neib,parent=currentpoint$location)
        frontier<-c(frontier,list(neib)) #Added into frontier if not in expanded
      }
    }
  }
  
  ##Backwards path finding
  path=list()
  path<-c(path,list(currentpoint))
  steps=1 
  while (path[[steps]]$location!=positions[3]){
    for(item in expanded){
      if(item$location==path[[steps]]$parent){
        path<-c(path,list(item)) 
        steps=steps+1
      }
    }
  }
  #Steps are within the path, the first element is the destination and the last one is start point
  
  ##Moving and processing
  if (steps==1){
    move1 = 0
    move2 = 0
    moveInfo$mem$alpha[end,t]=0 #after search,set the probability=0
  }else if(steps==2){
    move1 = path[[1]]$location
    move2 = 0
    moveInfo$mem$alpha[end,t]=0 #after search,set the probability=0
  }else if(steps>2){
    move1=path[[(steps-1)]]$location
    move2=path[[(steps-2)]]$location
  }
 
  moveInfo$moves = c(move1, move2)
  moveInfo$mem$status=2
  
  return(moveInfo)
}