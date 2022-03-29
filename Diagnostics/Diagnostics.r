##############bynet#################
learn<-function(hist){
    total=nrow(hist)

    indexpn1=which(hist$Pn==1)
    indexsm1=which(hist$Sm==1)
    indexvtb1=which(hist$VTB==1)
    indextb1=which(hist$TB==1)
    indexlc1=which(hist$LC==1)
    indexbr1=which(hist$Br==1)
    indexxr1=which(hist$XR==1)
    indexdy1=which(hist$Dy==1)

    indexpn0=which(hist$Pn==0)
    indexsm0=which(hist$Sm==0)
    indexvtb0=which(hist$VTB==0)
    indextb0=which(hist$TB==0)
    indexlc0=which(hist$LC==0)
    indexxr0=which(hist$XR==0)
    indexdy0=which(hist$Dy==0)
    indexbr0=which(hist$Br==0)

    Ppn1=length(indexpn1)/total
    Ppn0=1-Ppn1
    Psm1=length(indexsm1)/total
    Psm0=1-Psm1
    Pvtb1=length(indexvtb1)/total
    Pvtb0=1-Pvtb1

    Ptb1l1=length(intersect(indextb1,indexvtb1))/length(indexvtb1)
    Ptb0l1=length(intersect(indextb0,indexvtb1))/length(indexvtb1)
    Ptb1l0=length(intersect(indextb1,indexvtb0))/length(indexvtb0)
    Ptb0l0=length(intersect(indextb0,indexvtb0))/length(indexvtb0)

    Plc1l1=length(intersect(indexsm1,indexlc1))/length(indexsm1)
    Plc0l1=length(intersect(indexsm1,indexlc0))/length(indexsm1)
    Plc1l0=length(intersect(indexsm0,indexlc1))/length(indexsm0)
    Plc0l0=length(intersect(indexsm0,indexlc0))/length(indexsm0)

    Pbr1l1=length(intersect(indexsm1,indexbr1))/length(indexsm1)
    Pbr0l1=length(intersect(indexsm1,indexbr0))/length(indexsm1)
    Pbr1l0=length(intersect(indexsm0,indexbr1))/length(indexsm0)
    Pbr0l0=length(intersect(indexsm0,indexbr0))/length(indexsm0)

    Pdy1l00=length(Reduce(intersect,  list(v1 = indexdy1,v2 = indexbr0,v3 = indexlc0)))/length(intersect(indexlc0,indexbr0))
    Pdy0l00=length(Reduce(intersect,  list(v1 = indexdy0,v2 = indexbr0,v3 = indexlc0)))/length(intersect(indexlc0,indexbr0))
    Pdy1l01=length(Reduce(intersect,  list(v1 = indexdy1,v2 = indexbr0,v3 = indexlc1)))/length(intersect(indexbr0,indexlc1))
    Pdy0l01=length(Reduce(intersect,  list(v1 = indexdy0,v2 = indexbr0,v3 = indexlc1)))/length(intersect(indexbr0,indexlc1))
    Pdy1l10=length(Reduce(intersect,  list(v1 = indexdy1,v2 = indexbr1,v3 = indexlc0)))/length(intersect(indexbr1,indexlc0))
    Pdy0l10=length(Reduce(intersect,  list(v1 = indexdy0,v2 = indexbr1,v3 = indexlc0)))/length(intersect(indexbr1,indexlc0))
    Pdy1l11=length(Reduce(intersect,  list(v1 = indexdy1,v2 = indexbr1,v3 = indexlc1)))/length(intersect(indexlc1,indexbr1))
    Pdy0l11=length(Reduce(intersect,  list(v1 = indexdy0,v2 = indexbr1,v3 = indexlc1)))/length(intersect(indexlc1,indexbr1))

    Pxr1l000=length(Reduce(intersect,  list(v1 = indexxr1, v2 = indexpn0,v3 = indextb0,v4 = indexlc0)))/length(Reduce(intersect,  list(v1 = indexpn0,v2 = indextb0,v3 = indexlc0)))
    Pxr1l001=length(Reduce(intersect,  list(v1 = indexxr1, v2 = indexpn0,v3 = indextb0,v4 = indexlc1)))/length(Reduce(intersect,  list(v1 = indexpn0,v2 = indextb0,v3 = indexlc1)))
    Pxr1l010=length(Reduce(intersect,  list(v1 = indexxr1, v2 = indexpn0,v3 = indextb1,v4 = indexlc0)))/length(Reduce(intersect,  list(v1 = indexpn0,v2 = indextb1,v3 = indexlc0)))
    Pxr1l100=length(Reduce(intersect,  list(v1 = indexxr1, v2 = indexpn1,v3 = indextb0,v4 = indexlc0)))/length(Reduce(intersect,  list(v1 = indexpn1,v2 = indextb0,v3 = indexlc0)))
    Pxr1l110=length(Reduce(intersect,  list(v1 = indexxr1, v2 = indexpn1,v3 = indextb1,v4 = indexlc0)))/length(Reduce(intersect,  list(v1 = indexpn1,v2 = indextb1,v3 = indexlc0)))
    Pxr1l101=length(Reduce(intersect,  list(v1 = indexxr1, v2 = indexpn1,v3 = indextb0,v4 = indexlc1)))/length(Reduce(intersect,  list(v1 = indexpn1,v2 = indextb0,v3 = indexlc1)))
    Pxr1l011=length(Reduce(intersect,  list(v1 = indexxr1, v2 = indexpn0,v3 = indextb1,v4 = indexlc1)))/length(Reduce(intersect,  list(v1 = indexpn0,v2 = indextb1,v3 = indexlc1)))
    Pxr1l111=length(Reduce(intersect,  list(v1 = indexxr1, v2 = indexpn1,v3 = indextb1,v4 = indexlc1)))/length(Reduce(intersect,  list(v1 = indexpn1,v2 = indextb1,v3 = indexlc1)))

    Pxr0l000=length(Reduce(intersect,  list(v1 = indexxr0, v2 = indexpn0,v3 = indextb0,v4 = indexlc0)))/length(Reduce(intersect,  list(v1 = indexpn0,v2 = indextb0,v3 = indexlc0)))
    Pxr0l001=length(Reduce(intersect,  list(v1 = indexxr0, v2 = indexpn0,v3 = indextb0,v4 = indexlc1)))/length(Reduce(intersect,  list(v1 = indexpn0,v2 = indextb0,v3 = indexlc1)))
    Pxr0l010=length(Reduce(intersect,  list(v1 = indexxr0, v2 = indexpn0,v3 = indextb1,v4 = indexlc0)))/length(Reduce(intersect,  list(v1 = indexpn0,v2 = indextb1,v3 = indexlc0)))
    Pxr0l100=length(Reduce(intersect,  list(v1 = indexxr0, v2 = indexpn1,v3 = indextb0,v4 = indexlc0)))/length(Reduce(intersect,  list(v1 = indexpn1,v2 = indextb0,v3 = indexlc0)))
    Pxr0l110=length(Reduce(intersect,  list(v1 = indexxr0, v2 = indexpn1,v3 = indextb1,v4 = indexlc0)))/length(Reduce(intersect,  list(v1 = indexpn1,v2 = indextb1,v3 = indexlc0)))
    Pxr0l101=length(Reduce(intersect,  list(v1 = indexxr0, v2 = indexpn1,v3 = indextb0,v4 = indexlc1)))/length(Reduce(intersect,  list(v1 = indexpn1,v2 = indextb0,v3 = indexlc1)))
    Pxr0l011=length(Reduce(intersect,  list(v1 = indexxr0, v2 = indexpn0,v3 = indextb1,v4 = indexlc1)))/length(Reduce(intersect,  list(v1 = indexpn0,v2 = indextb1,v3 = indexlc1)))
    Pxr0l111=length(Reduce(intersect,  list(v1 = indexxr0, v2 = indexpn1,v3 = indextb1,v4 = indexlc1)))/length(Reduce(intersect,  list(v1 = indexpn1,v2 = indextb1,v3 = indexlc1)))

    Te0=hist$Te[indexpn0] 
    Te1=hist$Te[indexpn1] 
    mean1 =mean(Te0)
    sigma1=sd(Te0)
    mean2 =mean(Te1)
    sigma2=sd(Te1)
    #Ptel0=dnorm(Te0,mean1,sigma1)/Ppn0
    #Ptel1=dnorm(Te1,mean2,sigma2)/Ppn1
    #len=max(length(Ptel0),length(Ptel1))

    Bystruct=list(Ppn=NULL,Pte=NULL,Pvtb=NULL,Ptb=NULL,Psm=NULL,Plc=NULL,Pbr=NULL,Pxr=NULL,Pdy=NULL)
    m1<-matrix(c(Ppn0,Ppn1),nrow=1,ncol=2,dimnames=list(NULL,c("Pn=0","Pn=1")))
    m2<-matrix(c(mean1,sigma1,mean2,sigma2),nrow=2,ncol=2,dimnames=list(c("mean","sd"),c("Pn=0","Pn=1")))
    m3<-matrix(c(Pvtb0,Pvtb1),nrow=1,ncol=2,dimnames=list(NULL,c("VTB=0","VTB=1")))
    m4<-matrix(c(Ptb0l0,Ptb0l1,Ptb1l0,Ptb1l1),nrow=2,ncol=2,dimnames=list(c("VTB=0","VTB=1"),c("TB=0","TB=1")))
    m5<-matrix(c(Psm0,Psm1),nrow=1,ncol=2,dimnames=list(NULL,c("Sm=0","Sm=1")))
    m6<-matrix(c(Plc0l0,Plc0l1,Plc1l0,Plc1l1),nrow=2,ncol=2,dimnames=list(c("Sm=0","Sm=1"),c("LC=0","LC=1")))
    m7<-matrix(c(Pbr0l0,Pbr0l1,Pbr1l0,Pbr1l1),nrow=2,ncol=2,dimnames=list(c("Sm=0","Sm=1"),c("Br=0","Br=1")))
    m8<-matrix(c(Pxr0l000,Pxr0l001,Pxr0l010,Pxr0l100,Pxr0l110,Pxr0l101,Pxr0l011,Pxr0l111,Pxr1l000,Pxr1l001,Pxr1l010,Pxr1l100,Pxr1l110,Pxr1l101,Pxr1l011,Pxr1l111),nrow=8,ncol=2,
    dimnames=list(c("Pn=0,TB=0,LC=0","Pn=0,TB=0,LC=1","Pn=0,TB=1,LC=0","Pn=1,TB=0,LC=0","Pn=1,TB=1,LC=0","Pn=1,TB=0,LC=1","Pn=0,TB=1,LC=1","Pn=1,TB=1,LC=1"),c("XR=0","XR=1")))
    m9<-matrix(c(Pdy0l00,Pdy0l01,Pdy0l10,Pdy0l11,Pdy1l00,Pdy1l01,Pdy1l10,Pdy1l11),nrow=4,ncol=2,dimnames=list(c("Br=0,LC=0","Br=0,LC=1","Br=1,LC=0","Br=1,LC=1"),c("Dy=0","Dy=1")))
    Bystruct$Ppn<-m1
    Bystruct$Pte<-m2
    Bystruct$Pvtb<-m3
    Bystruct$Ptb<-m4
    Bystruct$Psm<-m5
    Bystruct$Plc<-m6
    Bystruct$Pbr<-m7
    Bystruct$Pxr<-m8
    Bystruct$Pdy<-m9

    return(Bystruct)
}

##############diagnose#################
diagnose<-function(network,cases){
    Bystruct=network
    result=matrix(nrow=10,ncol=4)
    #randomvector=runif(88888, min = 0, max = 1)
    #I want to generate a lot random numbers one time, but the effect is worse than single runif(1)
    i=1
    for (k in 1 : 10){

        samples=matrix(nrow=1000,ncol=9)
        ran = sample(0:1,4,T)
        cases[k,]$Pn=ran[1]
        cases[k,]$TB=ran[2]
        cases[k,]$LC=ran[3]
        cases[k,]$Br=ran[4]
        te=cases[k,]$Te
        vtb=cases[k,]$VTB
        sm=cases[k,]$Sm
        xr=cases[k,]$XR
        dy=cases[k,]$Dy
        #1000 samples
         for (j in 1:1000){
        #the first unknown
        if (cases[k,]$Pn==1){
            pn=0
        }else{
            pn=1
        }

        prob1=choosep(cases[k,]$Pn,te,vtb,cases[k,]$TB,sm,cases[k,]$LC,cases[k,]$Br,xr,dy,Bystruct)
        Pold=prob1$Ppn * prob1$Pte * prob1$Pvtb * prob1$Psm * prob1$Ptb * prob1$Plc * prob1$Pbr * prob1$Pxr * prob1$Pdy
        prob2=choosep(pn,te,vtb,cases[k,]$TB,sm,cases[k,]$LC,cases[k,]$Br,xr,dy,Bystruct)
        Pnew=prob2$Ppn * prob2$Pte * prob2$Pvtb * prob2$Psm * prob2$Ptb * prob2$Plc * prob2$Pbr * prob2$Pxr * prob2$Pdy

        if (Pnew>Pold){
             cases[k,]$Pn<-pn
        }else {
            if(runif(1)<(Pnew/Pold)){
            i=i+1
            cases[k,]$Pn<-pn
            }else{
            i=i+1    
            }
        }
        
        #the second unknown
        if (cases[k,]$TB==1){
            tb=0
        }else{
            tb=1
        }
         

        prob1=choosep(cases[k,]$Pn,te,vtb,cases[k,]$TB,sm,cases[k,]$LC,cases[k,]$Br,xr,dy,Bystruct)
        Pold=prob1$Ppn * prob1$Pte * prob1$Pvtb * prob1$Psm * prob1$Ptb * prob1$Plc * prob1$Pbr * prob1$Pxr * prob1$Pdy
        prob2=choosep(cases[k,]$Pn,te,vtb,tb,sm,cases[k,]$LC,cases[k,]$Br,xr,dy,Bystruct)
        Pnew=prob2$Ppn * prob2$Pte * prob2$Pvtb * prob2$Psm * prob2$Ptb * prob2$Plc * prob2$Pbr * prob2$Pxr * prob2$Pdy

        if (Pnew>Pold){
            cases[k,]$TB<-tb
        }else {
            if(runif(1)<(Pnew/Pold)){
                i=i+1
                cases[k,]$TB<-tb
            }else{
                i=i+1    
            }
        }
        

        #the third unknown
        if (cases[k,]$LC==1){
            lc=0
        }else{
            lc=1
        }

        prob1=choosep(cases[k,]$Pn,te,vtb,cases[k,]$TB,sm,cases[k,]$LC,cases[k,]$Br,xr,dy,Bystruct)
        Pold=prob1$Ppn * prob1$Pte * prob1$Pvtb * prob1$Psm * prob1$Ptb * prob1$Plc * prob1$Pbr * prob1$Pxr * prob1$Pdy
        prob2=choosep(cases[k,]$Pn,te,vtb,cases[k,]$TB,sm,lc,cases[k,]$Br,xr,dy,Bystruct)
        Pnew=prob2$Ppn * prob2$Pte * prob2$Pvtb * prob2$Psm * prob2$Ptb * prob2$Plc * prob2$Pbr * prob2$Pxr * prob2$Pdy

        if (Pnew>Pold){
            cases[k,]$LC<-lc
        }else {
            if(runif(1)<(Pnew/Pold)){
                i=i+1
                cases[k,]$LC<-lc
            }else{
                i=i+1    
            }
        }
        

        #the 4th unknown
        if (cases[k,]$Br==1){
            br=0
        }else{
            br=1
        }

        prob1=choosep(cases[k,]$Pn,te,vtb,cases[k,]$TB,sm,cases[k,]$LC,cases[k,]$Br,xr,dy,Bystruct)
        Pold=prob1$Ppn * prob1$Pte * prob1$Pvtb * prob1$Psm * prob1$Ptb * prob1$Plc * prob1$Pbr * prob1$Pxr * prob1$Pdy
        prob2=choosep(cases[k,]$Pn,te,vtb,cases[k,]$TB,sm,cases[k,]$LC,br,xr,dy,Bystruct)
        Pnew=prob2$Ppn * prob2$Pte * prob2$Pvtb * prob2$Psm * prob2$Ptb * prob2$Plc * prob2$Pbr * prob2$Pxr * prob2$Pdy

        if (Pnew>Pold){
            cases[k,]$Br<-br
        }else {
            if(runif(1)<(Pnew/Pold)){
                i=i+1
                cases[k,]$Br<-br
            }else{
                i=i+1    
            }
        }
        

        #store sample
        samples[j,]=as.matrix(cases[k,])
        }

        Ppnf=sum(samples[101:1000,1])/900
        Ptbf=sum(samples[101:1000,4])/900
        Plcf=sum(samples[101:1000,6])/900
        Pbrf=sum(samples[101:1000,7])/900
        result[k,]<-c(Ppnf,Ptbf,Plcf,Pbrf)

    }

     return (result)
}

##############choosep#################
choosep<-function(pn,te,vtb,tb,sm,lc,br,xr,dy,Bystruct){
    
    if(pn==0){
        Ppn=Bystruct[["Ppn"]][1]
    }else if(pn==1){
        Ppn=Bystruct[["Ppn"]][2]
    }
    
    if(vtb==0){
        Pvtb=Bystruct[["Pvtb"]][1]
    }else if(vtb==1){
        Pvtb=Bystruct[["Pvtb"]][2]
    }
    
    if(sm==0){
        Psm=Bystruct[["Psm"]][1]
    }else if(sm==1){
        Psm=Bystruct[["Psm"]][2]
    }
    
    if(pn==0){
        #Pte=dnorm(te,Bystruct[["Pte"]][1,1],Bystruct[["Pte"]][2,1])/Bystruct[["Ppn"]][1]
        Pte=dnorm(te,Bystruct[["Pte"]][1,1],Bystruct[["Pte"]][2,1])
    }else if (pn==1){
        #Pte=dnorm(te,Bystruct[["Pte"]][1,2],Bystruct[["Pte"]][2,2])/Bystruct[["Ppn"]][2]
        Pte=dnorm(te,Bystruct[["Pte"]][1,2],Bystruct[["Pte"]][2,2])
        
        
    }
    
    if(vtb==0&tb==0){
        Ptb=Bystruct[["Ptb"]][1,1]
        
    }else if(vtb==1&tb==0){
        Ptb=Bystruct[["Ptb"]][2,1]
        
    }else if(vtb==0&tb==1){
        Ptb=Bystruct[["Ptb"]][1,2]
        
    }else if(vtb==1&tb==1){
        Ptb=Bystruct[["Ptb"]][2,2]
        
    }
    
    if(sm==0&lc==0){
        Plc=Bystruct[["Plc"]][1,1]
        
    }else if(sm==1&lc==0){
        Plc=Bystruct[["Plc"]][2,1]
        
    }else if(sm==0&lc==1){
        Plc=Bystruct[["Plc"]][1,2]
        
    }else if(sm==1&lc==1){
        Plc=Bystruct[["Plc"]][2,2]
        
    }
    
    if(sm==0&br==0){
        Pbr=Bystruct[["Pbr"]][1,1]
        
    }else if(sm==1&br==0){
        Pbr=Bystruct[["Pbr"]][2,1]
        
    }else if(sm==0&br==1){
        Pbr=Bystruct[["Pbr"]][1,2]
        
    }else if(sm==1&br==1){
        Pbr=Bystruct[["Pbr"]][2,2]
        
    }
    
    if(br==0&lc==0&dy==0){
        Pdy=Bystruct[["Pdy"]][1,1]
        
    }else if(br==0&lc==1&dy==0){
        Pdy=Bystruct[["Pdy"]][2,1]
        
    }else if(br==1&lc==0&dy==0){
        Pdy=Bystruct[["Pdy"]][3,1]
        
    }else if(br==1&lc==1&dy==0){
        Pdy=Bystruct[["Pdy"]][4,1]
        
    }else if(br==0&lc==0&dy==1){
        Pdy=Bystruct[["Pdy"]][1,2]
        
    }else if(br==0&lc==1&dy==1){
        Pdy=Bystruct[["Pdy"]][2,2]
        
    }else if(br==1&lc==0&dy==1){
        Pdy=Bystruct[["Pdy"]][3,2]
        
    }else if(br==1&lc==1&dy==1){
        Pdy=Bystruct[["Pdy"]][4,2]
        
    }
    
    if(pn==0&tb==0&lc==0&xr==0){
        Pxr=Bystruct[["Pxr"]][1,1]
    }else if(pn==0&tb==0&lc==1&xr==0){
        Pxr=Bystruct[["Pxr"]][2,1]
    }else if(pn==0&tb==1&lc==0&xr==0){
        Pxr=Bystruct[["Pxr"]][3,1]
    }else if(pn==1&tb==0&lc==0&xr==0){
        Pxr=Bystruct[["Pxr"]][4,1]
    }else if(pn==1&tb==1&lc==0&xr==0){
        Pxr=Bystruct[["Pxr"]][5,1]
    }else if(pn==1&tb==0&lc==1&xr==0){
        Pxr=Bystruct[["Pxr"]][6,1]
    }else if(pn==0&tb==1&lc==1&xr==0){
        Pxr=Bystruct[["Pxr"]][7,1]
    }else if(pn==1&tb==1&lc==1&xr==0){
        Pxr=Bystruct[["Pxr"]][8,1]
        
    }else if(pn==0&tb==0&lc==0&xr==1){
        Pxr=Bystruct[["Pxr"]][1,2]
    }else if(pn==0&tb==0&lc==1&xr==1){
        Pxr=Bystruct[["Pxr"]][2,2]
    }else if(pn==0&tb==1&lc==0&xr==1){
        Pxr=Bystruct[["Pxr"]][3,2]
    }else if(pn==1&tb==0&lc==0&xr==1){
        Pxr=Bystruct[["Pxr"]][4,2]
    }else if(pn==1&tb==1&lc==0&xr==1){
        Pxr=Bystruct[["Pxr"]][5,2]
    }else if(pn==1&tb==0&lc==1&xr==1){
        Pxr=Bystruct[["Pxr"]][6,2]
    }else if(pn==0&tb==1&lc==1&xr==1){
        Pxr=Bystruct[["Pxr"]][7,2]
    }else if(pn==1&tb==1&lc==1&xr==1){
        Pxr=Bystruct[["Pxr"]][8,2]
    }
    
    Prob=list(Ppn=Ppn,Pte=Pte,Pvtb=Pvtb,Psm=Psm,Ptb=Ptb,Plc=Plc,Pbr=Pbr,Pxr=Pxr,Pdy=Pdy)
    return(Prob)
}