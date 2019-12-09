Fit_V<-function(ZF, Compound, fixedP=0){
      Koeff1<-NULL
      Koeff2<-NULL
      Koeff3<-NULL
      Warning<-NULL

    Subset=ZF[which(ZF$Comp==Compound),]        
    mergeData=as.data.frame(aggregate(Subset, by=list(Subset$dose), "mean") )  
    
      Kurve1<-try( drm(Subset$response~Subset$dose, 
                       lowerl=c(0.2,  min(Subset$dose)), 
                       upperl=c(25, Inf), 
                       fct=LL.4(fixed=c(NA,0,100,NA)) )  ) 
      if(mode(Kurve1)=="list"){
        Koeff1=c(Kurve1$fit$par[1], 0, 100, Kurve1$fit$par[2])
      }
      Df1=3
      
      Kurve2<-try(drm(Subset$response~Subset$dose, 
                      lowerl=c(1.25, 30,  min(Subset$dose)), 
                      upperl=c(25, min(80, Subset$response[dim(Subset)[1]]+Subset$Std[dim(Subset)[1]]), 3*max(Subset$dose)), 
                      fct=LL.4(fixed=c(NA,NA, 100,NA)) )  )
      if(mode(Kurve2)=="list"){
        Koeff2=c(Kurve2$fit$par[1], Kurve2$fit$par[2], 100, Kurve2$fit$par[3])  
      }
      Df2=4
      
      Kurve3<-try( drm(Subset$response~Subset$dose, 
                        lowerl=c(0.2, 80,  min(Subset$dose)), 
                        upperl=c(5, 90, 3*max(Subset$dose)), 
                        fct=LL.4(fixed=c(NA,NA, 100,NA)) )  )
      if(mode(Kurve3)=="list"){
          Koeff3=c(Kurve3$fit$par[1], Kurve3$fit$par[2], 100, Kurve3$fit$par[3])  
      }
      Df3=4          
      
      if(!is.null(Koeff1) & !is.null(Koeff2) & !is.null(Koeff3)){
    
            if(mselect(Kurve3)[1]>mselect(Kurve2)[1]){ #print("Fit3 ist better than Fit2") (calculation is based on logLikelihood)
              Chi2=2*(mselect(Kurve3)[1]-mselect(Kurve1)[1])
              Df=Df3
              Kurve=Kurve3
              Koeff=signif(Koeff3, 3)     
              FitNr=3
            } else {# print("Fit2 is better than Fit3")
              Chi2=2*(mselect(Kurve2)[1]-mselect(Kurve1)[1])
              Df=Df2 
              Kurve=Kurve2
              Koeff=signif(Koeff2, 3) 
              FitNr=2
            }
      }else if(!is.null(Koeff1) & !is.null(Koeff2) & is.null(Koeff3)){ #Fit3 did not work
        Warning="File1: Fit3 failed"
        Chi2=2*(mselect(Kurve2)[1]-mselect(Kurve1)[1])
        Df=Df2 
        Kurve=Kurve2
        Koeff=signif(Koeff2, 3) 
        FitNr=2
      }else if(!is.null(Koeff1) & is.null(Koeff2) & !is.null(Koeff3)){ #Fit2 did not work
        Warning="File1: Fit2 failed"
        Chi2=2*(mselect(Kurve3)[1]-mselect(Kurve1)[1])
        Df=Df3
        Kurve=Kurve3
        Koeff=signif(Koeff3, 3)     
        FitNr=3
      }else if(!is.null(Koeff1) & is.null(Koeff2) & is.null(Koeff3)){ #only Fit1 worked
        Warning="File1: Fit2 and Fit3 failed"
        Df=Df1
        Kurve=Kurve1
        Koeff=signif(Koeff1, 3)
        FitNr=1
      }else{
        Warning="all Fits failed"
        FitNr=0
        Kurve=NA
        Koeff=NA
      }
    
      if(FitNr>1){   #if at least one of Fit2 or Fit3 worked
        p=as.numeric(pchisq(Chi2, df=Df-Df1, lower.tail=FALSE))
        if ((length(which(abs(100-mergeData$response)>5))>2) & (p<0.5)){   # only possible if at least 3 concentrations < 95%
          print("Model with 4 df selected")
        }else {# print("Fit1 is winner")
          Kurve=Kurve1
          Koeff=signif(Koeff1,3)
          FitNr=1
          Df=Df1
        }  
      }
    
    if(is.null(Warning)){
      Warning="File1: everything ok"
    }
    
    return(list(Kurve=Kurve, Koeff=Koeff, FitNr=FitNr, Warning=Warning))
    
}#end function