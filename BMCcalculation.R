BMCcalculation<-function(FileNamen, SheetName, i, input){
  print("****** Calculating BMCs ********")
    Compound=input$Compound
    
    minNbiol=NULL
    fixedP=0
    BMCLevels=c(as.numeric(input$BMCLevels))
    userBMR=input$userBMR
    if(!userBMR==""){
      BMCLevels=c(BMCLevels, as.numeric(userBMR))
    }
    BMCIntervalMethod=input$BMCIntervalMethod
    ConfidenceLevel= as.numeric(input$ConfidenceLevel)
    
    ZF=readWorksheetFromFile(FileNamen, sheet=SheetName, startRow=4, startCol=1, header=TRUE)  
    ZF=as.data.frame(ZF)
    colnames(ZF)[which(colnames(ZF)=="Chem.Name")]="Comp"
    colnames(ZF)[which(colnames(ZF)=="Test.Conc")]="dose"

    #selection of the values to use  
    ZF=ZF[which(ZF$Comp==Compound),]  
    ZF=ZF[which(!is.na(ZF$response)),]
  
  if(is.null(ZF$unit)){ #Harrill Lab data
    Doseunit="uM" 
  } else{
    Doseunit=ZF$unit[min(which(ZF$Comp==Compound))]  
  }
  if(input$BMC_in_logM){
        if ((Doseunit=="uM") | (Doseunit=="µM")) {
          ZF$dose=ZF$dose/1000/1000
        } else if (Doseunit=="mM"){
          ZF$dose=ZF$dose/1000
        } else if (Doseunit=="nM"){
          ZF$dose=ZF$dose/1000/1000/1000
        }
  }
  
    
    if(i==1){
      Resultat<-Fit_V(ZF, Compound)
    }else{
      Resultat<-Fit_M(ZF, Compound)
    }
    Koeff=Resultat$Koeff
    Kurve=Resultat$Kurve
    FitNr=Resultat$FitNr
    Warning=Resultat$Warning
        
    print("coefficients of the curve:")
    print(Koeff)
      
    BMCLevels=c(1, 2, BMCLevels) #add two dummy levels, so that there are always at least 2
    
    #print("pseudo BMC levels")
    pseudoBMCLevels=BMCLevels/(100-Koeff[2])*100
    
    j=which(pseudoBMCLevels<100)
    
    BMCTabelle=matrix(data=NA, nrow=length(BMCLevels), ncol=4,
                      dimnames=list( c(paste("BMR", BMCLevels,  sep="")),
                                     c("BMC", "Std.Error", "BMCL", "BMCU")) ) 
    if((i==1 | FitNr<3) & FitNr!=0 ){
      BMCTabelle[j,]=ED(Kurve, respLev=pseudoBMCLevels[j], level=ConfidenceLevel, interval=BMCIntervalMethod)   
    }
    
    BMCTabelle=BMCTabelle[,-2] #delete column 2 (the standard error)
    BMCTabelle=BMCTabelle[c(-1,-2),] #delete the two dummy levels from above
    
    if(input$BMC_in_logM){
      BMCTabelle=log10(BMCTabelle)
      Koeff[4]=log10(Koeff[4])
    }
  
    BMCTabelle=signif(BMCTabelle,3)
    
    BMCTabelle=as.data.frame(BMCTabelle)
    BMCTabelle$BMC=as.character(BMCTabelle$BMC)
    BMCTabelle$BMCL=as.character(BMCTabelle$BMCL)
    BMCTabelle$BMCU=as.character(BMCTabelle$BMCU)
  
    return(list(BMCTabelle=BMCTabelle, Koeff=Koeff, Warning=Warning) )

}#end BMC function



