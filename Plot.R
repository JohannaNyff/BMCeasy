Plot<-function(input){
  print("****** I am plotting ********")
  if(is.null(input$FileNamen2)){ #if only 1 file name = only 1 endpoint 
    FileNamen=input$FileNamen1$datapath
    SheetName=input$SheetName1
  } else { #2 file names
    FileNamen=c(input$FileNamen1$datapath, input$FileNamen2$datapath)
    SheetName=c(input$SheetName1, input$SheetName2)
  }
  
  Compound=input$Compound
  UntrRepGrName=input$UntrRepGrName
  if(input$UntrName!=""){
    UntrName=input$UntrName
  } else {
    UntrName="ctr"
  }

## graphical parameters column 1
Farben=c(input$Farbe1, input$Farbe2)
Punkte=c(as.numeric(input$Punkte1), as.numeric(input$Punkte2)) 
Legende=c(input$Legend1, input$Legend2) 
LegendPosition=input$LegendPosition  
Dimension=c(800,600,72)
Titel=Compound
TitelFarbe="black"

## graphical parameters column 2
xAchse=input$xAchse
xLim=NULL
yAchse1=input$yAchse1
yAchse2=input$yAchse2


yLim=c(0, as.numeric(input$yMax) )
yAxisSteps=as.numeric(input$yAxisSteps)
HLine=as.numeric(input$HLine)
HLineFarbe=input$HLineFarbe
HLineLty="dotted"
VLine=input$VLine
VLineFarbe=input$VLineFarbe
VLineLty="dotted"


## graphical parameters column 3
cexTitel=as.numeric(input$cexTitel)
cexLab=3 #as.numeric(input$cexLab)
cexAxis=as.numeric(input$cexAxis)
cexLegend=as.numeric(input$cexLegend)
cexPch=as.numeric(input$cexPch)
Lwd=as.numeric(input$Lwd)
LwdKurven=as.numeric(input$LwdKurven)

AnzKurven=length(FileNamen)
  

######################################Graph machen #######################
outfile <- tempfile(fileext='.png')      
png(outfile, width=800, height=600)
par(mar=c(7,12,3,3))  #vector of the form c(bottom, left, top, right)
    
    for (i in seq(1,AnzKurven)) {
      print(paste("i=", i, "***", Legende[i]))
      ZF=readWorksheetFromFile(FileNamen[i], sheet=SheetName[i], startRow=4, startCol=1, header=TRUE)  
      ZF=as.data.frame(ZF)
      colnames(ZF)[which(colnames(ZF)=="Chem.Name")]="Comp"
      colnames(ZF)[which(colnames(ZF)=="Test.Conc")]="dose"
      #selection of the values to use
      ZF=ZF[which((ZF$Comp==Compound) |(ZF$Comp==UntrRepGrName)),]  
      ZF=ZF[which(!is.na(ZF$response)),]
      print(ZF)
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
        
      ### determine the x axis limits based on the actual data
      if(i==1){
        minlogX=floor(log10(min(ZF$dose[ZF$Comp==Compound])))
        maxlogX=ceiling(log10(max(ZF$dose[ZF$Comp==Compound])))
        xLim[1]=10^minlogX
        xLim[2]=10^maxlogX
        
        if(UntrRepGrName==""){
          PosUntr=xLim[1]
        } else{
          PosUntr=0.35*xLim[1] 
          xLim[1]=0.25*xLim[1]
        }  
        
        if (xAchse=="") {
          if(input$BMC_in_logM){
            xAchse=expression(paste(bold("concentration "), "[log"[10], "(M)]"))
          }else{
            xAchse=paste("concentration [", Doseunit, "]", sep="")
          }
        }
      }
      ZF$dose[ZF$Comp==UntrRepGrName]=PosUntr
      print(ZF)
      
      Data1=as.data.frame(aggregate(ZF, by=list(ZF$dose), "mean") )
      Data2=as.data.frame(aggregate(ZF, by=list(ZF$dose), "sd") )
      mergedData=cbind(Data1$dose, Data1$response, Data2$response)
      mergedData=as.data.frame(mergedData)
      colnames(mergedData)=c("dose", "Mean", "Std")
      Data1<-NULL
      Data2<-NULL
      
      if(i==1){     
        plot(mergedData$dose, mergedData$Mean, col=Farben[i], log="x", pch=Punkte[i], cex=cexPch, bty='n',
             ylim=c(yLim[1], yLim[2]),  
             xlim=c(xLim[1],xLim[2]), 
             xlab=NA, ylab=NA, bg="white", xaxt="n", yaxt="n",
             cex.axis=cexAxis, cex.lab=cexLab, font.lab=2, las=1, col.axis="white")  
        for (H in HLine) {     
          lines(x=c(10^-10, xLim[2]), y=c(H,H), col=HLineFarbe,  lty=HLineLty, lwd=Lwd)           
        } 
        
        if(VLine!=""){
          VLine=as.numeric(VLine)
          lines(x=c(VLine, VLine), y=c(yLim[1], yLim[2]), col=VLineFarbe,  lty=VLineLty, lwd=Lwd)           
          
        }
        #Fit Daten1 = Viability  
        Resultat<-Fit_V(ZF, Compound)
        Kurve=Resultat$Kurve
      } else {   
        #Fit Daten2 (= functional endpoint)
        Resultat<-Fit_M(ZF, Compound)
        Kurve=Resultat$Kurve   
      }     
      
      #plot curves, points and error bar 
      if(!is.na(Kurve)){
        plot(Kurve, type="none", col=Farben[i], lty="dashed",  lwd=LwdKurven, add=TRUE)      
      }
      arrows(mergedData$dose, mergedData$Mean-mergedData$Std, mergedData$dose, mergedData$Mean+mergedData$Std, code=3, angle=90, length=0.04, lwd=Lwd, col=Farben[i])
      points(mergedData$dose, mergedData$Mean, col=Farben[i],  pch=Punkte[i],cex=cexPch, bg="white", lwd=Lwd, add=TRUE,col.axis="white")       
    
    }#for each endpoint         

################### Axis, legend, titel ######################              
## Ticks
Ticks=NULL
for (i in seq(minlogX, maxlogX-1)) {
  Ticks=c(Ticks, 10^i*c(1,2,3,4,5,6,7,8,9))   
}
Ticks=c(Ticks, xLim[1])
Ticks=Ticks[Ticks>=xLim[1]]
Ticks=Ticks[Ticks<=xLim[2]]    
if(!(UntrRepGrName=="")){
  Ticks=c(PosUntr, Ticks)
}
axis(1, at=Ticks, labels=FALSE, lwd=Lwd, col.axis="black")

## plot x-axis with tick labels
TickLabelPos=10^seq(minlogX, maxlogX) 
TickLabel=TickLabelPos
if(input$BMC_in_logM){
  TickLabel=log10(TickLabel)
}
axis(1, at=c(xLim[1], xLim[2]), labels=F, lwd=Lwd, lwd.ticks=0, col.axis="black", cex.axis=cexAxis) 
if(UntrRepGrName==""){
  axis(1, at=TickLabelPos,  lwd=Lwd,  labels=F,                    tck=0.01,  col.axis="black", cex.axis=cexAxis)      
  axis(1, at=TickLabelPos,  lwd=0,    labels=TickLabel,     line=1,    col.axis="black",  cex.axis=cexAxis)          
} else{
  axis(1, at=c(PosUntr, TickLabelPos), lwd=Lwd,  labels=F,                              tck=0.01, col.axis="black", cex.axis=cexAxis)
  axis(1, at=c(PosUntr, TickLabelPos), lwd=0,    labels=c(UntrName, TickLabel),  line=1,   col.axis="black", cex.axis=cexAxis) 
  axis.breakJN(1,PosUntr/7*13, brw=0.025, Lwd=Lwd) 
}

#plot y-axis with tick labels
axis(2, at=c(yLim[1],yLim[2]*1.05),                      labels=F,     lwd=Lwd, lwd.ticks=0, col.axis="black", cex.axis=cexAxis)
axis(2, at=seq(from=yLim[1], to=yLim[2], by=yAxisSteps), labels=TRUE,  lwd=Lwd, las=1,       col.axis="black",  cex.axis=cexAxis)

#axis labels
mtext(side = 1, xAchse, cex=cexAxis, font=2, line = par("mar")[1]-1.5)
if((yAchse2!="")){
  mtext(side = 2, yAchse1, cex=cexAxis, font=2, line = par("mar")[2]-3)  
  mtext(side = 2, yAchse2, cex=cexAxis, font=1, line = par("mar")[2]-6)
}else{
  mtext(side = 2, yAchse1, cex=cexAxis, font=2, line = par("mar")[2]-6)  
}

if(input$showLegend){
      legend(LegendPosition, Legende[1:AnzKurven], cex=cexLegend,
             col=Farben, lty="dashed", lwd=Lwd, pch=Punkte[1:AnzKurven], pt.cex=cexPch, pt.bg="white", bty="n")
}#end if showLegend

title(main=Titel, col.main=TitelFarbe, cex.main=cexTitel, line=par("mar")[3]-2.5) 

dev.off()  
return(outfile)
}#end function