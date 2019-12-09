#library(shiny)
#setwd("~/Dropbox/Shiny")
#runApp("version24")

library("gplots")
library("plotrix")
library("drc")
library("XLConnect")

source("axis.breakJN.R")
source("Fit_V.R")
source("Fit_M.R")  
source("Plot.R")
source("BMCcalculation.R")


shinyServer(function(input, output) {
  
  output$text1 <- renderText({ 
    if(!is.null(input$Compound)){ 
    Resultat<- BMCcalculation(FileNamen=input$FileNamen1$datapath, 
                              SheetName=input$SheetName1, 
                              i=1, input)
    paste("Warning", Resultat$Warning) 
    }
  })
  
  output$text2 <- renderText({ 
    if(!is.null(input$FileNamen2) & !is.null(input$Compound) ){
      Resultat<- BMCcalculation(FileNamen=input$FileNamen2$datapath, 
                                SheetName=input$SheetName2, 
                                i=2, input)
      paste("Warning", Resultat$Warning)   
    }
  }) #end renderText
  
  
  output$PNG<- renderImage({      
    if( (!is.null(input$FileNamen1)) & (input$Compound!="") ){  
      
        outfile<-Plot(input)
                 
        list(src = outfile,
             alt = "This is alternate text")
    }#end if
      
  }, deleteFile = TRUE)#Ende renderImage

   #BMCs of file1
   output$table1 <- renderTable({      
     if( (!is.null(input$FileNamen1)) & (input$Compound!="") ){  
       Resultat1<- BMCcalculation(FileNamen=input$FileNamen1$datapath, 
                                   SheetName=input$SheetName1, 
                                   i=1, input)
       return(Resultat1$BMCTabelle)        
       } 
   },
   rownames = TRUE, colnames = TRUE, hover=T, display=c("s", "s", "s", "s" ) ) #end renderTable
  
  #BMCs of file2
  output$table2 <- renderTable({      
    if( (!is.null(input$FileNamen2)) & (input$Compound!="") ){  
      Resultat2<- BMCcalculation(FileNamen=input$FileNamen2$datapath, 
                                  SheetName=input$SheetName2, 
                                  i=2, input)
    return(Resultat2$BMCTabelle)            
    } 
  },
  rownames = TRUE, colnames = TRUE, hover=T, display=c("s", "s", "s", "s" ) ) #end renderTable

  #coefficient of the curves
  output$table3 <- renderTable({    
    KoeffTabelle=matrix(data=NA, nrow=2, ncol=4)
    colnames(KoeffTabelle)=c("slope", "lower assymptote", "upper assymptote", "inflection point")
    rownames(KoeffTabelle)=c("File1", "File2")
    
    if( (!is.null(input$FileNamen1)) & (input$Compound!="") ){       
      Resultat1<- BMCcalculation(FileNamen=input$FileNamen1$datapath, 
                                SheetName=input$SheetName1, 
                                i=1, input)
      Koeff1=Resultat1$Koeff
      KoeffTabelle[1,]=as.character(signif(Koeff1,3))
      
      if(!is.null(input$FileNamen2)) {
        Resultat2<- BMCcalculation(FileNamen=input$FileNamen2$datapath, 
                                  SheetName=input$SheetName2, 
                                  i=2, input)
        Koeff2=Resultat2$Koeff
        KoeffTabelle[2,]=as.character(signif(Koeff2,3))       
      }      
    } 
    return(t(KoeffTabelle))       
  },
  rownames = TRUE, colnames = TRUE, hover=T, display=c("s", "fg",  "fg" )) #end renderTable
  
  output$downloadXLSX <- downloadHandler(
    filename = function(){
      if(input$Compound!=""){
        paste(input$Compound, ".xlsx", sep="")   
      }else{
        "empty.xlsx"
      }
    }, 
    content = function(fname){
      wb <- loadWorkbook(fname, create = TRUE)
      
      KoeffTabelle=matrix(data=NA, nrow=2, ncol=4)
      colnames(KoeffTabelle)=c("slope", "lower assymptote", "upper assymptote", "inflection point")
      rownames(KoeffTabelle)=c("File1", "File2")
      
      if( (!is.null(input$FileNamen1)) & (input$Compound!="") ){       
        Resultat1<- BMCcalculation(FileNamen=input$FileNamen1$datapath, 
                                   SheetName=input$SheetName1, 
                                   i=1, input)
        
        Koeff1=Resultat1$Koeff
        KoeffTabelle[1,]=as.character(signif(Koeff1,3))
        
        createSheet(wb, name = "Curve1")
        writeWorksheet(wb, sheet = "Curve1", startCol = 1, startRow = 1, data=input$Compound, header=F)
        writeWorksheet(wb, sheet = "Curve1", startCol = 1, startRow = 2, data=Sys.time(),     header=F)     
        writeWorksheet(wb, sheet = "Curve1", startCol = 1, startRow = 3, data=input$Legend1,  header=F)
        writeWorksheet(wb, sheet = "Curve1", startCol = 1, startRow = 5, data=Resultat1$BMCTabelle, rownames="BMR")
        
        if(!is.null(input$FileNamen2)) {
          Resultat2<- BMCcalculation(FileNamen=input$FileNamen2$datapath, 
                                     SheetName=input$SheetName2, 
                                     i=2, input)
          Koeff2=Resultat2$Koeff
          KoeffTabelle[2,]=as.character(signif(Koeff2,3))     
          
          createSheet(wb, name = "Curve2")
          writeWorksheet(wb, sheet = "Curve2", startCol = 1, startRow = 1, data=input$Compound, header=F)
          writeWorksheet(wb, sheet = "Curve2", startCol = 1, startRow = 2, data=Sys.time(),     header=F)     
          writeWorksheet(wb, sheet = "Curve2", startCol = 1, startRow = 3, data=input$Legend2,  header=F)
          writeWorksheet(wb, sheet = "Curve2", startCol = 1, startRow = 5, data=Resultat2$BMCTabelle, rownames="BMR")          
        }      
      } 
      
      createSheet(wb, name = "Coefficients")
      writeWorksheet(wb, sheet = "Coefficients", startCol = 1, startRow = 1,  data=input$Compound, header=F)
      writeWorksheet(wb, sheet = "Coefficients", startCol = 1, startRow = 2,  data=Sys.time(),     header=F)
      writeWorksheet(wb, sheet = "Coefficients", startCol = 1, startRow = 5, data=t(KoeffTabelle), rownames="Parameter" )
      
      saveWorkbook(wb)

    }#end content
    )#end downloadXLSX  
  
}) #Ende shinyServer