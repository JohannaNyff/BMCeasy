library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("BMC calculation"),
  fluidRow(
    column(4,
           
    p("by Johanna Nyffeler"),
    p("using packages 'shiny', 'drc' and 'XLConnect'"),
    p("Manual and example data file:" ,a("http://invitrotox.uni-konstanz.de/", href="http://invitrotox.uni-konstanz.de/") ),    
    offset=0.1)
    ),

  ##############################################################################################################
  ################################### Input  ###################################################################
  
  fluidRow(
    column(2, ############################# Files #############################
           h3("File 1"),
          fileInput("FileNamen1", 
                    label = strong("Input File 1 (Viability)"),
                    width="300px"),
          textInput("SheetName1", 
                    label = strong("Sheet Name"), 
                    value = "Summary",
                    width="200px"),
          h3("File 2 (optional)"),
          fileInput("FileNamen2", 
                    label = strong("Input File 2"),
                    width="300px") ,  
          textInput("SheetName2", 
                    label = strong("Sheet Name"), 
                    value = "Summary",
                    width="200px")       
          ),#end column fuer FileNames
    column(2, ############################# compounds / untreated #############################
           h3("Compound"),
           textInput("Compound",       
                     label = strong("Enter name of compound (exactly as in file)"), 
                     width="200px",
                     value = ""),
           h3("Name of control (optional)"),
           textInput("UntrRepGrName", 
                     label = strong("as indicated in the file:"), 
                     width="200px",
                     value = ""),
           textInput("UntrName",      
                     label = strong("desired label on the plot:"), 
                     width="200px",
                     value = ""),
           h3("Axis labels (optional)"),
           textInput("xAchse", 
                     label = strong("x axis label"), 
                     width="200px",
                     value = ""),
           textInput("yAchse1", 
                     label = strong("y axis label 1 (bold)"), 
                     width="200px",
                     value = "Viability parameter"),
           textInput("yAchse2", 
                     label = strong("y axis label 2"), 
                     width="200px",
                     value = "[% of control ± SD]")
           ),#end column fuer Compound
    column(2,  ############################# BMC options #############################
           h3("BMC options"),
           checkboxGroupInput("BMCLevels", 
                              label = strong("Bench mark response"), 
                              choices = list("BMR05" = 5, 
                                             "BMR10" = 10, 
                                             "BMR15" = 15, 
                                             "BMR20" = 20, 
                                             "BMR25" = 25, 
                                             "BMR30" = 30, 
                                             "BMR50" = 50),
                              selected =c(10, 25, 50) ),
           textInput("userBMR", 
                     label = strong("own BMR: [in %]"), 
                     width="150px",
                     value = ""),         
                  selectInput("ConfidenceLevel", 
                                label = h5("Confidence Level"), 
                                width="150px",
                               choices = list("99%" = 0.99,
                                             "95%" = 0.95, 
                                              "90%" = 0.90),
                                selected = 0.95),
               selectInput("BMCIntervalMethod", 
                           label = strong("BMC Interval Method"), 
                           width="150px",                       
                           choices = c( "tfls",  "delta"), #"none", "fls",
                           selected = "tfls"),
           checkboxInput("BMC_in_logM", 
                         label = "BMC values in logM", value = TRUE)
    ) ,
    column(6,   ############################# graphical options #############################
           h3("Graphical options"),
           column(5,  ######graphical options: columnn 1
                  fluidRow(
                          h4("Data curves"),
                          column(6,
                                 selectInput("Farbe1", 
                                             label = strong("Color 1"), 
                                             width="150px",                       
                                             choices = c("black", "gray30", "gray50", "gray70", "blue", "green4", "red", "orange", "darkorchid1"),
                                             selected = "gray30"),
                                 selectInput("Farbe2", 
                                             label = strong("Color 2"), 
                                             width="150px",
                                             choices = c("black", "gray30", "gray50", "gray70", "blue", "green4", "red", "orange", "darkorchid1"),
                                             selected = "darkorchid1")
                          ),
                          column(4,
                                 selectInput("Punkte1", 
                                             label = strong("Point Type 1"),
                                             width="90px",
                                             choices = as.character(seq(1,25)),
                                             selected = "24"),
                                 
                                 selectInput("Punkte2", 
                                             label = strong("Point Type 2"), 
                                             width="90px",
                                             choices = as.character(seq(1,25)),
                                             selected = "19")
                          )
                        ),#end fluid row fuer Data curves
                  fluidRow(
                          h4("Legend"),
                          checkboxInput("showLegend", label = "show legend", value = TRUE),        
                          selectInput("LegendPosition", 
                                      label = strong("Legend position"), 
                                       width="150px",
                                      choices = c("topright", "topleft", "bottomright", "bottomleft"),
                                      selected = "topright"),                                     
                          textInput("Legend1",
                                    label = strong("Legend 1"), 
                                    width="150px",
                                    value = "Viability"),  
                          textInput("Legend2",
                                    label = strong("Legend 2"),
                                    width="150px",
                                    value = "NA")
                        
                            )#end fluid row fuer Legend              
                  ), ##end column graphical options column 1
           column(3, ######graphical options: column 2
                  h4("Axes and Lines"),
                  selectInput("yMax", 
                              label = strong("maximum of y axis"),
                              width="200px",
                              choices = as.character(c(100, 125, 150, 175, 200, 250)),
                              selected = "150"),
                  selectInput("yAxisSteps", 
                              label = strong("steps of y axis"),
                              width="200px",
                              choices = as.character(c(10, 20, 25, 50)),
                              selected = "25"),
                  checkboxGroupInput("HLine", 
                                     label = strong("horizontal lines at"), 
                                     choices = list("100" = 100, 
                                                    "90" = 90, 
                                                    "75" = 75, 
                                                    "50" = 50),
                                     selected =c(100, 90) ),
                  selectInput("HLineFarbe", 
                              label = strong("horizontal line color"), 
                              width="150px",
                              choices = rev( c("black", "gray10", "gray30", "gray50", "gray70", "gray80", "gray90", "blue", "green4", "red", "orange", "darkorchid1") ),
                              selected = "gray80"),
                  textInput("VLine", 
                            label = strong("vertical line at"), 
                            width="200px",
                            value = ""),
                  selectInput("VLineFarbe", 
                              label = strong("vertical line color"), 
                              width="150px",
                              choices = rev( c("black", "gray10", "gray30", "gray50", "gray70", "gray80", "gray90", "blue", "green4", "red", "orange", "darkorchid1") ),
                              selected = "gray80")
                  ), #end column graphical options column 2
           column(3, ######graphical options: column 3
                  h4("Size of text"),
                  selectInput("cexTitel", 
                              label = strong("Title"),
                              width="100px",
                              choices = as.character(seq(1,5)),
                              selected = "3"),
                  selectInput("cexAxis", 
                              label = strong("Axis"),
                              width="100px",
                              choices = as.character(seq(1,5)),
                              selected = "3"),
                  selectInput("cexLegend", 
                              label = strong("Legend"),
                              width="100px",
                              choices = as.character(seq(1,5)),
                              selected = "3"),
                  h4("Size of points"),
                  selectInput("cexPch", 
                              label = strong("Size of points"),
                              width="100px",
                              choices = as.character(seq(1,5)),
                              selected = "3"),
                  h4("Line widths"),
                  selectInput("Lwd", 
                              label = strong("of axes"),
                              width="100px",
                              choices = as.character(seq(1,10)),
                              selected = "6"),
                  selectInput("LwdKurven", 
                              label = strong("of curves"),
                              width="100px",
                              choices = as.character(seq(1,10)),
                              selected = "8")
                  
                   )   #end column graphical options column 3      
           )  
  ),#end fluidRow
  
  fluidRow(
    column(4),
    column(4,
          submitButton("Submit", width="400px")
    ),   
    column(4)
    ),#end fluidRow
  
  ##############################################################################################################
  ################################### Output  ###################################################################
  
  fluidRow(
    column(5,
           textOutput("text1"), 
           textOutput("text2"),
           h3("Graph"),
           plotOutput("PNG")
           ),
    column(3,
           h3("BMC of File 1"),
           tableOutput("table1"),
           h3("BMC of File 2"),          
           tableOutput("table2"),
           offset=1
            ),
   column(3,     
          h3("Coefficients of fit curves"),          
          tableOutput("table3"),
          downloadButton('downloadXLSX',"Download the tables") 
          )
  )#end fluidRow
  

)#end fluidPage
)#end shinyUI
