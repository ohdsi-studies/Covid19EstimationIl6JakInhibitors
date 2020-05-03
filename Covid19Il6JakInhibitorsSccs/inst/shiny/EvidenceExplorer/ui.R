library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel(paste("SCCS Evidence Explorer", if (blind) "***Blinded***" else "")),
            tags$head(tags$style(type = "text/css", "
             #loadmessage {
                                 position: fixed;
                                 top: 0px;
                                 left: 0px;
                                 width: 100%;
                                 padding: 5px 0px 5px 0px;
                                 text-align: center;
                                 font-weight: bold;
                                 font-size: 100%;
                                 color: #000000;
                                 background-color: #ADD8E6;
                                 z-index: 105;
                                 }
                                 ")),
            conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                             tags$div("Procesing...",id = "loadmessage")),
            fluidRow(
              column(3,
                     selectInput("exposure", "Exposure", unique(exposureOfInterest$exposureName)),
                     selectInput("outcome", "Outcome", unique(outcomeOfInterest$outcomeName)),
                     checkboxGroupInput("database", "Data source", database$databaseId, selected = database$databaseId),
                     checkboxGroupInput("analysis", "Analysis", sccsAnalysis$description,  selected = sccsAnalysis$description)
              ),
              column(9,
                     dataTableOutput("mainTable"),
                     conditionalPanel("output.rowIsSelected == true",
                                      tabsetPanel(id = "detailsTabsetPanel",
                                                  tabPanel("Power",
                                                           uiOutput("powerTableCaption"),
                                                           tableOutput("powerTable"),
                                                           uiOutput("observationTimeTableCaption"),
                                                           tableOutput("observationTimeTable"),
                                                           uiOutput("exposureTimeTableCaption"),
                                                           tableOutput("exposureTimeTable")
                                                  ),
                                                  tabPanel("Overlap",
                                                    plotOutput("overlapPlot")       
                                                  ),
                                                  tabPanel("Population characteristics",
                                                           radioButtons("charCompareType", "", c("Pretty table", "Raw table"), selected = "Pretty table", inline = TRUE),
                                                           uiOutput("table1Caption"),
                                                           dataTableOutput("charCompareTable")),
                                                  tabPanel("Systematic error",
                                                           plotOutput("systematicErrorPlot"),
                                                           div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                    and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
                                                           div(style = "display: inline-block;vertical-align:top;",
                                                               downloadButton("downloadSystematicErrorPlotPng", label = "Download plot as PNG"),
                                                               downloadButton("downloadSystematicErrorPlotPdf", label = "Download plot as PDF")
                                                           ))
                                      )
                                      
                     )
              )
            )
            
  )
)

