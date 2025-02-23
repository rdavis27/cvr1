library(tidyverse)
library(shiny)
library(datasets)
library(ggplot2)
library(plotly)
ballottypediv <- 1
tabulatordiv <- 1
precinctdiv <- 1

# Replace 'ui <- fluidPage('...')' with
#       'shinyUI(fluidPage('...'))' if in separate ui.R file
shinyUI(fluidPage(
    titlePanel("Cast Vote Record (CVR) Analysis"),
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput("voteType","Vote Type",choices = NULL,selected = NULL,multiple = FALSE),
            selectInput("ballottypelo","BallotType Low",choices = NULL,selected = NULL,multiple = FALSE),
            selectInput("ballottypehi","BallotType High",choices = NULL,selected = NULL,multiple = FALSE),
            selectInput("tabulatorlo","Tabulator Low",choices = NULL,selected = NULL,multiple = FALSE),
            selectInput("tabulatorhi","Tabulator High",choices = NULL,selected = NULL,multiple = FALSE),
            selectInput("precinctlo","Precinct Low",choices = NULL,selected = NULL,multiple = FALSE),
            selectInput("precincthi","Precinct High",choices = NULL,selected = NULL,multiple = FALSE),
            selectInput("xvar","X Variable",
                        choices = c("CvrNumber","BallotType","Tabulator","Precinct","TabulatorSum"),
                        selected = "Tabulator"),
            checkboxInput("incl_oth","Include Other",value = FALSE),
            radioButtons("xlabel","X Label",choices = c("Normal","Index","Bin","N"),selected = "Index",inline = TRUE),
            numericInput("binsize","Bin Size",1000,min = 1),
            textInput("xcolor","Color",value = "blue,red,green3"),
            textInput("xscale", "X From,To,Step,Tick", value = ""),
            textInput("yscale", "Y From,To,Step,Tick", value = "")
            #wellPanel() # OPTIONAL GROUPING OF INPUTS
        ),
        mainPanel(
            width = 10,
            tabsetPanel(
                type = "tabs",
                tabPanel("Plot", plotOutput(outputId = "myPlot")),
                tabPanel("Plotly", plotlyOutput(outputId = "myPlotly",height = "600px")),
                tabPanel(
                    "Hist",
                    sidebarPanel(
                        width = 2,
                        numericInput("xminHist", "X Min", 0),
                        numericInput("xmaxHist", "X Max", 100),
                        numericInput("xstepHist", "X Step", 2)
                    ),
                    mainPanel(
                        width = 10,
                        plotOutput(outputId = "myHist")
                    )
                ),
                tabPanel(
                    "Text",
                    sidebarPanel(
                        width = 2,
                        numericInput("xsortcolText", "Sort Column", 7),
                        checkboxInput("xsortdescText","Desc",value = TRUE)
                    ),
                    mainPanel(
                        width = 10,
                        verbatimTextOutput("myVText")
                    )
                )
            )
        )
    )
))
