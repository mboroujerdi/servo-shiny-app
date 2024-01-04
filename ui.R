##shiny app for insulin regime
library(shiny)
library(shinyMobile)
library(shinycssloaders)
library(htmltools)
library(shinyscreenshot)
rm(list=(ls(all=TRUE)))


ui <- fluidPage(theme = "bootstrap.css",

HTML('<meta name="viewport" content="width=1024">'),
tags$head( tags$meta(name = "viewport", content = "width=1024"),uiOutput("body")),

tags$head(

  tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');

     h1 {
    font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
     }
      h3 {
    font-weight: bold;
         }

    "))
     ),



wellPanel(headerPanel("Second order model simulations."),

# or use radioButtons


 sidebarPanel(
 sliderInput(inputId = "mcr",
                     label = "Choose MCR (ml/min/kg)",
                     value = 0.8, min = 0.5, max = 10,round = FALSE),
 sliderInput(inputId = "zeta",
             label = "Choose damping factor",
             value = 0.9, min = 0.5, max = 2.0,round = FALSE)),
screenshotButton(),


  mainPanel(
    tags$style(type = "text/css", "a{color: #FF8C00;}"),

    tabsetPanel(
    tabPanel("",withSpinner(plotOutput("responseplot"))) )


)))
