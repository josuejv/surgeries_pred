## Aplicacion desarrollada por Josué Jiménez Vázquez
library(shiny)
library(tidyverse)
library(DT)

# Source helper functions ----

source("functions.R")


#------Load data ----

if(file.exists("rdas/ncie9_mdl.rda")){
    load(file = "rdas/ncie9_mdl.rda")
}

if(file.exists("rdas/procedimientos_mdl.rda")){
    load(file = "rdas/procedimientos_mdl.rda")
}

if(file.exists("rdas/CIE10_5a.rda")){
    load(file = "rdas/CIE10_5a.rda")
}

if(file.exists("rdas/cirugias_dat.rda")){
    load(file = "rdas/cirugias_dat.rda")
}



# Define UI for application that predics surgeries
ui <- fluidPage(
    
    # Application title
    titlePanel("Cirugias Predictor"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("edad",
                         "Años",
                         min = min(cirugias_dat$EDAD),
                         max = max(cirugias_dat$EDAD),
                         value = 30,
                         step = 1),
            radioButtons("sexo",
                         "Sexo",
                         choices = c("Femenino", "Masculino")
            ),
            selectInput("CIE10",
                        "Diagnóstico",
                        choices = sort(CIE10_5a$DESCRIPCION)),
            actionButton("btn", "Aplicar"),
            h4(textOutput("N_procedimientos")),
            h4(textOutput("N_n_procedimientos")),
        ),
        
        mainPanel(
            h3(textOutput("CIE9_1Text")),
            DT::dataTableOutput("CIE9_1Table"),
            h3(textOutput("CIE9_2Text")),
            DT::dataTableOutput("CIE9_2Table"),
            h3(textOutput("CIE9_3Text")),
            DT::dataTableOutput("CIE9_3Table"),
            h3(textOutput("CIE9_4Text")),
            DT::dataTableOutput("CIE9_4Table"),
            h3(textOutput("CIE9_5Text")),
            DT::dataTableOutput("CIE9_5Table"),
            h3(textOutput("CIE9_6Text")),
            DT::dataTableOutput("CIE9_6Table"),
            h3(textOutput("CIE9_7Text")),
            DT::dataTableOutput("CIE9_7Table"),
            h3(textOutput("CIE9_8Text")),
            DT::dataTableOutput("CIE9_8Table"),
            h3(textOutput("CIE9_9Text")),
            DT::dataTableOutput("CIE9_9Table"),
            h3(textOutput("CIE9_10Text")),
            DT::dataTableOutput("CIE9_10Table"),
        )
    )
)

##----____SERVER____----

server <- function(input, output) {
    
    PersonalData <- reactive({
        edad <- input$edad
        sexo <- input$sexo
        diagnostico <- input$CIE10
        
        dat <- dfCreator(edad, sexo, diagnostico)
        np <- round(procedimientos(dat))
        np_n <- round(n_procedimientos(dat))
        
        if(np == 1){
            sugerencias <- lapply(1:10, dataFilter, dat)
        } else {
            sugerencias <- NULL
        }
        
        results <- list(
            personal = dat,
            np = np,
            np_n = np_n,
            sugerencias = sugerencias
        )
        
    })
    
    output$personalTable <- DT::renderDataTable({
        dat <- PersonalData()
        dat <- dat$personal
        if(!is.null(dat)){
            DT::datatable(dat)
        }
    })

    observeEvent(input$btn,{
        dat0 <- PersonalData()
        dat <- dat0$sugerencias
        n <- dat0$np
        np <- dat0$np_n

        output$N_procedimientos <- renderText({
            if (n == 0){
                "NO NECESITA PROCEDIMIENTOS"
            } else {
                "NECESITA PROCEDIMIENTOS"
            }
        })

        output$N_n_procedimientos <- renderText({
            if (n == 0){
                ""
            } else {
                paste("Numero de procedimientos estimados:", np)
            }
        })
        lapply(1:10, proced_tables, dat, output, np)
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
