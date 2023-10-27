## Version 5
## Update 21 February 2023 [mw]

library(shiny); library(xlsx); library(plotly); library(reactable)

## Read the parameter definition matrix
#para <- read.csv("./shinyapp/param.csv")
# para <- read.csv("./param.csv")

file <- "disease_impact_calculator_v5.xlsm"
para1 <- read.xlsx(paste0("dat/", file), sheetName = "Disease parameter", as.data.frame = TRUE)
para2 <- read.xlsx(paste0("dat/", file), sheetName = "Dairy parameter", as.data.frame = TRUE)
para3 <- read.xlsx(paste0("dat/", file), sheetName = "Beef parameter", as.data.frame = TRUE)
para4 <- read.xlsx(paste0("dat/", file), sheetName = "Sheep parameter", as.data.frame = TRUE)

para <- reshape2::melt(para1, id = c("ID", "Section", "Parameter", "Unit"))
names(para) <- tolower(names(para))
para$no <- paste0("no", para$id)

levels(para$variable) <- gsub("\\.|\\.\\.", " ", levels(para$variable))

para$tabname <- factor(para$section, levels = unique(para$section))
levels(para$tabname) <- c("None", "Mortality", "Reproduction/milk", "Reproduction/milk", "Sell/cull", "Treatment")

para$tabno <- as.numeric(para$tabname)-1
pa <- para[para$variable == para$variable[1],]


makeslider <- function(data){
  data$num <- 1:nrow(data)
  lapply(data$num, function(i){

    if (!grepl("NZ\\$", data$unit[i])){

      ## Type: slider
      sliderInput(data$no[i],
                  data$parameter[i],
                  min = 0, max = 100,
                  value = data$value[i],
                  step = 0.1)
      
    } else {

      ## Type: slider
      sliderInput(data$no[i],
                  data$parameter[i],
                  min = 0, max = 1000,
                  value = data$value[i],
                  step = 10)
      
      
    }
  }
  )
}


# Define UI for input
shinyUI(fluidPage(
  theme = shinythemes::shinytheme("spacelab"),
  titlePanel("NZ livestock disease impact estimator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      ## Radio button input
      radioButtons("species", label = "Select an enterprise type:", 
                   choices = list("Dairy" = 1, "Beef" = 2, "Sheep" = 3), selected = 1, inline = TRUE),

      ## Multi select check box
      checkboxGroupInput("class", label = 'Select farm types (see "Farm Definition" tab):', 
                   choices = list("Farm type" = "Farm type"), selected = "Farm type", inline = TRUE),
      
      ## True/False check box
      checkboxInput("detail", label = "Details",  value = FALSE),
      
      ## Selection input
      selectInput("disease", 
                  label = "Select a similar disease:", 
                  multiple = FALSE, 
                  choices = levels(para$variable)),

                        
      hr(),
      
      makeslider(pa[pa$tabno == 0,]),

      tabsetPanel(

        ## lapply doesn't work here
        tabPanel(levels(pa$tabname)[2], makeslider(pa[pa$tabno == 1,])),
        tabPanel(levels(pa$tabname)[3], makeslider(pa[pa$tabno == 2,])),
        tabPanel(levels(pa$tabname)[4], makeslider(pa[pa$tabno == 3,])),
        tabPanel(levels(pa$tabname)[5], makeslider(pa[pa$tabno == 4,]))

      ),## End of tabsetPanel
      
      
      ## Download button
      downloadButton("downloadData", "Download output"),
      
    width = 3),
    
    # Show a table summarizing the values entered
    mainPanel(
      
      tabsetPanel(
        tabPanel("Partial Budget", reactableOutput("summary")),
        tabPanel("Calculation", reactableOutput("calculation")),
        # tabPanel("Farm gross margin", plotlyOutput("plot1")),
        # tabPanel("Disease impacts (%)", plotlyOutput("plot2")),
        # tabPanel("Disease impacts ($)", plotlyOutput("plot3")),
        tabPanel("Farm Gross Margin", plotOutput("plot1", width = "100%", height = "600px")),
        tabPanel("Disease Impacts ($)", plotOutput("plot3", width = "100%", height = "600px")),
        tabPanel("Disease Impacts (%)", plotOutput("plot2", width = "100%", height = "600px")),
        tabPanel("Farm definition", reactableOutput("ftype")),
        tabPanel("About", tableOutput("about"))
      )

      
      # tabsetPanel(
      #   tabPanel("Baseline incomes/expenses", plotOutput("plot1")),
      #   tabPanel("Disease impacts (%)", plotOutput("ploteco")),
      #   tabPanel("Disease impacts (NZ$)", plotOutput("ploteco")),
      #   tabPanel("Summary", verbatimTextOutput("summary")),
      #   tabPanel("Summary", tableOutput("summary"))
      # )
      
      
    )
  )
))

