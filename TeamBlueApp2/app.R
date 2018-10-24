#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(corrgram)
library(GGally)
library(ggthemes) 
library(DMwR)
library(gridExtra)
library(rattle)
library(readxl)
library(cluster)
library(shiny)

data <- read.csv("default of credit card clients.csv", header = TRUE, na= 'NA')

set.seed(456292)

dummies_model <- dummyVars(ï..ID ~ ., data=data)

encod <- predict(dummies_model, newdata = data)

data_encoded <- data.frame(encod)

df <- data_encoded

normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df = as.data.frame(lapply(df, normalize))

df <- sample_n(data_encoded,30000)



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput('clusters', 'Cluster count', 4,
                     min = 1, max = 9)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("clusterPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
#  kmeans.result <- reactive({
 #   kmeans(df, input$clusters, nstart = 20)
  #})
  
  clusterSize <- reactive({input$clusters })
  
  output$clusterPlot <- renderPlot({
    
    kmeans.result <- kmeans(df, clusterSize(),nstart = 20)
    
    par(mfrow=c(3,2))
    
    df_pca <- prcomp(df)
    df_out <- as.data.frame(df_pca$x)
    
    p<-ggplot(df_out,aes(x=PC1,y=PC2,color = as.factor(kmeans.result$cluster ) ))
    theme<-theme(panel.background = element_blank(),panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"))
    percentage <- round(df_pca$sdev / sum(df_pca$sdev) * 100, 2)
    percentage <- paste( colnames(df_out), "(", paste( as.character(percentage), "%", ")", sep="") )
    
    p<-p+geom_point()+theme+xlab(percentage[1]) + ylab(percentage[2])
    
    p
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

