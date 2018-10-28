# Interactive plot and list of credit card client anomaly analysis #

# Import Library
library(shiny)
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
library(DT)
library(dplyr)

# Import Data





# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel('Clustering Algorithm for Unupervised Learning - Credit Card Client Anomaly Analysis'),
   # Application title
   # titlePanel("Old Faithful Geyser Data"),
  

   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        #Selector for file upload
        fileInput('datafile', 'Choose CSV file',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain')),
        
        numericInput('clusters', 'Cluster count', 4,
                     min = 1, max = 9),
        
        numericInput('outliers', 'Outlier count', 10,
                     min = 1, max = 50)      
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Oulier Plot", plotOutput("clusterPlot")), 
          tabPanel("Outlier List",DT::dataTableOutput("outlierList")),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel("Disclaimer", verbatimTextOutput("Disclaimer"))
        )
         
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
#  kmeans.result <- reactive({
 #   kmeans(df, input$clusters, nstart = 20)
  #})
  
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  clusterSize <- reactive({input$clusters })
  
  outlierSize <- reactive({input$outliers })
  
  output$clusterPlot <- renderPlot({
    
    
    data <- filedata()
    
    if (is.null(data)) return(NULL)
    
    # 
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
    
    
    
    kmeans.result <- kmeans(df, clusterSize(),nstart = 20)
    
    par(mfrow=c(3,2))
    
    df_pca <- prcomp(df)
    df_out <- as.data.frame(df_pca$x)
    
    p<-ggplot(df_out,aes(x=PC1,y=PC2,color = as.factor(kmeans.result$cluster ) ))
    theme<-theme(panel.background = element_blank(),panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"))
    percentage <- round(df_pca$sdev / sum(df_pca$sdev) * 100, 2)
    percentage <- paste( colnames(df_out), "(", paste( as.character(percentage), "%", ")", sep="") )
    
    p<-p+geom_point()+theme+xlab(percentage[1]) + ylab(percentage[2])
    
    centers <- kmeans.result$centers[kmeans.result$cluster, ]  
    
    # Calculate distance each point is from the center of the cluster
    distances <- sqrt(rowSums((df - centers)^2))
    # Take the top 20 fartherest points from the cluster center
    outliers <- order(distances, decreasing=T)[1:outlierSize()]
    
    #Plot Outliers
    p<-p + geom_point(data=df_out[outliers,],aes(x=PC1,y=PC2), colour="red", size=4)+ggtitle("PCA Means Cluster with Outliers")

    p
  })
  
  output$outlierList <- DT::renderDataTable({
    data <- filedata()
    
    if (is.null(data)) return(NULL)
    
    # 
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
    
    
    
    kmeans.result <- kmeans(df, clusterSize(),nstart = 20)
    
    #Find the Center of each cluster
    centers <- kmeans.result$centers[kmeans.result$cluster, ]  
    
    # Calculate distance each point is from the center of the cluster
    distances <- sqrt(rowSums((df - centers)^2))
    # Take the top 20 fartherest points from the cluster center
    outliers <- order(distances, decreasing=T)[1:20]
    
    dst <- as.data.frame(distances)
    
    dst$cluster <- kmeans.result$cluster
    dst$ID <- seq.int(nrow(data))
    
    outliers <- dst %>%
      group_by(cluster) %>%
      top_n(n = 5, wt = distances)
    
    customer_anomaly<-data[outliers$ID,!colnames(data) %in% c('ï..ID','Ratio_BILL_AUG','Ratio_BILL_JULY','Ratio_BILL_JUNE','Ratio_BILL_MAY','Ratio_BILL_APRIL')]
    customer_anomaly$Cluster<-outliers$cluster
    
    customer_anomaly
    
    #kable(customer_anomaly,caption="List of Customers Exhibiting Anomaly Characteristics")
    
  })  
  
  output$summary <- renderText({
    "Hello "
  })
  
  output$Disclaimer <- renderText({
    "This App is jointly submitted by Tyler Blakeley, Benjamin Kan, Mohammad Islam, Avijeet Sing "
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

