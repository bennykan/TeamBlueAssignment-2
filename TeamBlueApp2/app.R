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
library(knitr)
library(sqldf)

# Import Data

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel('Clustering Algorithm for Unsupervised Learning - Credit Card Client Anomaly Analysis'),
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
      
      numericInput('outliers', 'Outlier count per cluster', 10,
                   min = 1, max = 50)      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Purpose & Instructions", verbatimTextOutput("Purpose")),
        tabPanel("Outlier Plot", plotOutput("clusterPlot")), 
        tabPanel("Outlier List",DT::dataTableOutput("outlierList")),
        tabPanel("Summary", plotOutput("summary",height= "1000px",width = "800px")),
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
  
  currentData <- reactive({
    
    data <- filedata()
    
    if (is.null(data)) return(NULL)
    
    
    
    data <- data[,!colnames(data) %in% c("default payment next month")]
    data$SEX<-ifelse(data$SEX==1,"M","F")
    
    data$EDUCATION <- ifelse(data$EDUCATION==0|data$EDUCATION==5|data$EDUCATION==6,5,data$EDUCATION)
    
    data$MARRIAGE <- ifelse(data$MARRIAGE==0,3,data$MARRIAGE)
    
    colnames(data)[colnames(data)=="PAY_0"] <- "PAY_STS_SEPT"
    colnames(data)[colnames(data)=="PAY_2"] <- "PAY_STS_AUG"
    colnames(data)[colnames(data)=="PAY_3"] <- "PAY_STS_JULY"
    colnames(data)[colnames(data)=="PAY_4"] <- "PAY_STS_JUNE"
    colnames(data)[colnames(data)=="PAY_5"] <- "PAY_STS_MAY"
    colnames(data)[colnames(data)=="PAY_6"] <- "PAY_STS_APRIL"
    colnames(data)[colnames(data)=="BILL_AMT1"] <- "BILL_AMT_SEPT"
    colnames(data)[colnames(data)=="BILL_AMT2"] <- "BILL_AMT_AUG"
    colnames(data)[colnames(data)=="BILL_AMT3"] <- "BILL_AMT_JULY"
    colnames(data)[colnames(data)=="BILL_AMT4"] <- "BILL_AMT_JUNE"
    colnames(data)[colnames(data)=="BILL_AMT5"] <- "BILL_AMT_MAY"
    colnames(data)[colnames(data)=="BILL_AMT6"] <- "BILL_AMT_APRIL"
    colnames(data)[colnames(data)=="PAY_AMT1"] <- "PAY_AMT_SEPT"
    colnames(data)[colnames(data)=="PAY_AMT2"] <- "PAY_AMT_AUG"
    colnames(data)[colnames(data)=="PAY_AMT3"] <- "PAY_AMT_JULY"
    colnames(data)[colnames(data)=="PAY_AMT4"] <- "PAY_AMT_JUNE"
    colnames(data)[colnames(data)=="PAY_AMT5"] <- "PAY_AMT_MAY"
    colnames(data)[colnames(data)=="PAY_AMT6"] <- "PAY_AMT_APRIL"
    
    factor_VARS <- c('SEX','EDUCATION','MARRIAGE','PAY_STS_SEPT','PAY_STS_AUG','PAY_STS_JULY','PAY_STS_JUNE','PAY_STS_MAY','PAY_STS_APRIL')
    
    data[factor_VARS]<- lapply(data[factor_VARS],function(x) as.factor(x))
    
    
    
    data$Ratio_BILL_AUG <- ifelse(data$PAY_AMT_SEPT==0,0,ifelse(data$BILL_AMT_AUG <=0,1,data$PAY_AMT_SEPT/data$BILL_AMT_AUG))
    data$Ratio_BILL_JULY <- ifelse(data$PAY_AMT_AUG==0,0,ifelse(data$BILL_AMT_JULY <=0,1,data$PAY_AMT_AUG/data$BILL_AMT_JULY))
    data$Ratio_BILL_JUNE <- ifelse(data$PAY_AMT_JULY==0,0,ifelse(data$BILL_AMT_JUNE <=0,1,data$PAY_AMT_JULY/data$BILL_AMT_JUNE))
    data$Ratio_BILL_MAY <- ifelse(data$PAY_AMT_JUNE==0,0,ifelse(data$BILL_AMT_MAY <=0,1,data$PAY_AMT_JUNE/data$BILL_AMT_MAY))
    data$Ratio_BILL_APRIL <- ifelse(data$PAY_AMT_MAY==0,0,ifelse(data$BILL_AMT_APRIL <=0,1,data$PAY_AMT_MAY/data$BILL_AMT_APRIL))
    set.seed(456292)
    
    
    dummies_model <- dummyVars(ï..ID ~ ., data=data)
    
    encod <- predict(dummies_model, newdata = data)
    
    data_encoded <- data.frame(encod)
    
    df <- data_encoded
    
    normalize = function(x) {
      return ((x - min(x)) / (max(x) - min(x)))
    }
    
    df = as.data.frame(lapply(df, normalize))
    
    
    
    #browser()
    
    kmeans.result <- kmeans(df, input$clusters,nstart = 20)
    
    
    kmeans.result
  })
  
  output$clusterPlot <- renderPlot({
    
    kmeans.result <- currentData()
    
    if (is.null(kmeans.result)) return(NULL)
    
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
    #outliers <- order(distances, decreasing=T)[1:outlierSize()]
    dst <- as.data.frame(distances)
    
    dst$cluster <- kmeans.result$cluster
    dst$ID <- seq.int(nrow(data))
    
    outliers <- dst %>%
      group_by(cluster) %>%
      top_n(n = outlierSize(), wt = distances)
    
    #Plot Outliers
    p<-p + geom_point(data=df_out[outliers$ID,],aes(x=PC1,y=PC2), colour="red", size=4)+ggtitle("PCA Means Cluster with Outliers")
    
    p
  })
  
  output$outlierList <- DT::renderDataTable({
    
    data <- filedata()
    
    if (is.null(data)) return(NULL)
    
    kmeans.result <- currentData()
    
    if (is.null(kmeans.result)) return(NULL)
    
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
  
  output$summary <- renderPlot({
    data <- filedata()
    if (is.null(data)) return(NULL)
    
    kmeans.result <- currentData()
    
    if (is.null(kmeans.result)) return(NULL)
    
    data <- data[,!colnames(data) %in% c("default payment next month")]
    data$SEX<-ifelse(data$SEX==1,"M","F")
    
    data$EDUCATION <- ifelse(data$EDUCATION==0|data$EDUCATION==5|data$EDUCATION==6,5,data$EDUCATION)
    
    data$MARRIAGE <- ifelse(data$MARRIAGE==0,3,data$MARRIAGE)
    
    colnames(data)[colnames(data)=="PAY_0"] <- "PAY_STS_SEPT"
    colnames(data)[colnames(data)=="PAY_2"] <- "PAY_STS_AUG"
    colnames(data)[colnames(data)=="PAY_3"] <- "PAY_STS_JULY"
    colnames(data)[colnames(data)=="PAY_4"] <- "PAY_STS_JUNE"
    colnames(data)[colnames(data)=="PAY_5"] <- "PAY_STS_MAY"
    colnames(data)[colnames(data)=="PAY_6"] <- "PAY_STS_APRIL"
    colnames(data)[colnames(data)=="BILL_AMT1"] <- "BILL_AMT_SEPT"
    colnames(data)[colnames(data)=="BILL_AMT2"] <- "BILL_AMT_AUG"
    colnames(data)[colnames(data)=="BILL_AMT3"] <- "BILL_AMT_JULY"
    colnames(data)[colnames(data)=="BILL_AMT4"] <- "BILL_AMT_JUNE"
    colnames(data)[colnames(data)=="BILL_AMT5"] <- "BILL_AMT_MAY"
    colnames(data)[colnames(data)=="BILL_AMT6"] <- "BILL_AMT_APRIL"
    colnames(data)[colnames(data)=="PAY_AMT1"] <- "PAY_AMT_SEPT"
    colnames(data)[colnames(data)=="PAY_AMT2"] <- "PAY_AMT_AUG"
    colnames(data)[colnames(data)=="PAY_AMT3"] <- "PAY_AMT_JULY"
    colnames(data)[colnames(data)=="PAY_AMT4"] <- "PAY_AMT_JUNE"
    colnames(data)[colnames(data)=="PAY_AMT5"] <- "PAY_AMT_MAY"
    colnames(data)[colnames(data)=="PAY_AMT6"] <- "PAY_AMT_APRIL"
    
    factor_VARS <- c('SEX','EDUCATION','MARRIAGE','PAY_STS_SEPT','PAY_STS_AUG','PAY_STS_JULY','PAY_STS_JUNE','PAY_STS_MAY','PAY_STS_APRIL')
    
    data[factor_VARS]<- lapply(data[factor_VARS],function(x) as.factor(x))
    
    
    data$Ratio_BILL_AUG <- ifelse(data$PAY_AMT_SEPT==0,0,ifelse(data$BILL_AMT_AUG <=0,1,data$PAY_AMT_SEPT/data$BILL_AMT_AUG))
    data$Ratio_BILL_JULY <- ifelse(data$PAY_AMT_AUG==0,0,ifelse(data$BILL_AMT_JULY <=0,1,data$PAY_AMT_AUG/data$BILL_AMT_JULY))
    data$Ratio_BILL_JUNE <- ifelse(data$PAY_AMT_JULY==0,0,ifelse(data$BILL_AMT_JUNE <=0,1,data$PAY_AMT_JULY/data$BILL_AMT_JUNE))
    data$Ratio_BILL_MAY <- ifelse(data$PAY_AMT_JUNE==0,0,ifelse(data$BILL_AMT_MAY <=0,1,data$PAY_AMT_JUNE/data$BILL_AMT_MAY))
    data$Ratio_BILL_APRIL <- ifelse(data$PAY_AMT_MAY==0,0,ifelse(data$BILL_AMT_APRIL <=0,1,data$PAY_AMT_MAY/data$BILL_AMT_APRIL))
    
    set.seed(456292)
    
    
    dummies_model <- dummyVars(ï..ID ~ ., data=data)
    
    encod <- predict(dummies_model, newdata = data)
    
    data_encoded <- data.frame(encod)
    
    df <- data_encoded
    
    normalize = function(x) {
      return ((x - min(x)) / (max(x) - min(x)))
    }
    
    df = as.data.frame(lapply(df, normalize))
    
    
    
    #browser()
    
    # kmeans.result <- kmeans(df, clusterSize(),nstart = 20)
    nn <- data[,!colnames(data) %in% c('ï..ID','SEX','EDUCATION','MARRIAGE','PAY_STS_SEPT','PAY_STS_AUG','PAY_STS_JULY','PAY_STS_JUNE','PAY_STS_MAY','PAY_STS_APRIL','ï..ID')]
    cc <- data[,colnames(data) %in% c('SEX','EDUCATION','MARRIAGE','PAY_STS_SEPT','PAY_STS_AUG','PAY_STS_JULY','PAY_STS_JUNE','PAY_STS_MAY','PAY_STS_APRIL','ï..ID','AGE')]
    
    #Add in Cluster results 
    nn$Cluster <- kmeans.result$cluster
    cc$Cluster <- kmeans.result$cluster
    
    #Transpose data 
    mn <- melt(nn, id.vars = c("Cluster"))
    
    mc <- melt(cc, id.vars = c("Cluster"))
    
    #Aggregate Average value for each cluster and numeric variable
    a_mn <- sqldf('SELECT Cluster, Variable, avg(value) as AvgValue
                  FROM mn
                  GROUP BY Cluster, Variable')
    
    
    
    #Factor Cluster varaible to be discrete
    a_mn$Cluster <- as.factor(a_mn$Cluster)
    
    Paid <- ggplot(data = a_mn[a_mn$variable %in% c("Ratio_BILL_AUG","Ratio_BILL_JULY","Ratio_BILL_JUNE","Ratio_BILL_MAY","Ratio_BILL_APRIL"),], aes(x = variable, y = AvgValue, group = Cluster, fill = Cluster))
    Paid <- Paid + geom_bar(stat = "identity", width = 0.5, position = "dodge")
    Paid <- Paid + theme_bw()
    Paid <- Paid +  coord_flip() + labs(title = "% of Bill Paided: Cluster Analysis")
    x <- ggplot(data = a_mn[!a_mn$variable %in% c("Ratio_BILL_AUG","Ratio_BILL_JULY","Ratio_BILL_JUNE","Ratio_BILL_MAY","Ratio_BILL_APRIL","AGE"),], aes(x = variable, y = AvgValue, group = Cluster, fill = Cluster))
    x <- x + geom_bar(stat = "identity", width = 0.5, position = "dodge")
    x <- x + theme_bw()
    x <- x +  coord_flip() + labs(title = "Avg Bill and Payment: Cluster Analysis")
    
    
    a_mc <- sqldf("SELECT Cluster, Variable,Value, COUNT(*) as Freq
                  FROM mc
                  GROUP BY Cluster, Variable,Value")
    
    #Factor Cluster varaible to be discrete
    a_mc$Cluster <- as.factor(a_mc$Cluster)
    
    #Creat Plot clusters and variables
    edu <- ggplot(data = a_mc[a_mc$variable =="EDUCATION",], aes(x = Cluster, y = Freq, group = variable, fill = value)) + geom_bar(stat = "identity", width = 0.5, position = "fill") + labs(title = "Credit ~ Education") + scale_fill_discrete(labels=c("Married","Single","Others"))
    
    sex <- ggplot(data = a_mc[a_mc$variable =="SEX",], aes(x = Cluster, y = Freq, group = variable, fill = value)) + geom_bar(stat = "identity", width = 0.5, position = "fill") + labs(title = "Credit ~ SEX")
    
    mar <- ggplot(data = a_mc[a_mc$variable =="MARRIAGE",], aes(x = Cluster, y = Freq, group = variable, fill = value)) + geom_bar(stat = "identity", width = 0.5, position = "fill") + labs(title = "Credit ~ Marital Status") + scale_fill_discrete(labels=c("Graduate School","University","High School","Others"))
    
    pay <- ggplot(data = a_mc[a_mc$variable=='PAY_STS_SEPT'|a_mc$variable=='PAY_STS_AUG'|a_mc$variable=='PAY_STS_JULY'|a_mc$variable=='PAY_STS_JUNE'|a_mc$variable=='PAY_STS_MAY'|a_mc$variable=='PAY_STS_APRIL',], aes(x = Cluster, y = Freq, group = variable, fill = value))
    
    pay <- pay + geom_bar(stat = "identity", width = 0.5, position = "fill") + facet_grid(. ~ variable)  + labs(title = "Credit Balance Paided: Cluster Analysis")
    pay <- pay +  coord_flip()+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
    
    
    pay <- pay + scale_fill_discrete(labels=c("Pay Ontime","No consumption","Revolving credit card","Pay Delay 1 Mth","Pay Delay 2 Mths","Pay Delay 3 Mths","Pay Delay 4 Mths","Pay Delay 5 Mths","Pay Delay 6 Mths","Pay Delay 7 Mths","Pay Delay 8 Mths"))
    
    
    
    grid.arrange(pay,Paid,x,nrow = 3,top = "Cluster Analysis")
    
    
  })
  
  output$Purpose <- renderText({
    "The purpose of this app is to identify list of customers who display certain
    characteristics of anomalies. We have deployed the model in an application format 
    in which the users can upload the data file with a specified format. The application 
    is tailored to the Retail Credit Risk and Collections departments who have the role 
    to investigate credit card clients who have higher risk of default. 
    The users can specify the number of clusters and number of outliers they wish to see. 
    Once these parameters are selected, the application will return a plot showing the 
    outliers and the list of corresponding customers and their demographics as well as 
    credit card bill and payment balances."
  })
  
  output$Disclaimer <- renderText({
    "This App is jointly submitted by Tyler Blakeley, Benjamin Kan, Mohammad Islam, Avijeet Singh "
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

