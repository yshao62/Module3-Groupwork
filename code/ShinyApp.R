library(shiny)
library(leaflet)
library(tidyverse)
library(data.table)
library(rjson)
library(bslib)
library(jsonlite)
business <- jsonlite::stream_in(file("business.json"),pagesize = 10000)
breakfast_review<-read.csv("Breakfast_review.csv")
breakfast_business<-business%>%
  filter(business$business_id %in% breakfast_review$business_id)
dtm_new<-read.csv("dtm_NEW.csv",encoding = "UTF-8",header=T)
names(dtm_new)<-c("food","service","price","stars","stars_adjust","business_id","stars_scale")

pal <- colorFactor(c("blue","yellow","red"), domain = breakfast_business$stars)
mapplot<-function(dt){
  sub<-dt%>%filter(selected)
  sub%>%
    leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = sub$longitude,
      lat = sub$latitude,
      fillColor = ~pal(dt$stars),
      stroke = FALSE,
      fillOpacity = 0.8,
      popup = sub$name)
  
}
plot2<-function(df){
  
  df1<-data.frame('aspect'=c("food","service","price"),
                  'score'=c((sum(df$food*df$stars_scale))/nrow(df),
                            (sum(df$service*df$stars_scale))/nrow(df),
                            (sum(df$price*df$stars_scale))/nrow(df)))
  
  df2<-data.frame('aspect'=c("food","service","price"),
                  'score'=c((sum(dtm_new$food*dtm_new$stars_scale))/nrow(dtm_new),
                            (sum(dtm_new$service*dtm_new$stars_scale))/nrow(dtm_new),
                            (sum(dtm_new$price*dtm_new$stars_scale))/nrow(dtm_new)))
  df1$all<-df2[,2]
  
  df1%>%ggplot()+
    geom_bar(aes(x=aspect,y=score),fill="lightblue",stat = "identity")+
    geom_point(aes(x=aspect,y=all),col="blue")+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_blank())
  
}


plot1<-function(df){
  N<-nrow(df)
  df%>%
    count(stars)%>%
    mutate(percentage=n/N)%>%
    ggplot()+
    geom_bar(aes(x=stars,y=percentage),fill="lightblue", stat="identity")+
    labs(x="Stars",y="Percentage")+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_blank())
  
}
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  tags$h1("Breakfast restaurants suggestions"),
  titlePanel("Find your business"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("state", "Select a state",unique(breakfast_business$state)),
      uiOutput("secondSelection1"),
      uiOutput("secondSelection2"),
      uiOutput("secondSelection3")
      #actionButton("submit","Submit!")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("map", fluidRow(column(4,
                                        h4(textOutput("name")),
                                        h4(textOutput("stars")),
                                        textOutput("review_count"),
                                        
                                        textOutput("address1"),
                                        textOutput("address2")),
                                 column(8,leafletOutput("map")))),
        tabPanel("Analysis and Suggestion",
                 fluidRow(
                   column(6,align="center",
                          plotOutput("plot1"),
                          h5(textOutput("rank"))),
                   column(6,align="center",plotOutput("plot2"),
                          h5(textOutput("suggestion1"),
                             h5(textOutput("suggestion2")),
                             h5(textOutput("suggestion3")))
                   ))
        )))))


server <- function(input, output) {
  output$secondSelection1 <- renderUI({
    selectInput("City", "Select a city:", choices = as.character(breakfast_business[breakfast_business$state==input$state,"city"]))
  })
  output$secondSelection2 <- renderUI({
    selectInput("name", "Select a name:", choices = as.character(breakfast_business[breakfast_business$state==input$state&breakfast_business$city==input$City,"name"]))
  })
  output$secondSelection3 <- renderUI({
    selectInput("id", "Select a business ID:", choices = as.character(breakfast_business[breakfast_business$state==input$state&breakfast_business$city==input$City&breakfast_business$name==input$name,"business_id"]))
  })
  breakfast_subset<-reactive({
    breakfast_business%>%mutate(selected=((state%in%input$state))&
                                  (city%in%input$City)&
                                  (name%in%input$name)&
                                  (business_id%in%input$id))
  })
  subset2<-reactive({
    breakfast_review[which(breakfast_review$business_id==input$id),]
  })
  
  subset3<-reactive({
    dtm_new[which(dtm_new$business_id==input$id),]
  }) 
  output$map <- renderLeaflet({
    mapplot(breakfast_subset())
  })
  output$name<-renderText({
    paste(breakfast_business[which(breakfast_business$business_id==input$id),]$name)
  })
  output$review_count<-renderText({
    paste(breakfast_business["Review count: ",which(breakfast_business$business_id==input$id),]$review_count)
  })
  output$stars<-renderText({
    paste("Stars: ",breakfast_business[which(breakfast_business$business_id==input$id),]$stars)
  })
  output$address1<-renderText({
    paste("Address: ",breakfast_business[which(breakfast_business$business_id==input$id),]$address)
  })
  output$address2<-renderText({
    paste(breakfast_business[which(breakfast_business$business_id==input$id),]$city)
  })
  output$plot1<-renderPlot({
    plot1(subset2())
  })
  output$plot2<-renderPlot({
    plot2(subset3())
  })
  output$rank<-renderText({
    percentile_df<-breakfast_business%>%mutate(persentile=rank(stars)/nrow(breakfast_business))
    percentile<-percentile_df[which(percentile_df$business_id==input$id),'persentile']
    paste("Your business ranks ", round(percentile*100,0), "percentile among all the breakfast business")
  })
  output$suggestion1<-renderText({
    df<-dtm_new[which(dtm_new$business_id==input$id),]
    df1<-data.frame('aspect'=c("food","service","price"),
                    'score'=c((sum(df$food*df$stars_scale))/nrow(df),
                              (sum(df$service*df$stars_scale))/nrow(df),
                              (sum(df$price*df$stars_scale))/nrow(df)))
    
    df2<-data.frame('aspect'=c("food","service","price"),
                    'score'=c((sum(dtm_new$food*dtm_new$stars_scale))/nrow(dtm_new),
                              (sum(dtm_new$service*dtm_new$stars_scale))/nrow(dtm_new),
                              (sum(dtm_new$price*dtm_new$stars_scale))/nrow(dtm_new)))
    df1$all<-df2[,2]
    if (df1[1,]$score>df1[1,]$all){
      paste("You are doing good in food, good job!")
    }else
      if(df1[1,]$score<df1[1,]$all){
        paste("We suggest you keeping up in food, good luck!")
      }
  })
  output$suggestion2<-renderText({
    df<-dtm_new[which(dtm_new$business_id==input$id),]
    df1<-data.frame('aspect'=c("food","service","price"),
                    'score'=c((sum(df$food*df$stars_scale))/nrow(df),
                              (sum(df$service*df$stars_scale))/nrow(df),
                              (sum(df$price*df$stars_scale))/nrow(df)))
    
    df2<-data.frame('aspect'=c("food","service","price"),
                    'score'=c((sum(dtm_new$food*dtm_new$stars_scale))/nrow(dtm_new),
                              (sum(dtm_new$service*dtm_new$stars_scale))/nrow(dtm_new),
                              (sum(dtm_new$price*dtm_new$stars_scale))/nrow(dtm_new)))
    df1$all<-df2[,2]
    if (df1[2,]$score>df1[2,]$all){
      paste("You are doing good in service, good job!")
    }else
      if(df1[2,]$score<df1[2,]$all){
        paste("We suggest you keeping up in service, good luck!")
      }
    
  })
  output$suggestion3<-renderText({
    df<-dtm_new[which(dtm_new$business_id==input$id),]
    df1<-data.frame('aspect'=c("food","service","price"),
                    'score'=c((sum(df$food*df$stars_scale))/nrow(df),
                              (sum(df$service*df$stars_scale))/nrow(df),
                              (sum(df$price*df$stars_scale))/nrow(df)))
    
    df2<-data.frame('aspect'=c("food","service","price"),
                    'score'=c((sum(dtm_new$food*dtm_new$stars_scale))/nrow(dtm_new),
                              (sum(dtm_new$service*dtm_new$stars_scale))/nrow(dtm_new),
                              (sum(dtm_new$price*dtm_new$stars_scale))/nrow(dtm_new)))
    df1$all<-df2[,2]
    if(df1[3,]$score==0){
      paste("There is no review related to price, we can't provide suggestion.")  
    }else
    if (df1[3,]$score>df1[3,]$all){
      paste("You are doing good in price, good job!")
    }else
      if(df1[3,]$score<df1[3,]$all){
        paste("We suggest you keeping up in price, good luck!")
      }
  })
}


shinyApp(ui, server)
