library(shinydashboard)
library(shinyalert)
library(reshape2)
library(caret)
library(randomForest)
library("openxlsx")

final_data = read.xlsx("Final_Data_Both_Review_and_Information.xlsx")

title <- tags$a(icon("google-play"),style = "color: white",
                'App Ratings Prediction')

ui <- dashboardPage(
  
  title = "Google Play Store Application Rating Prediction",
  
  skin = "yellow",
 
  dashboardHeader(title = title, titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      
    selectInput(inputId = "category_parameter", final_data$Category,label = "Select the App Category",choices =final_data$Category,width = "100%"),
    
    numericInput(inputId ="size_parameter", label = "Enter the Size of App in KB", value = "", min = 0, max = NA, step = NA,width = NULL),
    
    numericInput(inputId ="install_parameter", label = "Enter the No. of Installs of Apps", value = "", min = 0, max = NA, step = NA,width = NULL),
    
    sliderInput(inputId = "polarity_parameter", "Polarity :",min = -1, max = 1,value = 0.4, step = 0.1),
    
    numericInput(inputId ="reviews_parameter", label = "Enter the No. of Reviews of Apps", value = "", min = 0, max = NA, step = NA,width = NULL),
    
    selectInput(inputId = "appType_parameter",final_data$Type,label = "Select the Type of App",choices = final_data$Type),
    
     conditionalPanel(condition = "input.appType_parameter == 'Paid' ",
     
     numericInput(inputId ="price_parameter", label = "Enter the Price of Apps", value = 1, min = 0, max = NA, step = NA,width = NULL)
    
   ),
    
    numericInput(inputId ="days_parameter", label = "Enter the No. of Days till Last Update", value = "", min = 0, max = NA, step = NA,width = NULL),
    
    selectInput(inputId = "contentRating_parameter", final_data$Content_Rating,label = "Select Content Rating",choices = final_data$Content_Rating ),
    
    useShinyalert(),
    
    actionButton("Gobutton", "Submit")
     
    )
  ),
  dashboardBody(
    
      tags$head(tags$style(HTML(' .content-wrapper, .right-side {
                                   background-color: white;
                                }                             

                                '))),
    
      fluidRow(
        
            tags$head(tags$style(".shiny-output-error{color: white;}")),
        
            infoBoxOutput("print")
        ),
      
               br(),br(),
      
    
     fluidRow(
       
           infoBoxOutput("AppCategory"),
           
           infoBoxOutput("size"),
           
           infoBoxOutput("Installs"),
           
           infoBoxOutput("Polarity"),
           
           infoBoxOutput("slider_rangeInput"),
           
           infoBoxOutput("Reviews"),
           
           infoBoxOutput("Type"),
           
           infoBoxOutput("Price"),
           
           infoBoxOutput("Days"),
           
           infoBoxOutput( "contentrating"),
           
           renderPrint("priceWarning")
           
       )
    )
  
)

server <- function(input, output,session) {
  
                    ##########  selected parameter display      #########                
  
              output$AppCategory<- renderPrint({
                    
           
                  infoBox(title = "Type of App Category", icon = icon("android"), width = 15,
                          fill = TRUE,value = input$category_parameter,color = "teal")
                  
                  })
              
              output$size <- renderPrint({
                
                infoBox(title = "Size of App", icon = icon("sort-numeric-up"), width = 15,
                        fill = TRUE,value =input$size_parameter,color = "purple"
                )
                
              })
              
              output$Installs <- renderPrint({
                
                
                infoBox(title = "No. of Install",icon = icon("download"),width = 15, 
                        fill = TRUE,value =input$install_parameter ,color = "olive" )
                
                
              })
              
              output$Polarity <- renderPrint({
                
                infoBox(title = "Polarity",icon = icon("poll"), width = 15,
                        fill = TRUE,value = input$polarity_parameter,color = "green")
                
              })
              
              output$Reviews <- renderPrint({
                
                infoBox(title = "No. of Reviews", icon = icon("comments"), width = 15,
                        fill = TRUE,value =input$reviews_parameter,color = "navy"
                )
                
              })
              
              output$Type <- renderPrint({
                
                infoBox(title = "Type of App",icon = icon("money-bill-alt"),width = 15, 
                        fill = TRUE,value = input$appType_parameter,color = "light-blue" )
                
              })
              
              output$Price <- renderPrint({
                
                infoBox(title = "Price of App",icon = icon("rupee-sign"),width = 15, 
                        fill = TRUE,value = ifelse(input$appType_parameter == 'Paid',input$price_parameter,0 ),color = "maroon" )
                
              })
              
              output$Days <- renderPrint({
                
                infoBox(title = "Days till Last Updated ",icon = icon("calendar-alt"),width = 15, 
                        fill = TRUE,value =input$days_parameter ,color = "aqua" )
                
              })
              
              output$contentrating <- renderPrint({
               
               infoBox(title = "Content Rating ",icon = icon("users"),width = 15, 
                       fill = TRUE,value =input$contentRating_parameter ,color = "olive" )
               
             })
  
                      ######   slider parameter  ######
                  
                    output$slider_rangeInput <- renderUI({
                      
                      observeEvent(input$polarity_parameter, {
                        
                        if(input$polarity_parameter!=0.4){
                          
                          shinyalert("Message !", "Be Positive : We have consider 0.4 as a default Polarity because it is average of Positive Polarity.")
                          
                        }
                      })
                      
                      if(input$polarity_parameter > 0 & input$polarity_parameter <= 1){
                        
                        
                          infoBox(
                            "Sentiment Type", paste("Positive"), width = 15, fill = TRUE,icon = icon("credit-card"),
                            color = "yellow"
                          )
                          
                        
                      }
                      
                      else if(input$polarity_parameter >= -1 & input$polarity_parameter <= -0.1)
                      {
                        
                        infoBox(
                          "Sentiment Type", paste("Negative"), width = 15, fill = TRUE,icon = icon("poll"),
                          color = "yellow"
                        )
                         
                        
                      }
                      else{
                        
                       
                         
                        infoBox(
                          "Sentiment Type", paste("Neutral"), width = 15, fill = TRUE,icon = icon("credit-card"),
                          color = "yellow"
                          )  
                      
                  
                      }
                    })
              
                      ######   On submit button ######
                        
                      observeEvent(input$Gobutton, {
                                       
             if(is.numeric(input$size_parameter) & is.numeric(input$install_parameter) & is.numeric(input$reviews_parameter)& is.numeric(input$days_parameter) )
                 {
                     
                    #### For Size can't be 0 ####     
               
                    observeEvent(input$size_parameter,{
                            
                          if(input$size_parameter == 0){
                              
                                shinyalert("Warning!", "Size of an App can't be 0")
                            }
                      
                       else{
                         
                         #### No. of Install can't be 0 ####  
                         
                      observeEvent(input$install_parameter, {
                          
                          if(input$install_parameter == 0){
                            
                            shinyalert("Warning!", "No. of Install can't be 0")
                            
                          }
                          else   
                          {
                            x <-  round(model(),digits = 2) 
                            
                            ######   printing the ratings ######
                            
                            output$print <- renderPrint({
                              
                              x <-  round(model(),digits = 2)
                              
                              
                              if(x >= 4){
                                
                                fluidRow(
                                  column(width = 12, offset = 12,
                                         valueBox(x, "Predicted Ratings", icon = icon("smile"), color = "aqua",width = 12)
                                  ))
                                
                              }
                              else if (x<4 & x>3.5){
                                
                                fluidRow(
                                  column(width = 12, offset = 12,
                                         valueBox(x, "Predicted Ratings", icon = icon("frown-open"), color = "aqua",width = 12)
                                  ))
                                
                              }
                              else
                              {
                                fluidRow(
                                  column(width = 12, offset = 12,
                                         valueBox(x, "Predicted Ratings", icon = icon("frown"), color = "aqua" ,width = 12)
                                  ))
                                
                              }
                              
                            })
                            
                            ######   Final Dialog Box ######
                            
                            showModal(modalDialog(
                              
                              title = "Predicted Ratings of an App",x, easyClose = TRUE )
                            )
                          }
                          
                        })
                        
                      }
                      
                    })
                    
                }
          
                  else
                        {
                            shinyalert("Alert!", "please enter numeric values", type = "error")
                                          
                          }	
                                        
                        })
                                                
    
                        ############## Random Forest model #############
             model <- reactive({

               load("RandomForest.rda")

               Pol <- c(input$polarity_parameter)
               Reviews <- c(input$reviews_parameter)
               Size <- c(input$size_parameter)

               Sentiment <- c('Sentiment_Positive','Sentiment_Negative','Sentiment_Neutral')
               w <- as.numeric(Sentiment == 'Sentiment_Negative')
               S1 <- data.frame(Sentiment,w,stringsAsFactors=FALSE)
               S2 <-  dcast(S1, w ~ Sentiment)
               S2 <- S2[S2$w == 1, ]
               S2<-S2[,c(2:4)]
               S2[is.na(S2)] <- 0

               Type <- c('Type_Free','Type_Paid')
               w1 <- as.numeric(input$appType_parameter)
               S11 <- data.frame(Type,w1,stringsAsFactors=FALSE)
               S21 <-  dcast(S11, w1 ~ Type)
               S21<- S21[S21$w == 1, ]
               S21<-S21[,c(2:3)]
               S21[is.na(S21)] <- 0
               S21

               Category <- c("Category_TOOLS","Category_COMMUNICATION","Category_SPORTS","Category_PERSONALIZATION","Category_SOCIAL",
                             "Category_HEALTH_AND_FITNESS","Category_FAMILY","Category_SHOPPING","Category_LIFESTYLE",
                             "Category_BOOKS_AND_REFERENCE","Category_MEDICAL","Category_GAME","Category_PHOTOGRAPHY","Category_TRAVEL_AND_LOCAL",
                             "Category_DATING","Category_ART_AND_DESIGN","Category_FOOD_AND_DRINK","Category_VIDEO_PLAYERS","Category_PRODUCTIVITY",
                             "Category_MAPS_AND_NAVIGATION","Category_FINANCE","Category_NEWS_AND_MAGAZINES","Category_EDUCATION","Category_BUSINESS",
                             "Category_WEATHER","Category_ENTERTAINMENT","Category_AUTO_AND_VEHICLES","Category_AUTO_AND_VEHICLES",
                             "Category_HOUSE_AND_HOME","Category_EVENTS","Category_LIBRARIES_AND_DEMO","Category_PARENTING","Category_COMICS","Category_BEAUTY")

               w11 <- as.numeric(input$category_parameter)
               S111 <- data.frame(Category,w11,stringsAsFactors=FALSE)
               S211 <-  dcast(S111, w11 ~ Category)
               S211<- S211[S211$w == 1, ]
               S211<-S211[,c(2:34)]
               S211[is.na(S211)] <- 0
               S211

               Content_Rating <- c("Content_Rating_Everyone","Content_Rating_Mature_17","Content_Rating_Everyone_10","Content_Rating_Teen","Content_Rating_Adults_only_18")
               w111 <- as.numeric(input$contentRating_parameter)
               S1111 <- data.frame(Content_Rating,w111,stringsAsFactors=FALSE)
               S2111 <-  dcast(S1111, w111 ~ Content_Rating)
               S2111<- S2111[S2111$w == 1, ]
               S2111<-S2111[,c(2:6)]
               S2111[is.na(S2111)] <- 0
               S2111

               Installs_1 <- c(input$install_parameter)
               Final_Price <- c(input$price_parameter)

               No_of_days_till_last_update <- (input$days_parameter)

               Final1 <- data.frame(Pol, Reviews, Size,Installs_1,Final_Price,No_of_days_till_last_update)

               Final <- cbind(Final1,S2,S21,S211,S2111)

               Predicted_Rating <- predict(rf, newdata = Final)

               Predicted_Rating <- as.vector(Predicted_Rating)

             })

             
}

shinyApp(ui, server)