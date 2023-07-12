
# Fitness & Diet R Project by Pranay Varade and Kshitij Ghodke
# 


#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("ggplot2")
#install.packages("plotrix")



library(shiny)
library(shinythemes)
library(ggplot2)
library(plotrix)

#User Interface

ui <- fluidPage(
  titlePanel("Fitness & Diet Analysis"),
  sidebarLayout(
    sidebarPanel(
      p(h1("BMI Calculator for Adults")),
      numericInput("mass_kg", label = strong("Input your weight (KG)"),0), 
      br(),
      numericInput("height_cm", label = strong("Input your height (CM)"),0),
      br(),
      actionButton("YourBMI", label = "Calculate")),
    # sidebarPanel(
    #   # sidebarPanel(
    #   #   h4("No. of Respondants based on Age")
    #   # ),
    # 
    #   #Mainpanel plotting the histogram for user selected field
    #   mainPanel(
    #     plotOutput("outputID"),
    #     position = c("left" ),
    #     fluid = TRUE
    #   )
    # ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Show your BMI",
                 p(h3("Input measurement is:")),
                 textOutput("current_weight"),
                 textOutput("current_height"),
                 br(),
                 p(h3("Your BMI calculated is:")),
                 textOutput("BMI_result"),
                 p(h3("This means you are:")),
                 textOutput("indicator_show")),
        tabPanel("Visualization",
                 fluidPage(theme = shinytheme("united"),
                           
                           #Creating a navigation bar and embedding the sidebars in it.
                           navbarPage(title = h1("Fitness & Diet Analysis"),
                                      tabPanel(
                                        #Tab1 title
                                        titlePanel(h4("Age")),
                                        # Sidebar inside tab 1
                                        sidebarLayout(
                                          #Histogram sidebar
                                          sidebarPanel(
                                            h4("No. of Respondants based on Age")
                                          ),
                                          #Mainpanel plotting the histogram for user selected field
                                          mainPanel(
                                            plotOutput("outputID1"),
                                            position = c("right"),
                                            fluid = TRUE)
                                        )
                                      ) 
                                      ,
                                      
                                      # exercise
                                      tabPanel(
                                        #Tab1 title
                                        titlePanel(h4("Exercise")),
                                        # Sidebar inside tab 1
                                        sidebarLayout(
                                          #Histogram sidebar
                                          sidebarPanel(
                                            h4("males VS females who do any exercise")
                                          ),
                                          #Mainpanel plotting the histogram for user selected field
                                          mainPanel(
                                            plotOutput("outputID2"),
                                            position = c("right"),
                                            fluid = TRUE)
                                        )
                                      )
                                      ,
                                      
                                      # ExerciseType 
                                      tabPanel(
                                        #Tab1 title
                                        titlePanel(h4("ExerciseType")),
                                        # Sidebar inside tab 1
                                        sidebarLayout(
                                          #Histogram sidebar
                                          sidebarPanel(
                                            h4("Type of exercises that males and female perform")
                                          ),
                                          #Mainpanel plotting the histogram for user selected field
                                          mainPanel(
                                            plotOutput("outputID3"),
                                            position = c("right"),
                                            fluid = TRUE)
                                        )
                                      )
                                      ,
                                      
                                      # BMI
                                      tabPanel(
                                        #Tab1 title
                                        titlePanel(h4("BMI")),
                                        # Sidebar inside tab 1
                                        sidebarLayout(
                                          #Histogram sidebar
                                          sidebarPanel(
                                            h4("BMI of all the respondants")
                                          ),
                                          #Mainpanel plotting the histogram for user selected field
                                          mainPanel(
                                            plotOutput("outputID4"),
                                            position = c("right"),
                                            fluid = TRUE)
                                        )
                                      )
                                      ,
                                      
                                      # Water
                                      tabPanel(
                                        #Tab1 title
                                        titlePanel(h4("Water")),
                                        # Sidebar inside tab 1
                                        sidebarLayout(
                                          #Histogram sidebar
                                          sidebarPanel(
                                            h4("Number of glasses of water that respondants consume in a day")
                                          ),
                                          #Mainpanel plotting the histogram for user selected field
                                          mainPanel(
                                            plotOutput("outputID5"),
                                            position = c("right"),
                                            fluid = TRUE)
                                        )
                                      )
                                      ,
                                      
                                      # Pranaayaam
                                      tabPanel(
                                        #Tab1 title
                                        titlePanel(h4("Pranaayaam")),
                                        # Sidebar inside tab 1
                                        sidebarLayout(
                                          #Histogram sidebar
                                          sidebarPanel(
                                            h4("Respondants who do Praanaayaam")
                                          ),
                                          #Mainpanel plotting the histogram for user selected field
                                          mainPanel(
                                            plotOutput("outputID6"),
                                            position = c("right"),
                                            fluid = TRUE)
                                        )
                                      )
                                      
                                      
                           )#navbarPage closed
                 )
        )
      )))
)


# server side

server <-function(input, output, session){
  values <- reactiveValues()
  observe({
    input$YourBMI
    values$bmi <- isolate({
      input$mass_kg/(input$height_cm/100 * input$height_cm/100)
    })
  })
  output$current_weight <- renderText({
    input$YourBMI
    paste("Current Weight(KG): ", isolate(input$mass_kg))
  })
  output$current_height <- renderText({
    input$YourBMI
    paste("Current Height(CM) :", isolate(input$height_cm))
  })
  output$indicator_show <- renderText({
    if(input$YourBMI == 0) "" else {
      if (values$bmi < 20.00){
        values$indicator_show = "Underweight , You need to start following a proper diet in order to gain some weight"
      }
      else if (values$bmi < 24.9){
        values$indicator_show =" Normal , Congrats!, You need to exercise and follow a diet (if currently you don't) to maintain your health"
      }
      else if (values$bmi < 29.9){
        values$indicator_show ="Overweight , You need to start exercising in order ot lose weight"
      }
      else{
        values$indicator_show ="Obese! , You need to start following a strict diet and exercising on immediate basis !!"
      }
      paste("", values$indicator_show)
    }
  })
  output$BMI_result <- renderText({
    if(input$YourBMI == 0) "" else
      paste("", values$bmi)
  })
  
  data<-read.csv("D:/PV/Mit/Sem-1/Data mining/R_miniProject/Fitness&Diet/newData.csv")
  
  #Function for plotting histogram
  output$outputID1 <- renderPlot({
    
    hist(data$age.yrs.,
         main = "No. of Respondants based on Age",
         xlab = "Age(years)",
         ylab = "No. of Respondants",
         col = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600")
    )
    
  }
  )
  
  #Function for plotting Exercise
  output$outputID2 <- renderPlot({
    
    ggplot(data, aes(x = gender, fill = exercise)) + geom_bar()
    
    
  }
  )
  
  #Function for plotting ExerciseType
  output$outputID3 <- renderPlot({
    
    ggplot(data, aes(x = exercise_type, fill = gender)) +
      geom_bar(position = "dodge")
    
    
  }
  )
  
  #Function for plotting BMI
  output$outputID3 <- renderPlot({
    
    ggplot(data, aes(x = exercise_type, fill = gender)) +
      geom_bar(position = "dodge")
    
  }
  )
  
  
  
  bmi<-data$BMI
  #people whose BMI<20.00
  less20<-length(bmi[bmi<20.00])
  less20
  
  
  #people whose BMI lies between 20.00 to 25.00
  bet20to25<-function(){
    a<-length(bmi[bmi<25.00])
    b<-a-less20
    return(b)
  }
  bet20to25()
  
  
  
  #people whose BMI lies between 25.00 to 30.00
  bet25to30<-function(){
    a<-length(bmi[bmi<30.00])
    b<-a-less20-bet20to25()
    return(b)
  }
  bet25to30()
  
  
  
  #people whose BMI>30.00
  greater30<-length(bmi[bmi>30.00])
  greater30
  
  vec<-c(less20,bet20to25(),bet25to30(),greater30)
  
  labelsOfPie<-c("below 20 " , "20 To 25 " , "25 To 30 " , "above 30 ")
  
  # Pie piechart with percentage and colour
  per<-round(vec/sum(vec)*100) #calcualiting percentage and rounding off
  labl1<-paste(labelsOfPie,per)
  labl1<-paste(labl1 , "%" , sep="") #creating labels for pie data
  
  
  #Function for plotting BMI
  output$outputID4 <- renderPlot({
    
    pie3D(vec, labels = labl1 ,
          explode = 0.2 , radius = 1.5 ,
          main="3D Pie chart for BMI ")
  }
  )
  
  
  #Function for plotting Water
  output$outputID5 <- renderPlot({
    
    hist(waterperday,
         main = "No. of Respondants based on Age",
         xlab = "No. of Glasses of Water",
         ylab = "No. of Respondants" ,
         col = c("red" , "yellow" , "gray" , "orange" , "gray" , "green" , "gray" , "purple")
    )
  }
  )
  
  pran<-data$pranaayaam
  pran
  pranNo<-length(pran[pran=="No"])
  pranNo
  
  pranYes<-length(pran[pran=="Yes"])
  pranYes
  
  pranay<-c(pranNo,pranYes)
  
  percen<-round(pranay/sum(pranay)*100)#calcualiting percentage and rounding off
  labels<-c("No" , "Yes")
  labels1<-paste(labels,percen)
  labels1<-paste(labels1 , "%" , sep="") #creating labels for pranaayaam pie data
  
  
  
  #Function for plotting Praanaayaam
  output$outputID6 <- renderPlot({
    
    pie(pranay , labels=labels1 , 
        col=rainbow(length(labels1)) , 
        main="Respondants who do Praanaayaam" , 
        radius=1)
    
  }
  )
  
}

shinyApp(ui, server)
