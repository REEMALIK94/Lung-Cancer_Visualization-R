library(shiny)
library(shinydashboard)
library(ggplot2)
german_data<-read.csv("german_credit_data (1).csv")
ui <- dashboardPage(skin="purple",
                    dashboardHeader(
                      title="Assignment-2-Reewa Malik-MDS202134", titleWidth =450),
                    dashboardSidebar(
                      width=250,
                      sidebarMenu(
                        menuItem("Problem-1-BUILD SIMULATOR",tabName = "menu1"),
                        menuItem("Problem-2-DATA VISUALIZATION",tabName = "menu2")
                      )
                    ),
                    dashboardBody(
                      
                      tabItems(
                        tabItem("menu1",
                                tabsetPanel(
                                  tabPanel("Gamma", fluidPage(
                                    fluidRow(
                                      numericInput("simsizeG","Simulation Size",1000),
                                      numericInput("sampsizeG","Sample Size",30),
                                      numericInput("meanofG","Mean of Pop",20),
                                      numericInput("sdofG","SD of Pop",5),
                                      #for displaying all the plots
                                      actionButton("pltG","PLOTS"),
                                      #for Histogram
                                      plotOutput("plotG"),
                                      #for Q-Q plot
                                      plotOutput("qqplotG"),
                                      #For Kolmogrov Normality test, this will result in a P value
                                      textOutput("KGStestG")
                                      
                                      
                                      
                                    ))),
                                  tabPanel("Uniform",fluidPage(
                                    fluidRow(
                                      numericInput("simsizeU","Simulation Size",1000),
                                      numericInput("sampsizeU","Sample Size",30),
                                      numericInput("meanofU","Mean of Pop",20),
                                      numericInput("sdofU","SD of Pop",5),
                                      actionButton("pltU","PLOTS")),
                                    mainPanel(
                                      plotOutput("plotU"),
                                      plotOutput("qqplotU"),
                                      textOutput("KGStestU"))    
                                    
                                  )))),
                        tabItem("menu2",
                                tabsetPanel(
                                  tabPanel("TASK-1",
                                           fluidPage(
                                             fluidRow(
                                               selectInput("select", "Choose One Categorical Variable from the given List", 
                                                           choices = list("Sex", "Job",
                                                                          "Housing","Saving.accounts","Checking.account","Purpose"), selected = 1))
                                             , actionButton("PieBar","PLOT")
                                             
                                             , plotOutput("Pie"),
                                             plotOutput("Bar"))
                                  ),
                                  tabPanel("TASK-2",fluidPage(
                                    fluidRow(
                                      selectInput("chooseC", "Choose One Categorical Variable from the given List", 
                                                  choices = list("Sex", "Job",
                                                                 "Housing","Saving.accounts","Checking.account","Purpose"), selected = 1))
                                    ,selectInput("chooseN", "Choose One Numerical Variable from the given List", 
                                                 choices = list("Age", "Credit.amount",
                                                                "Duration"), selected = 1)),
                                    actionButton("BoxPlot","PLOT")
                                    
                                    , plotOutput("Box")
                                  )
                                )
                        )
                      )
                      
                    ))



server <- function(input,output){ 
  #Gamma Parameters
  gamma_para1<-eventReactive(input$pltG,{
    ((input$meanofG)/(input$sdofG))^2
  })
  gamma_para2<-eventReactive(input$pltG,{
    (input$meanofG)/((input$sdofG)^2)
  })
  #Simulating Gamma
  simu_G<-eventReactive(input$pltG,{
    rgamma(input$simsizeG,gamma_para1(),gamma_para2())
  })  
  #Sample Mean from Gamma
  samp_meanG<-eventReactive(input$pltG,{
    
    replicate((input$simsizeG)/5,mean(sample(simu_G(),input$sampsizeG,TRUE)))
  })
  #uniform(a,b),finding a and b
  unif_para1<-eventReactive(input$pltU,{
    input$meanofU-input$sdofU*sqrt(3)
  })
  unif_para2<-eventReactive(input$pltU,{
    input$meanofU+input$sdofU*sqrt(3)
  })
  #Simulating Uniform 
  simu_U<-eventReactive(input$pltU,{
    runif(input$simsizeU,unif_para1(),unif_para2())
  })  
  #Sample Means from Uniform
  samp_meanU<-eventReactive(input$pltU,{
    
    replicate(input$simsizeU,mean(sample(simu_U(),input$sampsizeU,TRUE)))
  })
  
  # Histogram for Sample Means from Gamma Population
  output$plotG<-renderPlot({
    hist(samp_meanG(),main="Histogram of Sample Means",xlab="Sample Means",ylab="Density",border="Green",col="orange",prob=TRUE)
  })
  #Q-Q plot for Sample means of Gamma Population  
  output$qqplotG<-renderPlot({
    qqnorm(samp_meanG())
    qqline(samp_meanG(), col = "Red", lwd = 2)})
  #Kolmogorv-Smironov’s test of normality for Gamma
  output$KGStestG = renderPrint(
    {
      print(ks.test(samp_meanG(),"pnorm"))
    }
  )
  
  # Histogram for Sample Means from Uniform Population
  output$plotU<-renderPlot({
    bins <- seq(unif_para1(),unif_para2(),length=200)
    hist(samp_meanU(),main="Histogram of Sample Means",xlab="Sample Means",ylab="Density", breaks = bins, col = 'orange', border = 'white')})
  

  #Q-Q plot for Sample means of Uniform Population
  output$qqplotU<-renderPlot({
    qqnorm(samp_meanU())
    qqline(samp_meanU(), col = "Red", lwd = 2)})
  #Kolmogorv-Smironov’s test of normality for Uniform
  output$KGStestU = renderPrint(
    {
      print(ks.test(samp_meanU(),"pnorm"))
    }
  )
  # pie chart and bar chart for the selected categorical variable
  german_dat1 = eventReactive(input$PieBar,{prop.table(table(german_data[input$select]))})
  
  german_dat2 = eventReactive(input$PieBar, 
                              {
                                x = as.data.frame(100*german_dat1())
                                colnames(x) = c("Categories", "frequency")
                                x[2] = round(x[2],1)
                                x
                              })
  
  
  
  choosen_categ = eventReactive(input$BoxPlot,{input$chooseC})
                                
  choosen_num = eventReactive(input$BoxPlot,{input$chooseN})
                
  #Pie Chart
  output$Pie<-renderPlot(
    {
      pc = ggplot(data = german_dat2(), aes(x="",y=frequency,fill=Categories)) + 
        geom_col() + coord_polar("y",start=0) + 
        labs(title="Pie Chart")+geom_text(aes(label = frequency),position = position_stack(vjust = 0.5))
      pc1=pc+scale_fill_brewer(palette="Dark2")
      pc1
    }
  )
  #Bar Chart
  output$Bar<-renderPlot(
    {
      barplot(german_dat1(),col="pink")
    }
  )
  #box Plot 
  output$Box = renderPlot(
    {
      ggplot(german_data, 
             aes_string(x = choosen_categ(), y =choosen_num() , fill = choosen_categ())) + 
        geom_boxplot(outlier.colour = "red", outlier.shape = 1, outlier.size = 3, 
                     notch = FALSE) + 
        labs(title="Numerical Variable vs Categorical Variable",
             x ="Categegorical", y = "Numerical")+scale_fill_brewer(palette="Dark2")
      
    }
  )
}
shinyApp(ui,server)