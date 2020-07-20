# https://www.overdosefreepa.pitt.edu/know-the-facts/view-overdose-death-data/

library(shiny)
library(lubridate)
library(dplyr)
library(DT)

source("readData.R")  # Load the data from the CSV file.     

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Accidental ODs in Allegheny County"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          

            selectInput(inputId = "year", label = "Select a Year:", 
                        choices = c(seq(min(year(na.omit(AccidentalOD_Allegheny_County$death_date_and_time))),
                                    max(year(na.omit(AccidentalOD_Allegheny_County$death_date_and_time))), 1) )),
            
            
            # WHERE YOUR FOOTER GOES
            hr(),
            print(paste0("Prahlad G Menon, PhD [Data Updated: ", format(as.Date("2019-12-31"), "%m/%d/%Y" )," ] "))  ,
            
        
    ),
    
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(title = "Main drugs involved in a given year", 
                     
              selectInput(inputId = "AgeGrp", label = "Select an Age Group:", choices= as.character(unique(AccidentalOD_Allegheny_County$AgeGrp))) , 
              plotOutput("DrugInvolved"),
              h1("Tabular summary:"),
              DTOutput("tableDrugs01")
            ),
            
            tabPanel(title = "Key Age Groups involved", 
                     plotOutput("AgesInvolved")
            ),
            
            
            tabPanel(title = "Race Distribution for ODs in a given year", 
                     plotOutput("Race")
            ),
            
            tabPanel(title = "Accidental ODs over time", 
                     plotOutput("yearPlt")
            )   
            
        )
      )
    )
    )
    

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$yearPlt <- renderPlot({
      
      Acc_OD_countsByYear <- AccidentalOD_Allegheny_County %>% group_by(case_year) %>% summarize(count=n())
      barplot(Acc_OD_countsByYear$count[order(Acc_OD_countsByYear$count)], 
              names.arg = Acc_OD_countsByYear$case_year[order(Acc_OD_countsByYear$case_year)], 
              main="Case-Year-Grouping of Accidental ODs", horiz = T, las=2)
      
      plot(as.numeric(as.character(unlist(Acc_OD_countsByYear$case_year))), 
           Acc_OD_countsByYear$count, xlab = "Year", ylab="Case Counts", type="l")
      
      
    })
    
    
    output$AgesInvolved <- renderPlot ({ 
      
      AccidentalOD_Allegheny_County <- AccidentalOD_Allegheny_County %>% filter(case_year %in% input$year)
      
      #  Convert Age into "AGE GROUP BuCKETS"  
      AccidentalOD_Allegheny_County <- AccidentalOD_Allegheny_County %>% mutate(AgeGrp = cut(age, breaks = c(0, 18, 21, 30, 40, 50, 60, 100)))
      AgeFreqTable <- as.data.frame(table(AccidentalOD_Allegheny_County$AgeGrp))
      barplot(AgeFreqTable$Freq, names.arg = AgeFreqTable$Var1, main="Age-Grouping of Accidental ODs", horiz = T, las=2)
      barplot(AgeFreqTable$Freq[order(AgeFreqTable$Freq)], names.arg = AgeFreqTable$Var1[order(AgeFreqTable$Freq)], main="Age-Grouping of Accidental ODs", horiz = T, las=2)
      
      
    })
    
    output$tableDrugs01 <- renderDT( DT::datatable(runData()),
      options = list(
        pageLength = 5 #,
        #initComplete = I("function(settings, json) {alert('Done.');}")
      )
    )
    
    
    runData <- reactive ({
      AccidentalOD_Allegheny_County <- AccidentalOD_Allegheny_County %>% filter(case_year %in% input$year) %>% filter(AgeGrp %in% input$AgeGrp)
      #Most common Drug involved with OD's  
      DrugFreqTable <- as.data.frame(table(c(AccidentalOD_Allegheny_County$combined_od1,
                                             AccidentalOD_Allegheny_County$combined_od2,
                                             AccidentalOD_Allegheny_County$combined_od3,
                                             AccidentalOD_Allegheny_County$combined_od4,
                                             AccidentalOD_Allegheny_County$combined_od5,
                                             AccidentalOD_Allegheny_County$combined_od6,
                                             AccidentalOD_Allegheny_County$combined_od7,
                                             AccidentalOD_Allegheny_County$combined_od8,
                                             AccidentalOD_Allegheny_County$combined_od9,
                                             AccidentalOD_Allegheny_County$combined_od10
      ))) ##print table and order by freq
      
      
      df <- as.data.frame(DrugFreqTable)
      colnames(df) <- c("Drug", "# Cases")
      return(df[order(df$`# Cases`, decreasing=T),])
    })
    
    output$DrugInvolved <- renderPlot ({ 
      
      # print(input$AgeGrp)
      df <- as.data.frame(runData () )
      par(mar=c(0,5,5,1))
      barplot(tail(as.numeric(df$`# Cases`[order(df$`# Cases`)])), names.arg = tail(as.character(df$"Drug"[order(df$`# Cases`)])), main="Drug-Grouping of Accidental ODs", horiz = T, las=2)
      
      
      })
    
    
    output$Race <- renderPlot ({ 
      
      AccidentalOD_Allegheny_County <- AccidentalOD_Allegheny_County %>% filter(case_year %in% input$year)
      #Most common Drug involved with OD's  
      DrugFreqTable <- as.data.frame(table(AccidentalOD_Allegheny_County$race)) ##print table and order by freq
      par(mar=c(0,5,5,1))
      barplot(tail(DrugFreqTable$Freq[order(DrugFreqTable$Freq)]), names.arg = tail(DrugFreqTable$Var1[order(DrugFreqTable$Freq)]), main="Drug-Grouping of Accidental ODs", horiz = T, las=2)
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
