library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(rsconnect)
library(janitor)
library(DT)
library(gcookbook)
library(hrbrthemes)
library(RColorBrewer)


# Read in Data

#umd_arrest = readRDS("./data/umd_arrest.rds")
#umd_arrest_combined = readRDS("./data/arrest_combined.rds")

#umd_arrest = read.csv("https://raw.githubusercontent.com/ndmvisuals/university-police-logs/main/police-logs-app/data/umd_arrest.csv")
#umd_arrest_combined = read.csv("https://raw.githubusercontent.com/ndmvisuals/university-police-logs/main/police-logs-app/data/arrest_combined.csv")

umd_arrest = readRDS(url("https://raw.githubusercontent.com/ndmvisuals/university-police-logs/main/police-logs-app/data/umd_arrest.rds"))
umd_arrest_combined =  readRDS(url("https://raw.githubusercontent.com/ndmvisuals/university-police-logs/main/police-logs-app/data/arrest_combined.rds"))

df_latest_scrape_date = readRDS(url("https://raw.githubusercontent.com/ndmvisuals/university-police-logs/main/police-logs-app/data/latest_scrape_date.rds"))
scrape_date = df_latest_scrape_date$latest[[1]]


################## --------------------------
umd_arrest_list = unique(umd_arrest$type)
all_incident = "All"
umd_arrest_list = c(all_incident, umd_arrest_list)
umd_arrest_list = umd_arrest_list[is.na(umd_arrest_list) == FALSE]
ls_years = unique(umd_arrest$year)
min_year = min(ls_years)
max_year = max(ls_years)
cns_palette = c("#1979B9", "#FAA916", "#2EC4B6", "#8FD694", "#80217F", "#1979B9")
all_label = "All"

#color_scheme = "Dark2"
single_color = "#990000"


ui <- fluidPage(
  #tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")),
  
  ##########
  ## Page 1
  ##########
  navbarPage("University of Maryland Police Log Explorer", 
            
                      tabsetPanel(
                        tabPanel(title = "Charts",
                                 uiOutput("url_allow_popout_UI"),
                                 br(),
                                 tags$p(paste0("Data last scraped on: ", scrape_date, " EST")),
                                 br(),
                                 fluidRow(
                                   column(6, selectInput(inputId = "select_incident",
                                                         label = "Choose incident",
                                                         umd_arrest_list) ),
                                   column(6,checkboxInput("checkbox_race", "Aggregrate Years", value = FALSE) )
                                   
                                 )
                                 ,
                                 fluidRow(
                                   column(6, plotOutput("umd_arrest_year_graph")),
                                   column(6, plotOutput("umd_arrest_race_graph")) 
                                   
                                 ),
                                 br(),
                                 fluidRow(
                                   column(6, plotOutput("umd_arrest_time_graph")),
                                   column(6, plotOutput("umd_arrest_day_graph"))
                                 ),
                                 br()
                        ),
                        
                        
                        tabPanel(title = "Data Table",
                                 br(),
                                 fluidRow(
                                   column(3,selectInput("group", "One Entry Per: ", list("Charge","Arrest/Citation Case", "Person"), selected = "Charge")),
                                   #column(3,selectInput("vars", "Group By: ", names(umd_arrest), multiple = TRUE))
                                   column(3,htmlOutput("grouping_list"))
                                   
                                 ),
                                 br(),
                                 br(),
                                 fluidRow(
                                   column(12, DTOutput("umd_arrest_table2"))
                                   
                                 )
                        ),
                        
                        tabPanel(title = "About/Download Data",
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     
                                     selectInput("dataset_to_download", "Data",
                                                 choices = c("UMPD Arrest Report Ledger")),
                                     
                                     # Button
                                     downloadButton("downloadData", "Download")
                                     
                                     
                                     
                                   ),
                                   mainPanel(
                                     br(),
                                     tags$p("The University of Maryland Police Department publishes", a("daily crime and incident logs", href = "https://www.umpd.umd.edu/stats/incident_logs.cfm", target="_blank" ),
                                       "which provide basic information on all calls for service the police department responds to,
                                       and", a("arrest report ledgers", href = "https://www.umpd.umd.edu/stats/arrest_report.cfm", target="_blank"), "which provides basic information on arrests and citations."),
                                     
                                       tags$p("Capital News Service built a web scraper to download and aggregate this data daily and display it on this dashboard. See the GitHub repository - that runs the scraper and archives the data -", a("here.", href = "https://github.com/ndmvisuals/university-police-logs", target="_blank")),
                                     
                                     tags$p("If a service call results in an arrest or citation, then the UMPD case number will appear in the arrest ledger.
                                       In order to classify the primary type of the arrest/citation, the crime and incident log is joined to the arrest ledger by UMPD case number.
                                       However, if the case number is not included in the crime and incident log, its type is classified as N/A.
                                       A UMPD case number that appears in the arrest ledger could have multiple charges that differ from the single classification in the crime and incident log.")
                                     
                                 )
                                     
                                   )
                                 
                                 
                                 
                                 
                                 )
                        
                      )
                      
                      
                      
                      
  )
  
)


    

server <- function(input, output, session){
  ########################################## arrest ##########################################
  # UMD arrest ------------------------

  source("./url_allowPopout.R", local = TRUE)
  
  
  
  output$grouping_list = renderUI({
    
    if(input$group == "Person"){
      
      selectInput(inputId = "vars", #name of input
                  label = "Group On:", #label displayed in ui
                  choices = c("type","race","age","sex", "umpd_case_number", "arrest_number","date","time","week_day","year","month","time_hour"),
                  multiple = TRUE)
      
      
    }
    
    else if (input$group == "Arrest/Citation Case"){
      
      selectInput(inputId = "vars", #name of input
                  label = "Group On:", #label displayed in ui
                  choices = c("type","umpd_case_number", "arrest_number", "date","time","week_day","year","month","time_hour" ),
                  multiple = TRUE)
      
    }
    
    else{
      
      selectInput(inputId = "vars", #name of input
                  label = "Group On:", #label displayed in ui
                  choices = c("Cannot group - please choose arrest/citation case or person"))
      
      
    }
    
    
  })
  
  number_grouping_vars <- reactive(input$vars)
  grouping_var <- reactive(input$group)
  
  
 ####### Graph 1 Top Left Arrest Citation Cases by Year ################################# 
  
 df_umd_arrest_year <- reactive({
    #print(input$select_crime_hu) 
    req(input$select_incident)
    
    if(input$select_incident == all_incident){
      umd_arrest_combined
      
    }
   
    else{
      result_umd_arrest = umd_arrest_combined[umd_arrest_combined$type == input$select_incident,] %>% drop_na(type)
      
      
    }
   
    
  })

  
  
  # UMD arrest Graph
  output$umd_arrest_year_graph = renderPlot({
    
    if(input$select_incident == all_incident){
      
      ggplot(df_umd_arrest_year(), aes(x = year, y = number, fill = final_type))+
        geom_bar(stat = "identity")+
        labs(x = "Years", y = "Number of UMPD Cases",
             title = paste0("Primary Incident Type: ", all_label),
             subtitle = paste0("Arrest/Citation Cases By Year"),
             fill ="Incident Types")+
        #scale_fill_brewer(palette = color_scheme) +
        scale_fill_manual(values = cns_palette)+
        theme_ipsum_rc(grid="Y")+
        scale_x_continuous( breaks = ls_years)
      
    }
    
    else{
      ggplot(df_umd_arrest_year(), aes(x = year, y = number, fill = single_color))+
        geom_bar(stat = "identity", width = 0.8)+
        labs(x = "Years", y = "Number of UMPD Cases",
             title = paste0("Primary Incident Type: ", input$select_incident),
             subtitle =  paste0("Arrest/Citation Cases By Year"))+
        theme_ipsum_rc(grid="Y")+
        scale_x_continuous( breaks = ls_years)+
        theme(legend.position = "none")
      
      
    }
    
    
  
})
  
 
  
  
  
  
  ####### Graph 2 Top Right Arrest Citation Cases by Race ################################# 
  
  race_grouping <- reactive({
    if(input$checkbox_race == FALSE){
      
    }
    
    
    
  })
  
  df_umd_arrest_race_year <- reactive({
    #print(input$select_crime_hu) 
    req(input$select_incident)
    
    if(input$select_incident == all_incident){
      
      if(input$checkbox_race == FALSE){
        result_umd_arrest_race_year = umd_arrest  %>% 
          distinct(year, umpd_case_number, arrest_number, race, type) %>% 
          group_by(year, race) %>% 
          summarise(num_people = n())
        
      }
      
      else{
        
        result_umd_arrest_race_year = umd_arrest  %>% 
          distinct(year, umpd_case_number, arrest_number, race, type) %>% 
          group_by(race) %>% 
          summarise(num_people = n())
        
        
      }
      
      
        
      
    }
    
    else{
      if(input$checkbox_race == FALSE){
        
        result_umd_arrest_race_year = umd_arrest[umd_arrest$type == input$select_incident,]  %>% drop_na(type) %>% 
          distinct(year, umpd_case_number, arrest_number, race, type) %>% 
          group_by(year, race) %>% 
          summarise(num_people = n())
        
        
      }
      
      else{
        result_umd_arrest_race_year = umd_arrest[umd_arrest$type == input$select_incident,]  %>% drop_na(type) %>% 
          distinct(year, umpd_case_number, arrest_number, race, type) %>% 
          group_by(race) %>% 
          summarise(num_people = n())
        
        
      }
      
      
      
      
    }
    
    
  })
  
  
  
  output$umd_arrest_race_graph = renderPlot({
    
    if(input$checkbox_race == FALSE){
    
    ggplot(df_umd_arrest_race_year(), aes(x = year, y = num_people, fill = race))+
      geom_col(stat = "identity", position = "dodge", width = 0.8)+
      labs(x = "Years", y = "Number of People",
           title = paste0("Primary Incident Type: ",input$select_incident),
           subtitle = paste0("People Arrested/Cited By Race"),
           fill ="Race")+
      #scale_fill_brewer(palette = color_scheme) +
        scale_fill_manual(values = cns_palette)+
        theme_ipsum_rc(grid="Y")+
      scale_x_continuous( breaks = ls_years)
    }
    
    else{
      
      ggplot(df_umd_arrest_race_year(), aes(x = reorder(stringr::str_wrap(race, 10), -num_people, sum), y = num_people, fill = race))+
        geom_col(stat = "identity")+
        labs(x = "Years", y = "Number of People",
             title = paste0("Primary Incident Type: ", input$select_incident),
             subtitle = paste0("People Arrested/Cited By Race"),
             fill ="Race")+
        #scale_fill_brewer(palette = color_scheme) +
        scale_fill_manual(values = cns_palette)+
        theme_ipsum_rc(grid="Y")
      
      
      #subtitle = paste0("By race: ", toString(min_year), "-", toString(max_year))
        
      
      
      
    }
    
    
  })
  
  
  
  ####### Graph 3 Bottom Left Arrest Citation Cases by Time of Day ################################# 
  
  
 
  
  
  df_umd_arrest_time = reactive({
    
    req(input$select_incident)
    if(input$select_incident == all_incident){
      
      result_umd_arrest_time = umd_arrest %>% 
        distinct(year, umpd_case_number, .keep_all = TRUE) %>% 
        group_by(time_hour) %>% 
        count()
      
      
    }
    
    else{
      
      result_umd_arrest_time = umd_arrest[umd_arrest$type == input$select_incident,]  %>% 
        drop_na(type) %>% 
        distinct(year, umpd_case_number, .keep_all = TRUE) %>% 
        group_by(time_hour) %>% 
        count()
      
      
      
    }
  })
  
  output$umd_arrest_time_graph =renderPlot({
    
    ggplot(df_umd_arrest_time(), aes(x=time_hour, y=`n`, fill = single_color)) +
      geom_bar(stat="identity") +
      labs(x = "Time", y = "Number of UMPD Cases",
           title =  paste0("Primary Incident Type: ", input$select_incident),
           subtitle = paste0("Arrest/Citation Cases By Time of Day From ", toString(min_year), "-", toString(max_year)))+
      
      theme_ipsum_rc(grid="Y")+
      scale_x_discrete( labels = c("12 a.m.", "1 a.m.", "2 a.m.", "3 a.m.", "4 a.m.", "5 a.m.", "6 a.m.", "7 a.m.","8 a.m.", "9 a.m.", "10 a.m.", "11 a.m.",
                                   "12 p.m.", "1 p.m.", "2 p.m.", "3 p.m.", "4 p.m.", "5 p.m.", "6 p.m.", "7 p.m.","8 p.m.", "9 p.m.", "10 p.m.", "11 p.m."))+
      theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust=0.5))
    
    
    
    
  })
  
  
  
  
  ####### Graph 4 Bottom Right Arrest Citation Cases by Day of Week ################################# 
  
  df_umd_arrest_day = reactive({
    
    req(input$select_incident)
    if(input$select_incident == all_incident){
      
      result_umd_arrest_day = umd_arrest %>% 
        distinct(year, umpd_case_number, .keep_all = TRUE) %>% 
        group_by(week_day) %>% 
        count()
      
      
    }
    
    else{
      
      result_umd_arrest_day = umd_arrest[umd_arrest$type == input$select_incident,]  %>% 
        drop_na(type) %>% 
        distinct(year, umpd_case_number, .keep_all = TRUE) %>% 
        group_by(week_day) %>% 
        count()
      
      
      
    }
  })
  
  output$umd_arrest_day_graph =renderPlot({
    
    ggplot(df_umd_arrest_day(), aes(x=week_day, y=`n`, fill = single_color)) +
      geom_bar(stat="identity") +
      labs(x = "Day of Week", y = "Number of UMPD Cases",
           title =  paste0("Primary Incident Type: ", input$select_incident),
           subtitle = paste0("Arrest/Citation Cases By Day of Week From ", toString(min_year), "-", toString(max_year)))+
      
      theme_ipsum_rc(grid="Y")+
      #scale_x_discrete( labels = c("12 a.m.", "1 a.m.", "2 a.m.", "3 a.m.", "4 a.m.", "5 a.m.", "6 a.m.", "7 a.m.","8 a.m.", "9 a.m.", "10 a.m.", "11 a.m.",
                                   #"12 p.m.", "1 p.m.", "2 p.m.", "3 p.m.", "4 p.m.", "5 p.m.", "6 p.m.", "7 p.m.","8 p.m.", "9 p.m.", "10 p.m.", "11 p.m."))+
      theme(legend.position = "none")
    
    
    
    
  })
  
  
  
  ###############Table######################
  
  
  
  
  
  interactive_table = function(df, num_variables, grouping_variable){
    
    
    if(grouping_variable == "Person"){
      
      df = df %>% 
        select(-arrested_date_time_charge)
      
      if(num_variables < 1){
        
        result = df %>% 
          distinct(arrest_number, .keep_all = TRUE) %>% 
          select(-charge)
        return (result)
        
      }
      
      else{
        result = df %>% 
          distinct(arrest_number, .keep_all = TRUE) %>% 
          select(-charge) %>% 
          group_by(across(all_of(input$vars))) %>% 
          summarise(count = n(), .groups = "drop")
        
        return (result)
        
        
      } 
      
      
      
      
    }
    
    else if(grouping_variable == "Arrest/Citation Case") {
      
      df = df %>% 
        select(-arrested_date_time_charge)
      
      if(num_variables < 1){
        
        result = df %>% 
          distinct(umpd_case_number, .keep_all = TRUE) %>% 
          select(-race, -age, -sex, -charge, -arrest_number) %>% 
          relocate(type, .after = umpd_case_number)
        return (result)
        
      }
      
      else{
        result = df %>% 
          distinct(umpd_case_number, .keep_all = TRUE) %>% 
          select(-race, -age, -sex, -charge, -arrest_number) %>% 
          group_by(across(all_of(input$vars))) %>% 
          summarise(count = n(), .groups = "drop")
        
        return (result)
        
      } 
      
      
      
    }
    
    
    else{
      
      result = df %>% 
        select(-arrested_date_time_charge)
      
      return (result)
      
      
      
      
      
      
    }
    
    
    
    
    
    
       
    
  }
  
  output$umd_arrest_table2 <- renderDT(
        
        
        
       interactive_table(umd_arrest, length(number_grouping_vars()), grouping_var()),
        
        
        filter = "top",
        options = list(
          pageLength = 25
        )
        
      )
  
  
  
  ##### Downloadable data
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset_to_download,"_",substr(scrape_date,1,10), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(umd_arrest, file, row.names = FALSE)
    }
  )
      
}





shinyApp(ui = ui, server = server)