#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dplyr)
require(lubridate)
library(ggplot2)
library(deltafish)
require(readr)

create_fish_db()
cat("finished creating fish database")
con<-open_database()
surv<-open_survey(con)%>%
  mutate(StationID=paste(Source, Station),
         Year=as.integer(year(Date)),
         Month=as.integer(month(Date)))

fish<-open_fish(con)

surveys<-surv%>%
  distinct(Source)%>%
  collect()%>%
  pull(Source)

years<-surv%>%
  distinct(Year)%>%
  collect()%>%
  pull(Year)%>%
  range()

species<-fish%>%
  distinct(Taxa)%>%
  collect()%>%
  pull(Taxa)%>%
  sort()

length_max<-fish%>%
  filter(Length>0)%>%
  select(Length)%>%
  collect()%>%
  pull(Length)%>%
  max()

suisun_samples<-surv%>%
  filter(Source=="Suisun")%>%
  select(SampleID)%>%
  collect()%>%
  pull(SampleID)
gc()

#Settings for the "data crunching" message.
info_loading <- "Crunching data"
progress_color <- "black"
progress_background <- "#c5c5c9"

# Define UI for application that draws a histogram
ui <- fluidPage(

  a(shiny::icon("reply"), "Delta Science shinyapps homepage", href="https://deltascience.shinyapps.io/Home/"),
  # Application title
  titlePanel(title=div(h1("Delta fish database", style="display: inline-block"),
                       a(img(src="logo.png", height = 100, align="right", style="display: inline-block"), href="https://delta-stewardship-council.github.io/deltafish/"),
                       h5("If you encounter any issues, please email ", a("sam.bashevkin@waterboards.ca.gov.",
                                                                          href="mailto:sam.bashevkin@waterboards.ca.gov?subject=Fish%20data%20Shiny%20app"))),
             windowTitle = "Delta fish database"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(#Instructions
      actionBttn("Instructions", "Instructions", style="simple", color="primary", icon=icon("question-circle")),
      br(), br(),
      pickerInput("Surveys",
                  "Select surveys:",
                  choices = surveys,
                  multiple = TRUE,
                  selected=surveys,
                  options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
      pickerInput("Months",
                  "Select months:",
                  choices = 1:12,
                  multiple = TRUE,
                  selected=1:12,
                  options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
      sliderInput("Years",
                  "Select years:",
                  min = years[1],
                  max=years[2],
                  value=years,
                  step=1,
                  sep=""),
      pickerInput("Species",
                  "Select species:",
                  choices = species,
                  multiple = TRUE,
                  options=list("selected-text-format" = "count > 3",
                               "live-search" = TRUE,
                               "max-options" = 10,
                               "max-options-text" = "You can only select 10 species")),
      prettySwitch("Aggregate",HTML("<b>Sum counts over all lengths?</b>"), status = "success", fill = TRUE, bigger=TRUE),
      conditionalPanel(condition="input.Aggregate",
                       h4("Standard length cutoff (mm; Suisun survey)"),
                       fluidRow(column(6, numericInput("Standardmin", "Min", value = 0, min=0, max=length_max, step = 1)),
                                column(6, numericInput("Standardmax", "Max", value = length_max, min=0, max=length_max, step = 1))),
                       h4("Fork length cutoff (mm; all other surveys)"),
                       fluidRow(column(6, numericInput("Forkmin", "Min", value = 0, min=0, max=length_max, step = 1)),
                                column(6, numericInput("Forkmax", "Max", value = length_max, min=0, max=length_max, step = 1)))),
      actionBttn("Run", "Run/Update", style="bordered", icon = icon("play"), color="danger", size="sm"),
      h2("Rows:"),
      textOutput("rows"),
      h2("Can you safely open this in excel?"),
      textOutput("excel"),
      h2("Estimated CSV size:"),
      textOutput("size"),
      actionBttn("Download", "Download data", style="simple", color="royal", icon=icon("file-download"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("dataPlot")
    )
  ),
  # Display the "data crunching" message.
  tags$head(tags$style(type="text/css",
                       paste0("
                                             #loadmessage {
                                             position: fixed;
                                             top: 25%;
                                             left: 25%;
                                             width: 50%;
                                             padding: 30px 0px 30px 0px;
                                             text-align: center;
                                             font-weight: bold;
                                             font-size: 200%;
                                             color: ", progress_color,";
                                             background-color: ", progress_background,";
                                             z-index: 105;
                                             border: 2px solid black;
                                             border-radius: 50px;
                                             }
                                             "))),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div(tags$i(class = "fa fa-spinner", style = "color: black"), info_loading, tags$i(class = "fa fa-spinner", style = "color: black"), id="loadmessage")),
  tags$style(type="text/css", ".recalculating {opacity: 1.0;}")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #Popup for app instructions
  observeEvent(input$Instructions, {
    sendSweetAlert(session, title = "Instructions",
                   text = tags$span(tags$p("This app works best if you select a subset of the data, rather than trying to obtain the full dataset.
                                           You will not be allowed to select more than 10 fish species, since that will crash the app.
                                           If you wish to access the full dataset, you can do so at the",
                                           a("data publication.", href="https://doi.org/10.6073/pasta/a29a6e674b0f8797e13fbc4b08b92e5b")),
                                    tags$p("If you are an R user, you can access the database with more advanced options using the R package",
                                           a("deltafish.", href="https://delta-stewardship-council.github.io/deltafish/")),
                                    tags$p("The 'sum counts over all lengths' option allows you to decide whether to keep the data as length frequency
                                           data (the number of captured fish in each measured length category) or to ignore length and add up the
                                           total number of each species captured in each trawl. If you decide to sum counts over all lengths,
                                           you are given the option to only select fish within a given size range, so you can get the total number
                                           of fish in your desired length range. This can be helpful to exclude small fish that are not always counted in the surveys."),
                                    tags$p(tags$b("Please read the full documentation in the", a("data publication", href="https://doi.org/10.6073/pasta/a29a6e674b0f8797e13fbc4b08b92e5b"),
                                                  "before using these data. There are important details to take into account, such as the inconsistency in the fish length unit.")),
                                    "------------------------------------------",
                                    tags$p(tags$b("App created and maintained by Sam Bashevkin.
                                                  Please email", a("Sam", href="mailto:sam.bashevkin@waterboards.ca.gov?subject=Fish%20data%20Shiny%20app"), "with any questions."))),
                   type = "info",
                   btn_labels = "Ok", html = F, closeOnClickOutside = TRUE)
  })

  # Set default if input$Species is NULL
  species_filt<-reactive({
    if(is.null(input$Species)){
      species
    }else{
      input$Species
    }
  })

  # Filter the data to user inputs
  data_filt<-eventReactive(input$Run, {
    req(species_filt)
    gc()

    # Set default if input$Surveys is NULL
    if(is.null(input$Surveys)){
      survey_filt<-surveys
    }else{
      survey_filt<-input$Surveys
    }

    # Set default if input$Years is NULL
    if(is.null(input$Years)){
      year_filt<-years
    }else{
      year_filt<-input$Years
    }

    year_min<-min(year_filt)
    year_max<-max(year_filt)

    # Set default if input$Month is NULL
    if(is.null(input$Months)){
      month_filt<-1:12
    }else{
      month_filt<-input$Months
    }

    if(input$Aggregate){
      # If user wishes to aggregate, we need the length ranges to be set
      req(input$Standardmin, input$Standardmax, input$Forkmin, input$Forkmax)
    }

    # Filter the data, but only if the user actually deselects any values of each variable (to save time)
    out<-surv%>%
      {if(length(survey_filt)<length(surveys)){
        filter(., Source%in%survey_filt)
      }else{
        .
      }}%>%
      {if(length(month_filt)<12){
        filter(., Month%in%month_filt)
      }else{
        .
      }}%>%
      {if(year_min>min(years) | year_max<max(years)){
        filter(., Year>=year_min & Year<=year_max)
      }else{
        .
      }}%>%
      left_join(fish%>%
                  {if(length(species_filt())<length(species)){
                    filter(., Taxa%in%local(species_filt()))
                  }else{
                    .
                  }}%>%
                  {if(input$Aggregate){
                    # Filter lengths before summarising only if user wishes to aggregate across lengths
                    # Fork length for all surveys except Suisun, which uses standard length
                    filter(., (!SampleID%in%suisun_samples & Length>=local(input$Forkmin) & Length<=local(input$Forkmax)) |
                             (SampleID%in%suisun_samples & Length>=local(input$Standardmin) & Length<=local(input$Standardmax)) |
                             is.na(Length))%>%
                      compute()%>%
                      group_by(SampleID, Taxa)%>%
                      summarise(Count=sum(Count, na.rm=T), .groups="drop")
                  }else{
                    .
                  }},
                by="SampleID")
    return(out)

  })

  # Number of rows of dataset from data_filt()
  rows<-reactive({
    req(data_filt)
    data_filt()%>%
      select(SampleID)%>%
      collect()%>%
      nrow
  })

  # Format row number for display
  output$rows <- renderText({
    req(rows)
    format(rows(), big.mark=",")
  })

  # Tell users if they can safely open the dataset in excel
  output$excel <- renderText({
    req(rows)
    if(rows()>1048576){
      "No"
    }else{
      "Yes"
    }
  })

  # Estimate CSV file size (this will be an over-estimate for aggregated data since it is missing some columns)
  output$size <- renderText({
    req(rows)
    size<-rows()*0.00025*1048576 # estimated by writing csvs of different numbers of rows and extracting file size, then converting to bytes
    class(size)<-"object_size"
    return(format(size, units="auto"))
  })

  # Create very simple plot of total catch of each species per year
  output$dataPlot <- renderPlot({
    req(data_filt)
    plot_data<-data_filt()%>%
      select(Year, Taxa, Count)%>%
      group_by(Year, Taxa)%>%
      summarise(Count=sum(Count, na.rm=T), .groups="drop")%>%
      collect()

    p<-ggplot(plot_data, aes(x=Year, y=Count, fill=Taxa))+
      geom_bar(position="stack", stat="identity")+
      theme_bw()

    # Remove legend if > 10 fish species are present
    if(length(unique(plot_data$Taxa))>10){
      p<-p+theme(legend.position="none")
    }
    return(p)
  })

  #Show modal dialog to save data when Download button is clicked
  observeEvent(input$Download, {
    showModal(ModalDownloadData())
  })

  #Modal dialog (popup window) to download data
  ModalDownloadData<-function(){
    modalDialog(
      h1("Data info"),
      p("Please see the", a("data publication.", href="https://www.doi.org/10.6073/pasta/0cdf7e5e954be1798ab9bf4f23816e83"),
        "for all metadata associated with this dataset, as well as the citation information."),
      footer = tagList(modalButton("Cancel"),
                       downloadBttn("Downloaddata", "Download data", style="bordered", color = "primary", size="sm")),
      easyClose=TRUE
    )
  }

  #Download handler for data downloading
  output$Downloaddata <- downloadHandler(
    filename = function() {
      paste0("Delta fish ", gsub(":", ".", gsub("-", ".", Sys.time(), fixed=TRUE), fixed=TRUE), ".csv")
    },
    content = function(file) {
      data_filt()%>%
        collect()%>%
        write_csv(file)

      gc()
    }

  )

}

# Run the application
shinyApp(ui = ui, server = server)
