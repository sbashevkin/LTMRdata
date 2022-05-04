#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
require(lubridate)
library(ggplot2)
library(deltafish)

create_fish_db()

surv<-open_survey()%>%
    mutate(StationID=paste(Source, Station),
           Date=as_date(Date),
           Year=as.integer(year(Date)),
           Month=as.integer(month(Date)))

fish<-open_fish()

surveys<-surv%>%
    distinct(Source)%>%
    collect()%>%
    pull(Source)

# stations<-surv%>%
#     distinct(StationID)%>%
#     collect()%>%
#     as.vector()

years<-surv%>%
    distinct(Year)%>%
    collect()%>%
    pull(Year)

species<-fish%>%
    filter(Count>0)%>%
    group_by(Taxa)%>%
    summarise()%>%
    collect()%>%
    pull(Taxa)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("deltafish"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("Surveys",
                        "Select surveys:",
                        choices = surveys,
                        multiple = TRUE),
            selectInput("Months",
                        "Select months:",
                        choices = 1:12,
                        multiple = TRUE),
            selectInput("Years",
                        "Select years:",
                        choices = years,
                        multiple = TRUE),
            # selectInput("Stations",
            #             "Select sampling stations:",
            #             choices = stations,
            #             multiple = TRUE),
            selectInput("Species",
                        "Select species:",
                        choices = species,
                        multiple = TRUE),
            h2("Rows:"),
            textOutput("rows"),
            h2("Estimated CSV size:"),
            textOutput("size")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("dataPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    survey_filt<-reactive({
        if(is.null(input$Surveys)){
            surveys
        }else{
            input$Surveys
        }
    })

    month_filt<-reactive({
        if(is.null(input$Month)){
            1:12
        }else{
            input$Month
        }
    })

    year_filt<-reactive({
        if(is.null(input$Years)){
            as.integer(years)
        }else{
            as.integer(input$Years)
        }
    })

    species_filt<-reactive({
        if(is.null(input$Species)){
            species
        }else{
            input$Species
        }
    })

    data_filt<-reactive({
        req(survey_filt, month_filt, year_filt, species_filt)
        surv%>%
            filter(Source%in%local(survey_filt()) & Month%in%local(month_filt()) & Year%in%local(year_filt()))%>%
            left_join(fish%>%
                          filter(Taxa%in%local(species_filt())),
                      by="SampleID")
    })

    rows<-reactive({
        req(data_filt)
        data_filt()%>%
            select(SampleID)%>%
            collect()%>%
            nrow
    })
    output$rows <- renderText({
        req(rows)
            format(rows(), big.mark=",")
    })

    output$size <- renderText({
        req(rows)
        size<-rows()*0.00025*1048576 # estimated by writing csvs of different numbers of rows and extracting file size, then converting to bytes
        class(size)<-"object_size"
        return(format(size, units="auto"))
    })

    output$dataPlot <- renderPlot({
        plot_data<-data_filt()%>%
            group_by(Year, Taxa)%>%
            summarise(Count=sum(Count, na.rm=T), .groups="drop")%>%
            collect()

        p<-ggplot(plot_data, aes(x=Year, y=Count, fill=Taxa))+
            geom_bar(position="stack", stat="identity")+
            theme_bw()

        if(length(species_filt())>10){
            p<-p+theme(legend.position="none")
        }
        return(p)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
