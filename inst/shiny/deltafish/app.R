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

stations<-surv%>%
    distinct(StationID)%>%
    collect()%>%
    as.vector()

years<-surv%>%
    distinct(Year)%>%
    collect()%>%
    pull(Year)%>%
    range()

species<-fish%>%
    filter(Count>0)%>%
    group_by(Taxa)%>%
    summarise()%>%
    collect()%>%
    pull(Taxa)

length_max<-fish%>%
    filter(Length>0)%>%
    select(Length)%>%
    collect()%>%
    pull(Length)%>%
    max()
gc()
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("deltafish"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
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
            pickerInput("Stations",
                        "Select sampling stations:",
                        choices = stations,
                        multiple = TRUE,
                        selected=stations,
                        options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
            pickerInput("Species",
                        "Select species:",
                        choices = species,
                        multiple = TRUE,
                        selecte=species,
                        options=list(`actions-box`=TRUE, `selected-text-format` = "count > 3")),
            prettySwitch("Aggregate",HTML("<b>Aggregate data?</b>"), status = "success", fill = TRUE, bigger=TRUE),
            conditionalPanel(condition="input.Aggregate",
                             sliderInput("Standardlength", "Standard length cutoff (mm; Suisun survey)", value = c(0, length_max), min=0, max=length_max, step = 1),
                             sliderInput("Forklength", "Fork length cutoff (mm; all other surveys)", value = c(0, length_max), min=0, max=length_max, step = 1)),
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
            years
        }else{
            input$Years
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

        year_filts<-year_filt()
        year_min<-min(year_filts)
        year_max<-max(year_filts)


        out<-surv%>%
            filter(Source%in%local(survey_filt()) & Month%in%local(month_filt()) & Year>=year_min & Year<=year_max)%>%
            left_join(fish%>%
                          filter(Taxa%in%local(species_filt())),
                      by="SampleID")

        if(input$Aggregate){
            req(input$Standardlength, input$Forklength)
            standard_min<-min(input$Standardlength)
            standard_max<-max(input$Standardlength)
            fork_min<-min(input$Forklength)
            fork_max<-max(input$Forklength)
            out<-out%>%
                filter((Source!="Suisun" & Length>=standard_min & Length<=standard_max) | (Source=="Suisun" & Length>=fork_min & Length<=fork_max) | is.na(Length))%>%
                group_by(across(-c(Length, Count, Notes_catch)))%>%
                summarise(Count=sum(Count, na.rm=T), .groups="drop")

        }
        return(out)

    })

    rows<-reactive({
        req(data_filt)
        data_filt()%>%
            select(SampleID)%>%
            collect()%>%
            nrow
    })

    data_ag<-reactive({
        req(isTRUE(input$Aggregate), input$Standardlength, input$Forklength)

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
