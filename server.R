#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(qualtRics)
library(ggplot2)
library(lubridate)


qualtrics_api_credentials(Sys.getenv(c("QUALTRICS_API_KEY")), Sys.getenv(c("QUALTRICS_BASE_URL")))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    tcrs <- reactive({
        fetch_survey(
            surveyID = Sys.getenv(c("SURVEY_ID")), # Get from logging into qualtrics and clicking on "Qualtrics IDs" at the top
            verbose = TRUE,
            force_request = FALSE
        )
    })
    colMap <- reactive({
      extract_colmap(tcrs())
    })

    questions <- reactive({
        survey_questions(Sys.getenv(c("SURVEY_ID")))
    })
    roster <- reactive({
        fetch_mailinglist(Sys.getenv(c("MAILING_LIST_ID")))  #also from "qualtics IDs" page
    })
    
    section <- reactive({
      filter(roster(), '-' %in% input$filterBySection | section %in% input$filterBySection)
    })
    
    groupResults <- reactive({
      roster() %>%
        left_join(tcrs(), by=c('email'='RecipientEmail')) %>%
        filter(('-' %in% input$filterByGroup & (section %in% input$filterBySection | '-' %in% input$filterBySection)) | group %in% input$filterByGroup)
    })
    
    observeEvent(input$section, {
      updateSelectInput(inputid = "section", choices = roster() %>% filter() )
    })

    output$howDidYouDo <- renderPlot({
      # just the "how did you do" questions
      qs <- c('Q4', 'Q5', 'Q7')
      resp <- groupResults() %>% select(fullname, any_of(qs))  %>% pivot_longer(-fullname)
      # resp
      respG <- resp %>% filter(!is.na(value)) %>% group_by(name,value) %>% summarise(count=n()) %>% inner_join(colMap(), by=c("name"="qname"))
      
      ggplot(respG) + 
        geom_bar(aes(y=str_wrap(main, 25),x=count, fill=value), position = "stack", stat='identity') +  scale_fill_brewer(palette="PRGn")+
        theme(legend.position="bottom", axis.title.x=element_blank(), axis.title.y=element_blank())
      
    })
    
    output$sentiment <- renderPlot({
      sentimentQs <- c('Q3', 'Q6', 'Q16', 'Q21', 'Q23', 'Q24')
      sentiment <- groupResults()  %>% select(fullname, any_of(sentimentQs)) %>% pivot_longer(-fullname) %>% filter(!is.na(value)) 
      sentimentG <- sentiment %>% group_by(name,value) %>% summarise(count=n()) %>% inner_join(colMap(), by=c("name"="qname"))
      
      ggplot(sentimentG) + 
        geom_bar(aes(y=str_wrap(main, 25),x=count, fill=value), position = "stack", stat='identity') +  #scale_fill_brewer(palette="PRGn")+
        theme(legend.position="bottom", axis.title.x=element_blank(), axis.title.y=element_blank())
    })
    output$thisWeekIHave <- renderUI({
      # The "This week I have questions"
      q <- c("QID1", "QID2", "QID9", "QID10","QID9_7_TEXT","Q1_7_TEXT","Q2_6_TEXT","Q10_5_TEXT")
      qs <- colMap() %>%
        filter(ImportId %in% q) %>%
        select(qname)
      thisWeekI <- groupResults() %>%
        select(fullname, any_of(qs$qname)) %>%
        pivot_longer(-fullname) %>%
        inner_join(colMap(), by=c("name"="qname")) %>%
        filter(!is.na(value) & value != "Other") %>%
        group_by(main, fullname) %>%
        summarise(value= paste0(value, collapse=", ")) %>% # group together by respondent
        ungroup()%>%
        mutate(value=paste0("<li>",fullname,": ",value,"</li>")) %>% # each respondent summarised with name then list
        group_by(main) %>% 
        summarise(value=paste0(value,collapse="\n")) %>% ungroup() %>%
        mutate(summary=paste0("<h4>",main,"</h4><ul>",value,"</ul>")) %>%
        select(summary) %>% summarise(summary=paste0(summary,collapse="\n"))
      HTML(thisWeekI$summary)
    })
    output$completionInfo <- renderTable({
      groupResults() %>% mutate(minutes=`Duration (in seconds)`/60, EndDate=format(EndDate)) %>% select(fullname, EndDate, minutes) 
    })
    output$filterBySection <- renderUI({
        selectInput("filterBySection", "Section:",
            choices = c(section="-") %>% bind_rows(roster()) %>% distinct(section) %>% select(section)
        )
    })
    output$filterByDate <- renderUI({
      selectInput("filterByDate", "Reflection Period:",
                  choices = groupResults() %>% distinct(Q27) %>% select(Q27)
      )
    })
    output$filterByTeam <- renderUI({
        selectInput("filterByGroup", "Group:",
            choices = c(group="-") %>% bind_rows(section()) %>% distinct(group) %>% select(group) %>% arrange(group)
        )
    })
})
