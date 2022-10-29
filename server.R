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
library(ggthemes)


qualtrics_api_credentials(Sys.getenv(c("QUALTRICS_API_KEY")), Sys.getenv(c("QUALTRICS_BASE_URL")))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  tcrs <- reactive({
    cat(input$forceReload)
    fetch_survey(
      surveyID = Sys.getenv(c("SURVEY_ID")), # Get from logging into qualtrics and clicking on "Qualtrics IDs" at the top
      verbose = TRUE,
      force_request = ifelse(input$forceReload > 0, TRUE, FALSE)
    ) %>% filter(!is.na(EndDate))
  })
  colMap <- reactive({
    extract_colmap(tcrs())
  })

  questions <- reactive({
    survey_questions(Sys.getenv(c("SURVEY_ID")))
  })
  roster <- reactive({
    fetch_mailinglist(Sys.getenv(c("MAILING_LIST_ID"))) # also from "qualtics IDs" page
  })

  section <- reactive({
    filter(roster(), "-" %in% input$filterBySection | section %in% input$filterBySection)
  })

  groupResults <- reactive({
    roster() %>%
      left_join(tcrs(), by = c("email" = "RecipientEmail")) %>%
      filter(
        (("-" %in% input$filterByGroup & (section %in% input$filterBySection | "-" %in% input$filterBySection)) 
        | group %in% input$filterByGroup))
  })

  weekResults <- reactive({
      groupResults() %>%
      filter(Q27 %in% input$filterByDate)
  })

  observeEvent(input$section, {
    updateSelectInput(inputid = "section", choices = roster() %>% filter())
  })

  output$howDidYouDo <- renderPlot({
    # just the "how did you do" questions
    qs <- c("Q4", "Q5", "Q7")
    resp <- weekResults() %>%
      select(fullname, any_of(qs)) %>%
      pivot_longer(-fullname)
    # resp
    respG <- resp %>%
      filter(!is.na(value)) %>%
      mutate(value=fct_rev(value)) %>%
      group_by(name, value) %>%
      summarise(count = n()) %>%
      inner_join(colMap(), by = c("name" = "qname")) %>%
    select(main, value, count) %>%
    mutate(main = str_wrap(main, 15)) %>%
    pivot_wider(names_from=value,values_from=count, values_fill = 0, names_expand = TRUE)

  HH::likert(main~., respG, ReferenceZero=3, main="", ylab="") 
  })

  output$sentiment <- renderPlot({
    sentimentQs <- c("Q3", "Q6", "Q16", "Q21", "Q23", "Q24")

    sentiment <- weekResults() %>%
      select(fullname, any_of(sentimentQs)) %>%
      pivot_longer(-fullname) %>%
      filter(!is.na(value))

    sentimentG <- sentiment %>%
      mutate(value=fct_rev(value)) %>%
      group_by(name, value) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      inner_join(colMap(), by = c("name" = "qname")) %>%
      select(main, value, count) %>%
      mutate(main = str_wrap(main, 25)) %>%
      pivot_wider(names_from = value, values_from = count, values_fill = 0, names_expand = TRUE)

    HH::likert(main ~ ., sentimentG, ReferenceZero = 3, main = "", ylab = "")
  })
  output$sentimentOverTime <- renderPlot({
    sentimentQs <- c('Q3', 'Q6', 'Q16', 'Q21', 'Q23', 'Q24','Q27')
    sentiment <- groupResults()  %>% 
    select(fullname, any_of(sentimentQs)) %>%
    pivot_longer(-any_of(c('fullname','Q27'))) %>% filter(!is.na(value)) %>%
        mutate(value=fct_rev(value)) 
       
    sentimentG <- sentiment %>%
      group_by(name,value,Q27) %>%
      summarise(count=n()) %>%
      ungroup() %>%
      rename(Week=Q27) %>%
      inner_join(colMap(), by=c("name"="qname")) %>%
      select(main, value, count, Week) %>%
      mutate(main=str_wrap(main,70), value=as.numeric(value)) %>%
      group_by(main, Week) %>%
      summarise(avg=sum(value*count)/sum(count), min=min(value),max=max(value))
    

    ggplot(sentimentG, aes(x=Week,group=1)) +
        geom_ribbon(aes(ymin=min,ymax=max), fill="grey70") +
        geom_line(aes(y=avg)) +
        scale_y_discrete(name="Response",
            labels=levels(sentiment$value),
            limits=levels(sentiment$value)
            ) +
         theme_minimal() +
            facet_wrap(main ~ ., ncol=2,scales="free_y")+
            theme(
            axis.title.y = element_blank(),    
            axis.title.x = element_blank(), 
                    axis.text.y=element_text(color="black"),

            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border=element_rect(fill=NA,color='black', size=0.5),
            panel.grid.major.y=element_line(color="grey80", size=0.25),
            )

  })

  output$thisWeekIHave <- renderUI({
    # The "This week I have questions"
    q <- c("QID1", "QID2", "QID9", "QID10", "QID9_7_TEXT", "Q1_7_TEXT", "Q2_6_TEXT", "Q10_5_TEXT")
    qs <- colMap() %>%
      filter(ImportId %in% q) %>%
      select(qname)
    thisWeekI <- weekResults() %>%
      select(fullname, any_of(qs$qname)) %>%
      pivot_longer(-fullname) %>%
      inner_join(colMap(), by = c("name" = "qname")) %>%
      filter(!is.na(value) & value != "Other") %>%
      group_by(main, fullname) %>%
      summarise(value = paste0(value, collapse = ", ")) %>% # group together by respondent
      ungroup() %>%
      mutate(value = paste0("<li>", fullname, ": ", value, "</li>")) %>% # each respondent summarised with name then list
      group_by(main) %>%
      summarise(value = paste0(value, collapse = "\n")) %>%
      ungroup() %>%
      mutate(summary = paste0("<h4>", main, "</h4><ul>", value, "</ul>")) %>%
      select(summary) %>%
      summarise(summary = paste0(summary, collapse = "\n"))
    HTML(thisWeekI$summary)
  })
  output$reflection <- renderTable({
    weekResults() %>% select(fullname, Q15)
  })
  output$completionInfo <- renderTable({
    weekResults() %>%
      mutate(minutes = `Duration (in seconds)` / 60, EndDate = format(EndDate)) %>%
      select(fullname, EndDate, minutes)
  })
  output$filterBySection <- renderUI({
    selectInput("filterBySection", "Section:",
      choices = c(section = "-") %>% bind_rows(roster()) %>% distinct(section) %>% select(section)
    )
  })
  output$filterByDate <- renderUI({
    selectInput("filterByDate", "Reflection Period:",
      choices = tcrs() %>% distinct(Q27) %>% select(Q27)
    )
  })
  output$filterByTeam <- renderUI({
    selectInput("filterByGroup", "Group:",
      choices = c(group = "-") %>% bind_rows(section()) %>% distinct(group) %>% select(group) %>% arrange(group)
    )
  })
})
