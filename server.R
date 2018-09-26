library(tidyr)

shinyServer(function(input, output, session) {
  
  stats <- sqldf("select type as description, count(*) as value from df group by type")
  users <- sqldf("select 'users' as description, count(distinct(user_id)) as value from df")
  stats <- rbind(stats, users)
  stats <- stats[order(-stats$value),] 
  df_top_tags <- df %>%
    select(time, tag) %>%
    unnest_tokens(tag, tag, token=stringr::str_split, pattern=fixed(" ")) %>%
    group_by(tag) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    head(52)
  
  df_tags <- df %>% 
    mutate(tag = strsplit(as.character(tag), " ")) %>% 
    unnest(tag)
  df_tags$month <- as.numeric(df_tags$month)
  
  
  output$totalQuestions <- renderInfoBox({
    infoBox(
      "# of questions", stats$value[2], icon = icon("question-sign", lib = "glyphicon"), fill = T,
      color = "red"
    )
  })
  output$totalAnswers <- renderInfoBox({
    infoBox(
      "# of Answers", stats$value[3], icon = icon("ok", lib = "glyphicon"), fill = T,
      color = "yellow"
    )
  })
  output$acceptedAnswers <- renderInfoBox({
    infoBox(
      "# Accepted Answers", stats$value[4], icon = icon("thumbs-up", lib = "glyphicon"), fill = T,
      color = "green"
    )
  })
  output$totalUsers <- renderInfoBox({
    infoBox(
      "# of Users", stats$value[1], icon = icon("user", lib = "glyphicon"), fill = T,
      color = "blue"
    )
  })
  
  output$majorTagName <- renderInfoBox({
    infoBox(
      "Maximum Used Tag Name", toString(df_top_tags$tag[2]), icon = icon("tag", lib = "glyphicon"), fill = T,
      color = "purple"
    )
  })
  
  output$majorTagCount <- renderInfoBox({
    infoBox(
      "# of times maximum used tag", df_top_tags$count[2], icon = icon("stats", lib = "glyphicon"), fill = T,
      color = "teal"
    )
  })
  
  output$funnelChart <- renderAmCharts({
    amFunnel(data = stats, neck_height = 30, neck_width = 40) #%>% 
      #scale_color_manual(values = c("blue", "red", "yellow", "green"))
  })
  
  output$tagBreakdownChart <- renderPlotly({
    df_tags <- sqldf("select frequency as frequency, count(*) as total from df group by frequency")
    
    if (is.data.frame(df_tags) && nrow(df_tags) > 0) {
      
      
      p <- ggplot(data=df_tags, aes(x=frequency, y=total*100/stats$value[2])) +
        geom_bar(colour="black", stat="identity") + 
        #scale_x_continous(breaks=pretty_breaks(n=12)) +
        labs(title='Breakdown of # of Tags in Stack Overflow Questions',
             x='# of Tags in Question',
             y='% Count of Stack Overflow Questions') +
        theme(
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgrey"),
          plot.margin = unit(c(0,0,1,1), "cm")
        )
      ggplotly(p) %>% config(displayModeBar = F)
    }else {
      ggplotly(ggplot() + geom_blank()) %>% config(displayModeBar = F)
    }
  })
  
  output$mostActiveUsers <- renderPlotly({

    mostActiveUsers <- sqldf("select count(type) as Activity_Count, user_id from df 
                              group by user_id 
                              order by Activity_Count desc limit 50")
    mostActiveUsers['username'] <- as.character(mostActiveUsers$user_id)
    
    if (is.data.frame(mostActiveUsers) && nrow(mostActiveUsers) > 0) {
      p <- ggplot() + 
        geom_point(data=mostActiveUsers, aes(x=user_id, y=Activity_Count, color = Activity_Count), 
                   size=mostActiveUsers$Activity_Count/15) +
        labs(title='Top 50 Users Based On Activity',
           y='Sum of Activity Count',
           x='Users') +
        theme(
          panel.background = element_rect(fill = "white"),
          #axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_line(colour = "lightgrey"),
          plot.margin = unit(c(0,0,1,1), "cm"),
          panel.grid.major.x = element_blank(),
          axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1)
        )
      ggplotly(p) %>% config(displayModeBar = T)
    }else {
      ggplotly(ggplot() + geom_blank()) %>% config(displayModeBar = F)
    }
  })
  
  selectedUserId <- reactive({
    s <- event_data("plotly_click")
    if (length(s) != 0) {
      activityBasedUsersData <- sqldf(sprintf("select user_id as UserId, type as 'Activity', count(type) as Total
                                              from df where user_id = %s 
                                              group by type", s$x))
    }
    activityBasedUsersData
  })
  
  output$userActivityInfo <- renderDataTable({
    df1 <- selectedUserId()
    if (is.data.frame(df1) && nrow(df1) > 0) {
      datatable(df1, selection = c("single"),
                options = list(pageLength = 10, scrollX=TRUE, scrollCollapse=TRUE),
                extensions = 'Responsive'
      )
    }else {
      datatable(data.frame(c()))
    }
       
    #paste0("x=", input$mostActiveUsers_click$x, "\ny=", input$mostActiveUsers_click$y)
  })

  selectedUserIdDetails <- reactive({
    index <- input$userActivityInfo_row_last_clicked
    if (is.null(index)) {
      x <- data.frame(c())
      return(-1)
    } else {
      x <- selectedUserId()
      selectedId <- x[index,]$UserId
      selectedType <- x[index,]$Activity
    }
      activityBasedUsersDetails <- sqldf(sprintf("select title, text, time, vote, reputation
                                              from df where user_id = %s and type = '%s'", selectedId, selectedType))
      activityBasedUsersDetails
  })
  
  output$userActivityDetails <- renderDataTable({
    df1 <- selectedUserIdDetails()
    #if (is.data.frame(df1) && nrow(df1) > 0) {
      datatable(df1, selection = c("none"),
                options = list(pageLength = 10, scrollX=TRUE, scrollCollapse=TRUE),
                extensions = 'Responsive'
      )
    #}else {
    #  datatable(data.frame(c()))
    #}
  })
  
  output$monthlyTags <- renderPlotly({
    
    data_t <- sqldf(paste("select month, tag, count(*) as Total_Usage from df_tags 
                     where month = ", input$tagSliderDuration, 
                     " and tag in ('javascript', 'eclipse', 'swing', 'android', 'spring', 'multithreading', 'hibernate') 
                     group by tag order by Total_Usage desc limit 50", sep=""))
    #data_t$Total_Usage <- with(data_t, (Total_Usage - min(Total_Usage)) / (max(Total_Usage) - min(Total_Usage)))*10
    if (is.data.frame(data_t) && nrow(data_t) > 0) {
      p <- ggplot() + 
        geom_point(data=data_t, aes(x=tag, y=Total_Usage, color = tag, size = Total_Usage)) +
        labs(title='Tags Distribution',
             y='Sum of Tag Count',
             x='Tags') +
        theme(
          panel.background = element_rect(fill = "white"),
          #axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_line(colour = "lightgrey"),
          plot.margin = unit(c(0,0,1,1), "cm"),
          panel.grid.major.x = element_blank(),
          axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1)
        )
      ggplotly(p) %>% config(displayModeBar = F)
    }else {
      ggplotly(ggplot() + geom_blank()) %>% config(displayModeBar = F)
    }
  })
  
  
 
  
  
  
  
  
})