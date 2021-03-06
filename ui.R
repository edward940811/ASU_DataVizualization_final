


############# Dashboard UI ##############################################################################################################################

dashboardPage(
  title = "CSE 578 - Stack Overflow Analysis",
  skin = "blue",
  dashboardHeader(title = "Stack Overflow"),
  dashboardSidebar(
    sidebarMenu(
      id = "sbMenu",
      menuItem("Home",tabName = "dashboardHome", icon = icon("home")),
      menuItem("Summary",tabName = "summary", icon = icon("table")),
      menuItem("Users Statistics",tabName = "userActivityBased", icon = icon("user")),
      menuItem("Tags Statistics",tabName = "tags", icon = icon("tags"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "dashboardHome",
        fluidPage(
          fluidRow(
            box(
              title = "Welcome", width = 12, status = "primary", solidHeader = T, collapsible = T, collapsed = F,
              includeMarkdown("frontPage.html")
            )
          )
        )
      ),
      tabItem(
        "summary",
        fluidPage(
          fluidRow(
            id = "tableSummaryRow",
            box(
              title = "Overall Statistics", width = 12, status = "primary", solidHeader = T, collapsible = T, collapsed = F,
              infoBoxOutput("totalUsers", width = 6),
              infoBoxOutput("totalQuestions", width = 6),
              infoBoxOutput("totalAnswers", width = 6),
              infoBoxOutput("acceptedAnswers", width = 6),
              infoBoxOutput("majorTagName", width = 6),
              infoBoxOutput("majorTagCount", width = 6)
            )
          ),
          fluidRow(
            box(
              title = "Statistic Distribution", width = 6, status = "primary", solidHeader = T, collapsible = T, collapsed = F,
              amChartsOutput(outputId = "funnelChart")
            )
          )
        )
      ),
      tabItem(
        "userActivityBased",
        fluidPage(
          fluidRow(
            box(
              title = "Active Users Statistics", width = 12, status = "primary", solidHeader = T, collapsible = T, collapsed = F,
              #column(6, selectizeInput(inputId = "selectedDB", choices = choicesDB, label = "Select a database:", multiple = F, selected = NULL, width = "100%")),
              column(12, plotlyOutput("mostActiveUsers"))
            )
          ),
          fluidRow(
            box(
              title = "User Activity Info", width = 12, status = "primary", solidHeader = T, collapsible = T, collapsed = T,
              column(12, dataTableOutput("userActivityInfo"))
            ),
            box(
              title = "User Activity Detail", width = 12, status = "primary", solidHeader = T, collapsible = T, collapsed = T,
              column(12, dataTableOutput("userActivityDetails"))
            )
          )
        )
      ),
      tabItem(
        "tags",
        fluidPage(
          fluidRow(
            id = "tagBreakdown",
            box(
              title = "Tags", width = 12, status = "primary", solidHeader = T, collapsible = T, collapsed = F,
              column(12, plotlyOutput("tagBreakdownChart"))
            )
          ),
          fluidRow(
            id = "userTagsBased",
            box(
              title = "Active Users Statistics", width = 12, status = "primary", solidHeader = T, collapsible = T, collapsed = F,
              verbatimTextOutput("check"),
              column(12, sliderInput("tagSliderDuration", "Months:", min = 01, max = 12, value = 01, step = 01)),
              column(12, plotlyOutput("monthlyTags"))
            )
          )
        )
      )
    )
  )
)