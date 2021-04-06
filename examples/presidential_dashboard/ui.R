library(shiny)

fluidPage(
  title = "Cheatham Voting Precincts",

  fluidRow(
 
    column(4, align = "center",

      plotOutput('plot', width = "400", height = "400"),

      # Sidebar with a slider input for number of observations
      sliderInput("year",
                  "Election Year:",
                   min = 1998,
                   max = 2020,
                   step = 2,
                   value = 2020,
                   animate = animationOptions(loop = TRUE),
                   sep = ""),

    ),

    column(8, #style='padding-top: 20px;'
      h3(textOutput("table_title")),
      tableOutput("table"),

      h3("Candidates:"),
      tableOutput("candidates"),
    )
  )
)
