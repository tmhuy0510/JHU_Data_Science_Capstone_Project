library(shiny)

shinyUI(fluidPage(
    titlePanel("Predictive Text Model"),
    p('Huy (Henry) Truong', br(), '2022-02-28'),
    hr(),
    p("This text predicting application is inspired by the texting application 
      on smartphone keyboards, which is the texting tool that is capable of 
      predicting the next word as users are texting on their smartphone."),
    sidebarLayout(
        sidebarPanel(
            h3(strong("Instruction")),
            br(),
            p("Feel free to enter your text in the text input area. 
              A list of at most 3 recommended words which are highly likely 
              to follow next will be automatically updated and shown below."),
            br(),
            p("There is no limit on the length of the text but make sure 
              the text is entered in ",
              strong("English.")),
            br(),
            p(strong("Note: "),
              "If your last entered word is misspelled, an additional option 
              of ",
              code("MISSPELLED WORD?"),
              " will be seen in the list.")
        ),
        mainPanel(
            textAreaInput("text", "My Text", 
                          width = "100%", height = "100px",
                          placeholder = "Enter your text"),
            fluidRow(column(1, tableOutput("table"), offset = 4))
        )
    )
))
