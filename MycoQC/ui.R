library(shiny)

fluidPage(

    # Application title
    titlePanel("MycoSEQ QC Checks"),

    # File selection block
    sidebarLayout(
        sidebarPanel(
            shiny::fileInput(inputId = "fileInput", label = "Select file...", multiple = FALSE, accept = ".xls"),
            shiny::textInput(inputId = "author", label = "Author", value = "NA", placeholder = "Your Name"),
            shiny::downloadButton(outputId = "report", label = "Generate Report")
        ),

        mainPanel(
            textOutput("checkrender"),
            verbatimTextOutput("checkrenderprint")
        )
    )
)
