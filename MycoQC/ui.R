library(shiny)

fluidPage(

    # Application title
    titlePanel("MycoSEQ QC Checks"),

    # File selection block
    sidebarLayout(
        sidebarPanel(
            shiny::fileInput(inputId = "fileInput", label = "Select file...", multiple = FALSE, accept = ".xls", ),
            shiny::textInput(inputId = "author", label = "Author", value = "NA", placeholder = "Your Name"),
            shiny::actionButton(inputId = "generateReport", label = "Generate Report"),
            shiny::downloadButton(outputId = "report", label = "Generate Report")
        ),

        mainPanel(
            htmlOutput("markdown")
        )
    )
)
