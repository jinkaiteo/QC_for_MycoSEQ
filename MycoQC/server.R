#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(knitr)

# Define server logic required to draw a histogram
function(input, output, session) {
    
    output$checkrender <- renderText({
        if (identical(rmarkdown::metadata$runtime, "shiny")) {
            paste("TRUE", rmarkdown::metadata$runtime)
        } else {
            paste("FALSE", rmarkdown::metadata$runtime)
        }
    })
    
    output$checkrenderprint <- renderPrint(rmarkdown::metadata$runtime)

    output$report <- downloadHandler(
        filename = function() {
            paste0("report_", Sys.Date(), ".html")
        },
        content = function(file) {
            
            here::here()
            withProgress(
                message = 'Rendering, please wait!', {
                    rmarkdown::render(here::here("Lesson_05.Rmd"),
                                      output_file = file, 
                                      params = list(
                                          rendered_by_shiny = TRUE,
                                          source_file = input$fileInput$datapath[1],
                                          author = input$author
                                      ),
                                      envir = new.env(),
                                      intermediates_dir = tempdir())
                }
            )
            
          
            
        }
    )

}
