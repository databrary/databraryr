#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Load preliminary data

databraryapi::login_db("rogilmore")
default_video_url <- "https://nyu.databrary.org/slot/9807/-/asset/1/download?inline=true"
vol.id <- 1
video_list <- databraryapi::list_assets_by_type(vol.id = vol.id, type = 'video')
dv_list <- databraryapi::list_assets_by_type(vol.id = vol.id, type = 'datavyu')

# dv_df <- databraryapi::list_assets_by_type(vol.id = vol.id, type = "datavyu")
# if (is.null(dv_df)) {
#   message(paste0("No files of type ", type, "in volume ", vol.id))
# } else {
#   dv_df
# }

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("VideoClipR"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
     selectInput(inputId = "Volume", label = strong("Volume ID"),
                 choices = c("1", "4"),
                 selected = "1"),
      # Show a plot of the generated distribution
      mainPanel(
        uiOutput("vid")
        # https://www.youtube.com/watch?v=6F5_jbBmeJU for how to embed video
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$vid <- renderUI({
    h6("Full video", br(), tags$video(src=default_video_url, type="video/mp4", width="350px",
                                      height="350px", controls="controls"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

