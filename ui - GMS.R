library(shiny)
library(shinydashboard)

dbHeader <- dashboardHeader(
  title = span("GMS DataPrep", style = "color: white; font-size: 28px; font-weight: bold;"),
  tags$li(class = "dropdown",
          tags$style(HTML('
            .main-header .navbar-custom-title {
              position: absolute;
              left: 70px;
              top: 10px;
              font-size: 24px;
              color: white;
              font-weight: bold;
            }
          ')),
  )
)

dashboardPage(
  dbHeader,
  
  dashboardSidebar(
    tags$a(img(id = "logosidebar", src = 'GenusABS.png', height = 100, width = 2.2 * 100),
           href = "javascript:window.location.reload();"),
    sidebarMenu(
      textInput("GENEadvance", "Enter GENEadvance Number", placeholder = 0123456789),
      selectInput(inputId = "Type", label = "Active or Historical Animals", choices = c('Active','Historical',"Both")),
      
      # New Column View Toggle:
      radioButtons(
        inputId = "DataView",
        label = "Select Columns to Display:",
        choices = c("Full Data", "GMS Columns Only"),
        selected = "Full Data"
      ),
      
      actionButton("LoadData", "Load"),
      downloadButton("DownloadData", "Download Data", class = "btn-loadstyle")
    )
  ),
  
  dashboardBody(width = 500,
                tags$head(
                  tags$style(HTML('
        .main-sidebar {
          background-color: rgba(206, 17, 65)!important;
          margin-top: 20px; 
          box-shadow: 4px 0px 19px 0px grey;
          overflow-y: auto;
          text-align: center;
        }
        .skin-blue .main-header .logo {
          background-color: #003976;
          padding: 0px;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #003976;
          opacity: 0.9;
        }
        .sidebar-toggle {
          float: right !important;
        }
        .sidebar-toggle:hover {
          opacity:0.5;
        }
        .skin-blue .main-header .navbar {
          background-color: #003976;
        }
        .skin-blue .main-header .navbar .title {
          margin: auto;
        }
        .box.box-solid.box-primary>.box-header {
          background-color: rgba(206, 17, 65, 1)!important;
        }
        .box.box-solid.box-primary {
          border-color: #CE1141;
        }
        #plotrow {
          height: 90vh;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        #logosidebar {
          margin-bottom: 30px;
        }
        #LoadData {
          margin-top: 30px;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-left: auto;
          margin-right: auto;
          margin-bottom: 20px;
        }
        /* Style DownloadData button same as LoadData */
        #DownloadData {
          background-color: rgba(206, 17, 65) !important;
          color: white !important;
          border-color: rgba(206, 17, 65) !important;
          width: 150px;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-left: auto;
          margin-right: auto;
          margin-top: 20px;
          font-weight: bold;
          cursor: pointer;
        }
        #DownloadData:hover {
          background-color: #b5103f !important;
          border-color: #b5103f !important;
          color: white !important;
        }
        #DT {
          display: flex;
          height: 75vh;
          overflow-y: auto;
          font-size: 12px;
          spacing: xs;
          box-shadow: -3px 0px 19px 2px grey;
        }
        #buttons {
          display: flex;
          align-items: center;
          justify-content: center;
          margin-right: 5px;
        }
        @media screen and (min-width: 750px) {
          body {overflow-y: hidden;}
        }
        .content-wrapper {
          background-color: #fff;
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #003976;
        }
      '))
                ),
                
                fluidRow(
                  id = "plotrow",
                  shinydashboard::box(
                    id = "DT",
                    title = "Data Table",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 11,
                    div(style = "height:75vh; overflow-y:auto;",
                        DT::dataTableOutput("Bar")
                    )
                  )
                )
                
  )
)