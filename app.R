library(shiny) 
library(DT)
library(leaflet)
library(rgdal)
#renv::install("rgdal", repos="http://R-Forge.R-project.org")
#install_github("hrue/r-inla")
#renv::install("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
library(dplyr)
library(mapview)
library(webshot)
library(htmlwidgets)
library(shiny)
library(promises)
library(future)
library(spdep)
library(fastDummies)
library(readr)
library(maptools)
library(Matrix)
library(INLA)
library(future)
plan(multisession, future.seed = 999)

source("./Model/INLA_Mmodel_icar.R")
source("./Model/INLA_Mmodel_lcar.R")
source("./Model/INLA_Mmodel_pcar.R")
source("./Model/MCAR_INLA_st.R")

# Increase memory limit
#memory.size(max = FALSE)
options(shiny.maxRequestSize = 30*1024^2)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .navbar.navbar-static-top {
      background-color: #279DFF;
      font-weight: bold;
      color: white !important;
      position: fixed; /* ทำให้ Navbar ติดอยู่ด้านบน */
      top: 0; /* ระบุตำแหน่งด้านบนสุด */
      width: 100%; /* กว้างเท่ากับหน้าจอ */
    }
    /* Adjust the color of the navbar text */
      .navbar-default .navbar-nav > li > a {
        color: white !important;
      }
    .nav-default .navbar-nav .nav-item a {
      color: white !important; /* ทำให้ตัวอักษรสีขาวทุก tab */
    }
    .nav.navbar-nav .active a {
      background-color: #3366FF !important;
      font-weight: bold;
      color: white !important;
    }
    .navbar .navbar-nav {
      margin-bottom: 0; /* ปรับระยะห่างของ tabbar ด้านล่างให้เป็น 0 */
    }

    .tab-content {
      padding-top: 50px; /* ปรับระยะห่างด้านบนของเนื้อหาใน tab เพื่อไม่ให้ tabbar ทับเนื้อหา */
    }
    .center-content {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
    }
   #go_to_upload_data {
        background-color: #3366FF;
        font-weight: bold;
        font-size: 18px;
        color: white;
        padding: 10px 50px;
        border-radius: 10px;
   }

    #go_to_Manual {
        background-color: #B8B6B6;
        font-weight: bold;
        font-size: 18px;
        color: white;
        padding: 10px 50px; /* 15px คือ ส่วนด้านบนและด้านล่าง, 30px คือ ส่วนด้านซ้ายและด้านขวา */
        border-radius: 10px; /* กำหนดเส้นขอบปุ่มโค้ง*/
    }
    #go_to_Map {
        background-color: #3366FF;
        font-weight: bold;
        font-size: 16px;
        color: white;
        padding: 8px 50px; /* 10px คือ ส่วนด้านบนและด้านล่าง, 30px คือ ส่วนด้านซ้ายและด้านขวา */
        border-radius: 30px; /* กำหนดเส้นขอบปุ่มโค้ง */
    }
    #Preview_data {
        background-color: #279DFF;
        font-weight: bold;
        font-size: 16px;
        color: white;
        padding: 8px 50px; /* 10px คือ ส่วนด้านบนและด้านล่าง, 30px คือ ส่วนด้านซ้ายและด้านขวา */
        border-radius: 30px; /* กำหนดเส้นขอบปุ่มโค
    }
    .action-button {
        margin-right: 65px; /* กำหนดระยะห่างระหว่างปุ่ม */
    }
    .sidebar-panel {
        border: 2px solid #279DFF !important; /* กำหนดขอบสีฟ้าของ sidebar */
        background-color: white !important; /* กำหนดพื้นหลังสีขาวของ sidebar */
    }
    .custom-help {
      margin-bottom: 1px;  /* ลดระยะห่างด้านล่างของ helpText */
    }
    .custom-note {
      margin-top: 1px;   
    }
    .custom-text {
      font-weight: bold; /* ความหนาของตัวอักษร */
    }

    .custom-file-input {
      margin-top: 1px;  /* ลดระยะห่างด้านบนของ fileInput */
    }
    .dataTableWrapper {
      box-sizing: border-box;
      overflow-x: auto;
      max-width: 100%;
    }
    .dataTable {
      width: 100% !important;
      box-sizing: border-box;
    }
    .custom-tooltip {
      display: block;
      position: relative;
    }
    .fa-circle-exclamation {
      position: absolute;
      top: 5px;
      right: 15px;
      color:#279DFF;
    }
    .icon-spacing {
          margin-right: 5px; /* เพิ่มระยะห่างทางด้านขวา 5px */
    }
    .shiny-tab-panel .leaflet-container {
    margin-top: 0 !important;
    padding-top: 0 !important;
  }
  .shiny-tab-panel h3 {
    margin-top: 0 !important;
    padding-top: 0 !important;
  }    
  ")),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css"
    ),
    
  ),
  navbarPage(
    id = "navbar",
    title = div(
      "MSTE Analyse",
      style = "color: white"),
    tabPanel("Home",icon = icon("house",style="color:#ffffff"),
             fluidRow(
               column(width = 4, # ส่วนซ้ายของหน้าจอ
                      div(
                        class = "center-content",
                        img(src = "png home.png", width = "400px", height = "400px")
                      )
               ),
               column(width = 8, # ส่วนขวาของหน้าจอ
                      style = "margin-top: 250px;",
                      h1("Multivariate Spatiotemporal Epidemiological Analysis"),
                      HTML("<br>"), # สร้างระยะห่าง 50px
                      h3("This is an application for analyzing space-time patterns and associations,",
                         HTML("<br>"), "with risk factor of depression, suicide, and other health outcomes, ", HTML("<br>"), "which allows users to import their own data, analyze, and visualize."),
                      HTML("<br>"), # เพิ่มระยะห่าง 50px
                      actionButton("go_to_upload_data", "Get Started", class = "action-button"),
                      tags$style(HTML(".action-button { margin-right: 50px; }")),
                      actionButton("go_to_Manual", "How to use ?", class = "action-button")
               )
             )
    ),
    tabPanel("Upload Data", value = "upload_data",#tags$style(HTML("body { background-color: #f2f2f2; }")),
             #h2("Upload Data",style="color:#279DFF; font-weight: semibold; margin-left: 0px;"),
             column(width = 4, h2("Input Data",style="color: #279DFF; margin-left: -15px;")),
             column(width = 8, h2("Preview Input Data",style="color: #279DFF; margin-left: 0px;")),
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar-panel",
                 #tags$h3("Upload Shapefile",icon("arrow-up-from-bracket",style = "color: #279DFF; margin-left:10px;")),
                 tags$div( # กลุ่มองค์ประกอบเข้าด้วยกัน
                   tags$span("1.", 
                             style = "font-size: 1.75em; 
                   color: #279DFF; 
                   background-color: #D7F0FF; 
                   padding: 20px; 
                   border-radius: 5px; 
                   margin-right: 10px; 
                   display: flex; /* ใช้ flexbox สำหรับจัดตำแหน่งข้อความ */
                   justify-content: center; /* จัดข้อความให้อยู่ตรงกลางแนวนอน */
                   align-items: center; /* จัดข้อความให้อยู่ตรงกลางแนวตั้ง */
                   height: 35px; /* กำหนดขนาดสูง หากจำเป็น */
                   width: 35px;"),
                   tags$h3("Upload Shapefile", style = "margin: 0; display: inline;", # ข้อความ "Upload Shapefile"
                           icon("arrow-up-from-bracket", style = "color: #279DFF; margin-left: 10px;")), # ไอคอน
                   style = "display: flex; align-items: center;" # จัดให้องค์ประกอบอยู่ในแนวเดียวกันและตรงกลาง
                 ),
                 tags$hr(
                   style = "border: 1.5px solid #D4D4D4; width: 100%; margin-top: 20px; margin-bottom: 20px;"
                 ),
                 helpText(
                   HTML("<span style='color: #279DFF; font-weight: bold;'>Upload all map files at once: </span>
                        <span style= 'color: #000000;font-weight: normal;'>shp, dbf, shx and prj.</span>"),
                   class = "custom-help"),
                 fileInput("shpfile","",accept = c(".shp", ".dbf", ".shx", ".prj"), multiple = TRUE),
                 h5("*Select columns area id in the map."),
                 #selectInput("column_areaID_shp", "Area id:", choices = NULL),
                 div(class = "custom-tooltip",
                     selectInput("column_areaID_shp", "Area id:", choices = NULL),
                     tags$abbr(title = "Area id is a number starting at 1, used to identify provinces.",
                               tags$i(class = "fa-solid fa-circle-exclamation"))
                 ),
                 helpText(
                   HTML("<span style='color: #279DFF; font-weight: bold;'>Note: </span>
                        <span style='color: #000000; font-weight: bold;'>Area id </span>
                        <span style= 'color: #000000;font-weight: normal;'>is name of the area. Area id in the data must be the same name and order as area id in the</span>
                        <span style='color: #000000; font-weight: bold;'>csv file. </span>"),
                   class = "custom-note"),
                   HTML("<br><br>"),
                 #p("Area name is name of the area. Area name in the data must be the same name and order as area name in the csv file."),
                 tags$hr(
                   style = "border: 1.5px solid #279DFF; width: 100%; margin: 0 auto;"
                 ),
                 HTML("<br>"),
                 #h3("Upload CSV File"),
                 #tags$h3("Upload CSV File", icon("arrow-up-from-bracket",style = "color: #279DFF; margin-left:10px;")),
                 tags$div( # กลุ่มองค์ประกอบเข้าด้วยกัน
                   tags$span("2.", 
                             style = "font-size: 1.75em; 
                   color: #279DFF; 
                   background-color: #D7F0FF; 
                   padding: 20px; 
                   border-radius: 5px; 
                   margin-right: 10px; 
                   display: flex; /* ใช้ flexbox สำหรับจัดตำแหน่งข้อความ */
                   justify-content: center; /* จัดข้อความให้อยู่ตรงกลางแนวนอน */
                   align-items: center; /* จัดข้อความให้อยู่ตรงกลางแนวตั้ง */
                   height: 35px; /* กำหนดขนาดสูง หากจำเป็น */
                   width: 35px;"),
                   tags$h3("Upload CSV File", style = "margin: 0; display: inline;", # ข้อความ "Upload Shapefile"
                           icon("arrow-up-from-bracket", style = "color: #279DFF; margin-left: 10px;")), # ไอคอน
                   style = "display: flex; align-items: center;" # จัดให้องค์ประกอบอยู่ในแนวเดียวกันและตรงกลาง
                 ),
                 tags$hr(
                   style = "border: 1.5px solid #D4D4D4; width: 100%; margin-top: 20px; margin-bottom: 20px;"
                 ),
                 h4("Select Data*", class = "custom-text"),
                 helpText(
                   HTML("<span style='color: #000000; font-weight: normal;'>*.csv file needs to have columns: </span>
                        <span style= 'color: #279DFF;font-weight:bold;'>area id, year, cases, population, disease level</span>"),
                   class = "custom-help"),
                 fileInput("csvfile", "", accept = ".csv"),
                 h5("*Please ensure that the column names you choose match the descriptions provided."),
                 #tags$div(
                   #class = "row",
                   #tags$div(
                     #class = "col-md-6",
                div(class = "custom-tooltip",
                    selectInput("column_areaID_csv", "Area id:", choices = NULL),
                    tags$abbr(title = "Area id is name of the area. Area id in the data must be the same as area name in the shapefile.",
                              tags$i(class = "fa-solid fa-circle-exclamation"))
                     ),
                     #selectInput("column_areaName", "Area name:", choices = NULL)
                   #),
                   #tags$div(
                     #class = "col-md-6",
                     #selectInput("column_areaID_csv", "Area id:", choices = NULL)
                   #)
                 #),
                 tags$div(
                   class = "row",
                   tags$div(
                     class = "col-md-6",
                     div(class = "custom-tooltip",
                         selectInput("column_population", "Population:", choices = NULL),
                         tags$abbr(title = "Population is population in data",
                                   tags$i(class = "fa-solid fa-circle-exclamation"))),
                     #selectInput("column_population", "Population:", choices = NULL)
                   ),
                   tags$div(
                     class = "col-md-6",
                     div(class = "custom-tooltip",
                         selectInput("column_cases", "Cases:", choices = NULL),
                         tags$abbr(title = "Case is number of case or outcome in each area.",
                                   tags$i(class = "fa-solid fa-circle-exclamation"))),
                     #selectInput("column_cases", "Cases:", choices = NULL)
                   )
                 ),
                 tags$div(
                   class = "row",
                   tags$div(
                     class = "col-md-6",
                     div(class = "custom-tooltip",
                         selectInput("column_year", "Year:", choices = NULL),
                         tags$abbr(title = "Year is time period in data",
                                   tags$i(class = "fa-solid fa-circle-exclamation"))),
                     #selectInput("column_year", "Year:", choices = NULL)
                   ),
                   tags$div(
                     class = "col-md-6",
                     div(class = "custom-tooltip",
                         selectInput("column_level", "Disease level:", choices = NULL),
                         tags$abbr(title = "Disease level is level of case or outcome",
                                   tags$i(class = "fa-solid fa-circle-exclamation"))),
                     #selectInput("column_level", "Disease level:", choices = NULL)
                   )
                 ),
                 h4("Select Covariate*", class = "custom-text"),
                 h5("*Select between 1 to 8 covariates.",style="color: #000000; font-weight: normal;"),
                 #h5("Select columns:"),
                 tags$div(
                   class = "row",
                   tags$div(
                     class = "col-md-6",
                     selectInput("covariate_1", "covariate 1", choices = NULL)
                   ),
                   tags$div(
                     class = "col-md-6",
                     selectInput("covariate_2", "covariate 2", choices = NULL)
                   )
                 ),
                 tags$div(
                   class = "row",
                   tags$div(
                     class = "col-md-6",
                     selectInput("covariate_3", "covariate 3", choices = NULL)
                   ),
                   tags$div(
                     class = "col-md-6",
                     selectInput("covariate_4", "covariate 4", choices = NULL)
                   )
                 ),
                 tags$div(
                   class = "row",
                   tags$div(
                     class = "col-md-6",
                     selectInput("covariate_5", "covariate 5", choices = NULL)
                   ),
                   tags$div(
                     class = "col-md-6",
                     selectInput("covariate_6", "covariate 6", choices = NULL)
                   )
                 ),
                 tags$div(
                   class = "row",
                   tags$div(
                     class = "col-md-6",
                     selectInput("covariate_7", "covariate 7", choices = NULL)
                   ),
                   tags$div(
                     class = "col-md-6",
                     selectInput("covariate_8", "covariate 8", choices = c("select"))
                   )
                 ),
                HTML("<br>"),
                tags$p("Note :", style = "color: #279DFF; font-weight: bold;"),
                  div(
                    style = "display: flex; align-items: center;",
                    tags$i(class = "fa-solid fa-circle", style = "font-size: 4px; margin-right: 5px;margin-left: 20px;"),
                    "Disease level column must have more than 1 level."),
                  #tags$p("must be numeric.", style = "margin-left: 30px;"),
                div(
                  style = "display: flex; align-items: center;",
                  tags$i(class = "fa-solid fa-circle", style = "font-size: 4px; margin-right: 5px;margin-left: 20px;"),
                  "The data in the Year, Cases, Level, and Population"),
                tags$p("columns must be numeric.", style = "margin-left: 30px;"),
                HTML("<br><br>"), 
                div(style = "display: flex; justify-content: center;", # Flexbox styles
                     actionButton("Preview_data", "Preview data")),
                HTML("<br>")
                ),
         
               mainPanel( uiOutput("resultUIuploadData"),
               ),
             ),
             div(style = "display: flex; justify-content: flex-start; margin-left: 46px", # This aligns the button to the left
               actionButton("go_to_Map", "Preview Map Distribution")
             ),
             HTML("<br>")
    ),
    tabPanel("Map Distribution",value = "Map_Distribution",
             HTML("<br><br>"),
             uiOutput("resultUIMap"),
             uiOutput("resultbuttonMap"),
             HTML("<br>")
    ),
    tabPanel("Analysis",value = "Analysis_data",
             HTML("<br><br>"),
             uiOutput("resultUIAnalysis"),
             uiOutput("resultbuttonAnalysis"),
             HTML("<br>")
    ),
    tabPanel("About Application", h2("About Application",style="color: #279DFF; margin-left: 0px;"),
             HTML("<br>"),
             h4("Background",style="color: #00000; margin-left: 0px;")),
    tabPanel("Manual", value = "Manual"),
    tabPanel("Help", "This panel is intentionally left blank")
  )
)

# Define server function
server <- function(input, output, session) {
  
  # สร้าง Reactive Valueสำหรับเก็บข้อมูล
  rv <- reactiveValues(map = NULL,
                       datos = NULL,
                       datosOriginal = NULL,
                       column_areaID_shp = NULL,
                       column_areaID_csv = NULL,
                       l_distibution = NULL,
                       covariate_df = NULL)

  uploadStatus <- reactiveValues(success = FALSE)
  
  # สร้างUIหน้าuploadฝั่งmainpanel
  output$resultUIuploadData <- renderUI({
    # Check if 'Preview' button has been pressed
    if (!input$Preview_data) {
      # Case 1: 'Preview' button has not been pressed
      # Return tabset panel with placeholders
      uploadStatus$success <- FALSE
      tabsetPanel(id = 'dataset',
                  tabPanel("Map Data (Shape file)", width = 12, img(src = "NoYeyData.png", width = "100%")),
                  tabPanel("Data (.csv file)", width = 12, img(src = "NoYeyData.png", width = "100%"))
      )
    } else {
      missingConditions <- c()  # Initialize the missing conditions vector
      
      # Check if files have been uploaded
      if (is.null(input$shpfile) || is.null(input$csvfile)) {
        # Case 2: Files have not been uploaded
        missingConditions <- c(missingConditions, "Please upload both Shapefile and CSV file.")
      } else {
        # Check if uploaded files meet the requirements
        if (!is.null(rv$datosOriginal) && !is.null(rv$map)) {
          # Check for numeric columns and valid values
          numericFields <- c(input$column_year, input$column_cases, input$column_level, input$column_population) # Replace 'column_population' with the actual input ID for the population column
          numericChecks <- sapply(numericFields, function(field) { 
            fieldData <- rv$datosOriginal[[field]]
            return(!is.null(fieldData) && is.numeric(fieldData))
          })
          if (any(!numericChecks)) {
            missingConditions <- c(missingConditions, "Year, Cases, Level, and Population columns must be numeric.")
          }
          
          # Additional checks for levels greater than 1 and unique values
          if (length(unique(rv$datosOriginal[[input$column_level]])) <= 1 || length(unique(rv$datosOriginal[[input$column_level]])) == 0) {
            missingConditions <- c(missingConditions, "Disease level column must have more than 1 level.")
          }
          
          
          # Checking for matching Area IDs
          if (!all(rv$datosOriginal[[input$column_areaID_csv]] %in% rv$map[[input$column_areaID_shp]])) {
            missingConditions <- c(missingConditions, "Area ID in CSV does not match Area ID in Shapefile.")
          }
        } else {
          missingConditions <- c(missingConditions, "Unable to read uploaded files. Please check the format.")
        }
      }
      
      # Display notification if there are missing conditions
      if (length(missingConditions) > 0) {
        # Case 3: There are missing or incorrect conditions
        missingText <- paste(missingConditions, collapse = " ")
        showNotification(paste("Missing or incorrect: ", missingText, "*Please correct the errors and press 'Preview Data' again."), type = "warning", duration = 10)
        
        uploadStatus$success <- FALSE
        # Return tabset panel with placeholders
        tabsetPanel(id = 'dataset',
                    tabPanel("Map Data (Shape file)", width = 12, img(src = "NoYeyData.png", width = "100%")),
                    tabPanel("Data (.csv file)", width = 12, img(src = "NoYeyData.png", width = "100%"))
        )
      } else {
        uploadStatus$success <- TRUE
        # Case 4: All conditions have been met
        # Render the UI components for the map and data tables
        outputUI <- tabsetPanel(id = 'dataset',
                            tabPanel("Map Data (Shape file)",
                                     h3("Example Map", style = "margin-top:0;"),
                                     div(class = 'dataTableWrapper',
                                         leafletOutput("map"),
                                         HTML("<br>"),
                                         h3("Example Data"),
                                         verbatimTextOutput("uploadmapsummary"),
                                         HTML("<br>")),
                                     h3("Summary Table"),
                                     dataTableOutput('uploadmaptable')),
                            tabPanel("Data (.csv file)",
                                     verbatimTextOutput("uploaddatasummary"),
                                     HTML("<br>"),
                                     DTOutput("uploaddatatable"))
        )
        # Set uploadStatus$success <- TRUE after rendering UI
        uploadStatus$success <- TRUE
        outputUI
      }
    }
  })
  
 
  # กำหนดตัวแปรเพื่อติดตามการกดปุ่ม 'Preview Data'
  previewDataViewed <- reactiveVal(FALSE)
  
  # ตรวจจับการกดปุ่ม 'Preview Data' และอัปเดตสถานะ
  observeEvent(input$Preview_data, {
    previewDataViewed(TRUE)
  }, ignoreInit = TRUE)
  
  # ฟังก์ชันเพื่อตรวจสอบและแสดงการแจ้งเตือน
  checkAndNotify <- function() {
    if (previewDataViewed()) {
      showNotification("You have made changes. Please press 'Preview Data' to refresh.", type = "default", duration = NULL, closeButton = TRUE)
      previewDataViewed(FALSE)  # รีเซ็ตค่าเมื่อแจ้งเตือนเสร็จสิ้น
    }
  }
  
  # ตรวจจับการอัปโหลดหรือการเปลี่ยนแปลงใดๆ และเรียกใช้ฟังก์ชัน checkAndNotify
  observeEvent(input$shpfile, {
    checkAndNotify()
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$column_cases, input$column_year, input$column_level, input$column_population,
                 input$column_areaName, input$column_areaID_csv), {
                   checkAndNotify()
                 }, ignoreInit = TRUE)
  
  observeEvent(c(input$covariate_1, input$covariate_2, input$covariate_3, input$covariate_4,
                 input$covariate_5, input$covariate_6, input$covariate_7, input$covariate_8), {
                   checkAndNotify()
                 }, ignoreInit = TRUE)
  
  
  
  
  

  observeEvent(input$go_to_upload_data, {
    updateNavbarPage(session, inputId = "navbar", selected = "upload_data")
  })
  observeEvent(input$go_to_Manual, {
    updateNavbarPage(session, inputId = "navbar", selected = "Manual")
  })
  observeEvent(input$go_to_Map, {
    updateNavbarPage(session, inputId = "navbar", selected = "Map_Distribution")
  })
  observeEvent(input$back_to_upload, {
    updateNavbarPage(session, inputId = "navbar", selected = "upload_data")
  })
  observeEvent(input$go_to_analysis_1, {
    updateNavbarPage(session, inputId = "navbar", selected = "Analysis_data")
  })
  observeEvent(input$back_to_upload_2, {
    updateNavbarPage(session, inputId = "navbar", selected = "upload_data")
  })
  observeEvent(input$go_to_analysis_2, {
    updateNavbarPage(session, inputId = "navbar", selected = "Analysis_data")
  })
  
  
  df <- eventReactive(input$csvfile, {
    req(input$csvfile)
    df <- read.csv(input$csvfile$datapath)
    return(df)
  })
  
  # Upload data
  observe({
    inFile <- input$csvfile
    if (is.null(inFile))
      return(invisible())
    rv$datosOriginal <- read.csv(inFile$datapath)
  })

  
  # ใช้ eventReactive เพื่อรอการอัปโหลดไฟล์และทำงานเมื่อไฟล์ถูกอัปโหลดเสร็จ
  uploaded_file <- eventReactive(input$shpfile, {
    shpdf <- input$shpfile
    
    # ตรวจสอบว่าไฟล์ Shapefile ถูกอัปโหลดหรือไม่
    if (is.null(shpdf)) {
      return(NULL)
    }
    
    # บันทึกไดเรกทอรีทำงานปัจจุบัน
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    
    # เปลี่ยนชื่อไฟล์ในไดเรกทอรีทำงาน
    for (i in 1:nrow(shpdf)) {
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
    
    # เปลี่ยนกลับไปที่ไดเรกทอรีทำงานปัจจุบัน
    setwd(previouswd)
    
    # อ่านข้อมูล Shapefile และแปลง CRS เป็น WGS84
    map <- readOGR(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))
    map <- spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
    # นำข้อมูล Shapefile มาเก็บไว้ใน Reactive Values
    rv$map <- map
    
    # ส่งค่าเพื่อให้ eventReactive ทำงานต่อ
    return(TRUE)
  })
  
  #update input for area id/name  map
  
  observeEvent(uploaded_file(), {
    
    
    x <- names(rv$map)
    xd <- c("-", x)
    # Can use character(0) to remove all choices
    if (is.null(x)) {
      x <- character(0)
      xd <- x
    }
    
    updateSelectInput(session, "column_areaID_shp", choices = x, selected = head(x, 1))
  })
  
 
  # แสดงรายการไฟล์ที่อัพโหลด
  observe({
    if (is.null(names(rv$datosOriginal)))
      xd <- character(0)
    
    xd<-names(rv$datosOriginal)
    
    if (is.null(xd))
      xd <- character(0)
    
    xd2<- c("-", xd)

    # สร้างตัวเลือกสำหรับทุกช่องเลือก
    #updateSelectInput(session, "column_areaName", choices = xd)
    updateSelectInput(session, "column_areaID_csv", choices = xd)
    updateSelectInput(session, "column_population", choices = xd)
    updateSelectInput(session, "column_cases", choices = xd)
    updateSelectInput(session, "column_year", choices = xd)
    updateSelectInput(session, "column_level", choices = xd)
    updateSelectInput(session, "covariate_1", choices = xd2)
    updateSelectInput(session, "covariate_2", choices = xd2)
    updateSelectInput(session, "covariate_3", choices = xd2)
    updateSelectInput(session, "covariate_4", choices = xd2)
    updateSelectInput(session, "covariate_5", choices = xd2)
    updateSelectInput(session, "covariate_6", choices = xd2)
    updateSelectInput(session, "covariate_7", choices = xd2)
    updateSelectInput(session, "covariate_8", choices = xd2)
  })
  output$Level_selector <- renderUI({
    #req(input$Preview_data)
    #req(input$column_level)
    req(uploadStatus$success)
    datos <- rv$datosOriginal
    levelselector <- unique(datos[, input$column_level])
    selectInput("level_selector", "Level", choices = levelselector)
  })
  output$Level_selector_Cluster <- renderUI({
    #req(input$Preview_data)
    #req(input$column_level)
    req(uploadStatus$success)
    datos <- rv$datosOriginal
    levelselector <- unique(datos[, input$column_level])
    selectInput("level_selector_Cluster", "Level", choices = levelselector)
  })
  output$Level_selector_Association <- renderUI({
    #req(input$Preview_data)
    #req(input$column_level)
    req(uploadStatus$success)
    datos <- rv$datosOriginal
    levelselector <- unique(datos[, input$column_level])
    selectInput("level_selector_Association", "Level", choices = levelselector)
  })
  output$Year_selector_Cluster <- renderUI({
    #req(input$Preview_data)
    #req(input$column_year)
    req(uploadStatus$success)
    datos <- rv$datosOriginal
    Yearselector <- unique(datos[, input$column_year])
    selectInput("year_selector_Cluster", "Year", choices = Yearselector)
  })
  output$Year_selector <- renderUI({
    #req(input$Preview_data)
    #req(input$column_year)
    req(uploadStatus$success)
    datos <- rv$datosOriginal
    Yearselector <- unique(datos[, input$column_year])
    selectInput("year_selector", "Year", choices = Yearselector)
  })
  
  
  

  #แสดงตัวอย่างmapในหน้าอัปโหลด
  output$map <- renderLeaflet({
    req(uploaded_file())
    req(rv$map)
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = rv$map, fillOpacity = 0.7, fillColor = "blue", color = "white", weight = 1)
  })
  #output$uploadmapsummary <- renderPrint({
    #if (!is.null(rv$map)){
      #print(summary(rv$map@data))
    #}
  #})
  
  output$uploadmapsummary <- renderPrint({
    if (!is.null(rv$map)){
      # ใช้ head() เพื่อแสดง 5 แถวแรกจากข้อมูล
      print(head(rv$map@data, 5))
    }
  })
  
  
  #output$uploadmaptable  <- renderDataTable({
    #if (is.null(rv$map))
      #return(NULL)
    #rv$map@data
  #})
  
  
  #output$uploadmaptable <- renderDataTable({
    #if (is.null(rv$map))
      #return(NULL)
    #datatable(rv$map@data, options = list(pageLength = 5))
  #})
  
  output$uploadmaptable <- renderDataTable({
    if (is.null(rv$map))
      return(NULL)
    
    # สร้าง list ของคอลัมน์ที่จะซ่อน
    # เริ่มตั้งแต่คอลัมน์ที่ 6 ไปจนถึงคอลัมน์สุดท้าย
    hidden_columns <- setdiff(1:ncol(rv$map@data), 1:5)
    
    # กำหนดค่า options สำหรับ DataTables
    datatable(rv$map@data, options = list(
      pageLength = 6,
      scrollX = TRUE,
      columnDefs = list(
        list(visible = FALSE, targets = hidden_columns - 1) # ลบ 1 เพราะ JavaScript นับตั้งแต่ 0
      )
    ))
  })
  
  
  
  
  #output$uploaddatasummary <- renderPrint({
    #if (!is.null(rv$datosOriginal)){
      #print(head(summary(rv$datosOriginal,5)))
    #}
  #})
  
  library(DT)
  
  output$uploaddatasummary <- renderDataTable({
    if (is.null(rv$datosOriginal)){
      return()
    }
    
    # สรุปข้อมูลและแปลงเป็น data.frame
    summary_data <- lapply(rv$datosOriginal, summary)
    
    # สร้าง matrix จากลิสต์ของเวกเตอร์
    # ตรวจสอบว่าทุกองค์ประกอบในลิสต์มีความยาวเท่ากันก่อน
    if (length(unique(sapply(summary_data, length))) == 1) {
      summary_matrix <- do.call(cbind, summary_data)
    } else {
      stop("The length of vectors in summary_data is not equal.")
    }
    
    # แปลงเป็น data.frame
    summary_df <- as.data.frame(summary_matrix, stringsAsFactors = FALSE)
    
    # แสดงผลในรูปแบบตารางโต้ตอบได้
    datatable(summary_df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  
  #แสดงตัวอย่างข้อมูลในไฟล์csvแบบตาราง
  #output$table <- renderDT({
    #datatable(df(), options = list(pageLength = 5))
  #})
  
  
  output$uploaddatatable <- renderDataTable({
    if (is.null(rv$datosOriginal) || ncol(rv$datosOriginal) < 6)
      return(NULL)
    
    # สร้าง list ของคอลัมน์ที่จะซ่อน
    hidden_columns <- setdiff(1:ncol(rv$datosOriginal), 1:5)
    
    # กำหนดค่า options สำหรับ DataTables
    datatable(rv$datosOriginal, options = list(
      pageLength = 5,
      scrollX = TRUE,
      columnDefs = list(
        list(visible = FALSE, targets = hidden_columns - 1) # ลบ 1 เพราะ JavaScript นับตั้งแต่ 0
      )
    ))
  })
  
  

  output$resultUIMap <- renderUI({
    # ตรวจสอบว่าผู้ใช้ได้กดปุ่ม Preview_data หรือยัง
    if (!input$Preview_data) {
      # ถ้ายังไม่กด แสดงแค่ภาพ
      img(src = "mapdistribution.png", width = "100%")
    } else {
      # หลังจากกดปุ่ม ใช้ req() เพื่อตรวจสอบ input อื่นๆ
      req(input$Preview_data)
      
      # Initialize logical vector to check conditions
      conditions_met <- c(
        !is.null(rv$map),
        !is.null(rv$datosOriginal),
        !is.null(input$column_cases),
        !is.null(input$column_year),
        !is.null(input$column_level)
      )
      
      # Debugging: print the conditions to the console
      print(conditions_met)
      
      # Check all conditions are TRUE
      if (all(conditions_met)) {
        # ถ้าเงื่อนไขทั้งหมดตรง แสดง UI ตามเงื่อนไข 'else'
        sidebarLayout(
          sidebarPanel(
            class = "sidebar-panel",
            tags$div( # กลุ่มองค์ประกอบเข้าด้วยกัน
              tags$h3(icon("globe", style = "color: #279DFF; margin-left: 2px;"),"Map Distribution",
                      style = "margin-left: 3px; display: inline;", 
              ), 
              style = "display: flex; align-items: center;" # จัดให้องค์ประกอบอยู่ในแนวเดียวกันและตรงกลาง
            ),
            #h3("Map Distribution"),
            HTML("<br>"),
            p("The Map Distribution displays an interactive distribution map of the user uploaded shapefile and csv file on the Upload data page using case column to plot. Users can visualize and select filters including time period and color scheme "),
            HTML("<br>"),
            tags$hr(style = "border: 1.5px solid #279DFF; width: 100%; margin: 0 auto;"),
            helpText(
              HTML("<h3><span style='color: #279DFF; font-weight: semi-bold;'>Filter</span></h3>")
            ),
            uiOutput("Level_selector"),
            uiOutput("Year_selector"),
            selectInput("colorScheme", "Color Scheme:",
                        choices = c("Reds", "Oranges", "Blues", "Purples", "Greens"),
                        selected = character(0)),
            HTML("<br>"),
            #downloadButton('downloadMap', 'Download Map'),
            #actionButton("save_map", "Save Map Image", icon = icon("camera"), class = "blue-button"),
            div(style = "display: flex; justify-content: center;",
                actionButton("save_map", "Save Map Image", icon = icon("camera", style = "color: #279DFF;"), 
                             style = "background-color: white; color: #279DFF; border: 1px solid #279DFF; font-size: 16px; padding: 8px 30px; border-radius: 30px;")
            ),
            HTML("<br>")
            
          ),
            mainPanel(
              leafletOutput("map_distribution", height = "81vh")
          )
        )
      } else {
        # ถ้าเงื่อนไขไม่ตรงทั้งหมด แสดงภาพ
        img(src = "mapdistribution.png", width = "100%")
      }
    }
  })
  
 
  
  
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste('map_distribution', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      # ดึงข้อมูลแผนที่จาก reactive values
      l <- rv$l_distribution
      if (is.null(l)) {
        stop("Map not yet rendered")
      }
      # บันทึกแผนที่ไปยังไฟล์
      mapshot(l, file = file)
    }
  )


  
  output$resultUIAnalysis <- renderUI({
    # ตรวจสอบว่าผู้ใช้ได้กดปุ่ม Preview_data หรือยัง
    if (!input$Preview_data) {
      # ถ้ายังไม่กด แสดงแค่ภาพ
      img(src = "mapdistribution.png", width = "100%")
    } else {
      # หลังจากกดปุ่ม ใช้ req() เพื่อตรวจสอบ input อื่นๆ
      req(input$Preview_data)
      
      # Initialize logical vector to check conditions
      conditions_met <- c(
        !is.null(rv$map),
        !is.null(rv$datosOriginal),
        !is.null(input$column_cases),
        !is.null(input$column_year),
        !is.null(input$column_level)
      )
      
      # Debugging: print the conditions to the console
      print(conditions_met)
      
      # Check all conditions are TRUE
      if (all(conditions_met)) {
        # ถ้าเงื่อนไขทั้งหมดตรง แสดง UI ตามเงื่อนไข 'else'
        sidebarLayout(
          sidebarPanel(class = "sidebar-panel",
                       tabsetPanel(
                         tabPanel("Cluster Detection",
                                  helpText(
                                    HTML("<span style='color: #279DFF; font-weight: normal;'>The cluster detection Tab</span>
                      <span style= 'color: #000000;font-weight: normal;'>displays a</span>
                      <span style= 'color: #000000;font-weight: bold;'>hotspot</span>
                      <span style= 'color: #000000;font-weight: normal;'>and</span>
                      <span style= 'color: #000000;font-weight: bold;'>non-hotspot</span>
                      <span style= 'color: #000000;font-weight: normal;'>  areas. They have the flexibility to select the model type for analysis 
                      and can filter the data by choosing the desired time period and level. 
                      Additionally, users can customize the color scheme for representing hotspots and non-hotspots. 
                      For more detailed information about the model, please refer to the </span>
                      <span style= 'color: #279DFF;font-weight: normal;'>Help Page.</span>")
                                  ),
                                  HTML("<br>"),
                                  tags$hr(style = "border: 1.5px solid #279DFF; width: 100%; margin: 0 auto;"),
                                  helpText(HTML("<h3><span style='color: #279DFF; font-weight: semi-bold;'>Model Type</span></h3>")),
                                  selectInput("Spatial", "Spatial",
                                              choices = c("intrinsic", "Leroux", "proper"),
                                              selected = character(0)),
                                  selectInput("Temporal", "Temporal",
                                              choices = c("rw1", "rw2"),
                                              selected = character(0)),
                                  HTML("<br>"), 
                                  div(style = "display: flex; justify-content: center;", # Flexbox styles
                                      actionButton("Analysis_Cluster", "Start Analysis",
                                                   style = "background-color: #279DFF; font-weight: bold; font-size: 16px; color: white; padding: 8px 30px; border-radius: 30px;")),
                                  HTML("<br>"),
                                  tags$hr(style = "border: 1.5px solid #279DFF; width: 100%; margin: 0 auto;"),
                                  helpText(HTML("<h3><span style='color: #279DFF; font-weight: semi-bold;'>Filter</span></h3>")),
                                  uiOutput("Level_selector_Cluster"),
                                  uiOutput("Year_selector_Cluster"),
                                  
                                  # The corrected code
                                  tags$div(
                                    class = "row",
                                    tags$div(
                                      class = "col-md-6",
                                      selectInput("colorScheme_hotspot", "Hotspot colors:",
                                                  choices = c("red", "magenta", "blue", "cyan", "green", "yellow"),
                                                  selected = character(0))
                                    ), # Closing parenthesis for the first tags$div(), comma needed after this parenthesis
                                    tags$div( # Added a tags$div() to wrap the Non-hotspot selectInput
                                      class = "col-md-6",
                                      selectInput("colorScheme_nonhotspot", "Non-hotspot colors:",
                                                  choices = c("grey", "black", "white"),
                                                  selected = character(0))
                                    )
                                    
                                  ), 
                                    
                                  HTML("<br>")
                         ),
                         tabPanel("Association with Factors",
                                  helpText(
                                    HTML("<span style='color: #000000; font-weight: normal;'>This tab displays an association between risk factors and case outcomes. From the analysis, this map indicates the: </span>
                      <span style= 'color: #000000;font-weight: bold;'>percent incresse value</span>
                      <span style= 'color: #000000;font-weight: normal;'>and</span>
                      <span style= 'color: #000000;font-weight: bold;'>significance</span>
                      <span style= 'color: #000000;font-weight: normal;'> of each risk factor in each area. Users can visualize by using filters including each risk factor and color scheme. For details of the model and value, plase refer to the</span>
                      <span style= 'color: #279DFF;font-weight: normal;'>Help Page.</span>")
                                  ),
                                  HTML("<br>"),
                                  HTML("<br>"),
                                  tags$hr(style = "border: 1.5px solid #279DFF; width: 100%; margin: 0 auto;"),
                                  helpText(HTML("<h3><span style='color: #279DFF; font-weight: semi-bold;'>Model Type</span></h3>")),
                                  selectInput("Spatial_Association", "Spatial",
                                              choices = c("intrinsic", "Leroux", "proper"),
                                              selected = character(0)),
                                  HTML("<br>"), 
                                  div(style = "display: flex; justify-content: center;", # Flexbox styles
                                      actionButton("Analysis_Association", "Start Analysis",
                                                   style = "background-color: #279DFF; font-weight: bold; font-size: 16px; color: white; padding: 8px 30px; border-radius: 30px;")),
                                  HTML("<br>"),
                                  tags$hr(style = "border: 1.5px solid #279DFF; width: 100%; margin: 0 auto;"),
                                  helpText(HTML("<h3><span style='color: #279DFF; font-weight: semi-bold;'>Filter</span></h3>")),
                                  uiOutput("Level_selector_Association"),
                                  uiOutput("factor_selector"),
                                  selectInput("Risk_Factor", "Risk Factor:",choices = NULL),
                                  tags$div(
                                    class = "row",
                                    tags$div(
                                      class = "col-md-6",
                                      selectInput("colorAssociation_hotspot", "Hotspot colors:",
                                                  choices = c("red", "magenta", "blue", "cyan", "green", "yellow"),
                                                  selected = character(0))
                                    ), # Closing parenthesis for the first tags$div(), comma needed after this parenthesis
                                    tags$div( # Added a tags$div() to wrap the Non-hotspot selectInput
                                      class = "col-md-6",
                                      selectInput("colorAssociation_nonhotspot", "Non-hotspot colors:",
                                                  choices = c("grey", "black", "white"),
                                                  selected = character(0))
                                    )
                                    
                                  ),
                                  HTML("<br>")
                         )
                       )
          ),
          mainPanel(leafletOutput("map_cluster",height = "115vh")
          )
        )
      } else {
        # ถ้าเงื่อนไขไม่ตรงทั้งหมด แสดงภาพ
        img(src = "mapdistribution.png", width = "100%")
      }
    }
  })
  
  output$resultbuttonMap <- renderUI({
    # ตรวจสอบว่าผู้ใช้ได้กดปุ่ม Preview_data หรือยัง
    if (!input$Preview_data) {
    } else {
      # หลังจากกดปุ่ม ใช้ req() เพื่อตรวจสอบ input อื่นๆ
      req(input$Preview_data)
      
      # Initialize logical vector to check conditions
      conditions_met <- c(
        !is.null(rv$map),
        !is.null(rv$datosOriginal),
        !is.null(input$column_cases),
        !is.null(input$column_year),
        !is.null(input$column_level)
      )
      
      # Debugging: print the conditions to the console
      print(conditions_met)
      
      # Check all conditions are TRUE
      if (all(conditions_met)) {
        # ถ้าเงื่อนไขทั้งหมดตรง แสดง UI ตามเงื่อนไข 'else'
        fluidRow(
          column(2, actionButton("back_to_upload", "Back to Upload Page", 
                                 style = "background-color: #B8B6B6; font-weight: bold; font-size: 16px; color: white; padding: 8px 30px; border-radius: 30px;")),
          column(2, actionButton("go_to_analysis_2", "Go to Analysis Page", 
                                 style = "background-color: #3366FF; font-weight: bold; font-size: 16px; color: white; padding: 8px 30px; border-radius: 30px;")),
        )
      } else {
      }
    }
  })
  
  
  output$resultbuttonAnalysis <- renderUI({
    # ตรวจสอบว่าผู้ใช้ได้กดปุ่ม Preview_data หรือยัง
    if (!input$Preview_data) {
    } else {
      # หลังจากกดปุ่ม ใช้ req() เพื่อตรวจสอบ input อื่นๆ
      req(input$Preview_data)
      
      # Initialize logical vector to check conditions
      conditions_met <- c(
        !is.null(rv$map),
        !is.null(rv$datosOriginal),
        !is.null(input$column_cases),
        !is.null(input$column_year),
        !is.null(input$column_level)
      )
      
      # Debugging: print the conditions to the console
      print(conditions_met)
      
      # Check all conditions are TRUE
      if (all(conditions_met)) {
        # ถ้าเงื่อนไขทั้งหมดตรง แสดง UI ตามเงื่อนไข 'else'
        div(style = "display: flex; justify-content: flex-start; margin-left: 87px", # This aligns the button to the left
            actionButton("back_to_upload_2", "Back to Upload Page",
                         style = "background-color: #3366FF; font-weight: bold; font-size: 16px; color: white; padding: 8px 30px; border-radius: 30px;")
        )
      } else {
      }
    }
  })
  
  
  
  output$map_distribution <- renderLeaflet({
    req(rv$map, rv$datosOriginal)
    req(input$year_selector)
    req(input$level_selector)
    # เรียกตัวแปรเฉย ๆ จะได้ใช้ง่าย
    data1 <- rv$datosOriginal
    map1 <- rv$map
    
    data1 <- data1 %>%
      filter(.data[[input$column_year]] == input$year_selector,
             .data[[input$column_level]] == input$level_selector)
    
    
    # รวมข้อมูล shapefile และ CSV file
    datafiltered <- data1
    ordercounties <- match(map1@data[, input$column_areaID_shp], datafiltered[, input$column_areaID_csv])  # match ระหว่างสองข้อมูลโดยใช้ column ของแต่ละชุดข้อมูล เป็นตัวเชื่อม เช่น อย่างในนี้จะเป็น ชื่อจังหวัดในไทย
    map1@data <- datafiltered[ordercounties, ]
    
    # สร้าง leaflet map
    l <- leaflet(map1) %>% addTiles()
    
    # กำหนดสีตามค่าข้อมูล
    pal <- colorNumeric(palette = input$colorScheme, domain = map1@data[, input$column_cases])
    
    labels <- sprintf("<strong> %s </strong> <br/>  %s : %s ",
                      map1@data[, input$column_areaID_csv],
                      input$column_cases,
                      map1@data[, input$column_cases]) %>%
      lapply(htmltools::HTML)
    
    l %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "ESRI National Geographic World Map") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addPolygons(
        color = "grey", weight = 1,
        fillColor = ~pal(map1@data[, input$column_cases]), fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 4),
        label = labels,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px", direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, values = ~map1@data[, input$column_cases], opacity = 0.7,
        title = input$column_cases, position = "bottomright"
      ) %>%
      addLayersControl(baseGroups = c("Open Street Map", "ESRI World Imagery", "ESRI National Geographic World Map", "CartoDB Positron"),
                       position = c("topleft"),
                       options = layersControlOptions(collapsed = TRUE))
  
    })
  

  Mmodel_reactive <- reactiveVal()
  
  observeEvent(input$Analysis_Cluster, {
    req(uploadStatus$success)
    req(rv$map)
    req(rv$datosOriginal)
    req(input$column_areaID_csv)
    req(input$column_year)
    req(input$column_level)
    req(input$column_cases)
    req(input$column_population)
    
    info_id <- showNotification("Modeling in progress. Please wait a moment.", type = "message", duration = NULL)
    
    carto <- rv$map 
    data <- rv$datosOriginal
    
    
    carto <- rv$map  
    data <- rv$datosOriginal
    ID.area <- input$column_areaID_csv
    ID.year <- input$column_year
    ID.disease <- input$column_level
    O <- input$column_cases
    E <- input$column_population
    spatial <- input$Spatial 
    temporal <- input$Temporal 
    interaction <- "TypeI" 
    strategy <- "simplified.laplace"
    
    Mmodel <- MCAR_INLA_ST(carto=carto, data=data, ID.area=ID.area, ID.year=ID.year, ID.disease=ID.disease,
                           O=O, E=E, spatial=spatial, temporal=temporal, interaction=interaction,
                           strategy=strategy)
    
    
    
      removeNotification(info_id)
      showNotification("Model created successfully.", type = "message")
      
      Mmodel_reactive(Mmodel)
      
      #browser()

  })
  
  
  
  
  output$map_cluster <- renderLeaflet({
    req(Mmodel_reactive())  
    req(rv$map) 
    req(rv$datosOriginal)
    req(input$column_year)
    req(input$column_level)
    
    Mmodel <- Mmodel_reactive()
    datos <- rv$datosOriginal
    
    
    #print(Mmodel)
    #print(input$column_year)
    #print(input$column_level)
    
    #browser()
    
    J <- length(unique(datos[[input$column_level]]))
    S <- length(unique(datos[[input$column_areaID_csv]]))  
    T <- length(unique(datos[[input$column_year]]))
    
    #print(J)
    #print(S)
    #print(T)
    
    years <- as.numeric(input$year_selector_Cluster)
    diseases <- as.numeric(input$level_selector_Cluster) 
    
    #browser()
    
    #print(years)
    #print(diseases)
    
    ## Cluster Detection ##
    #######################
    
    
    
    start_year <- unique(as.numeric(as.numeric_version(datos[[input$column_year]])))[1]
    
    #print(start_year)
    
    cluster_df <- datos %>%
      filter(
        .data[[input$column_year]] == years & .data[[input$column_level]] == diseases
      ) %>%
      select(
        name = !!sym(input$column_areaID_csv), 
        year = !!sym(input$column_year), 
        case = !!sym(input$column_cases)
      ) 
    
    #print(cluster_df)
    #browser()
    
    
    # Compute exceedance probabilities
    exceedance_prob <- sapply(
      Mmodel$marginals.fitted.values[((S * (years - start_year)) + (S * T * (diseases - 1)) + 1):(S * (years - start_year + 1) + S * T * (diseases - 1))],
      FUN = function(marg) {
        1 - inla.pmarginal(q = 1, marginal = marg)
      }
    )
    cluster_df[, "exceedance_prob"] <- exceedance_prob
    cluster_df$hotspot <- ifelse(cluster_df$exceedance_prob > 0.95, "hotspot", "non-hotspot")
    
    
    
    #print(cluster_df)
    #print(Mmodel)
    print(cluster_df)
    
    
    #browser()
    
    # เรียกตัวแปรเฉย ๆ จะได้ใช้ง่าย
    data1 <- cluster_df
    map1 <- rv$map
    
    # รวมข้อมูล shapefile และ CSV file
    datafiltered <- data1
    ordercounties <- match(map1@data[, input$column_areaID_shp], datafiltered[, "name"])  
    map1@data <- datafiltered[ordercounties, ]
    
    
    # สร้าง leaflet map
    l_cluster <- leaflet(map1) %>% addTiles()
    
    # กำหนดสีตามค่าข้อมูล

    hotspot_colors <- colorFactor(c(input$colorScheme_hotspot,input$colorScheme_nonhotspot), domain = c("hotspot", "non-hotspot"))
    # ใช้ sprintf และ lapply เพื่อสร้าง labels และแปลงเป็น HTML
    labels <- sprintf("<strong> %s </strong> <br/>  %s : %s ",
                      map1@data[, "name"],
                      "case",
                      map1@data[, "case"]) %>%
      lapply(htmltools::HTML)
    
    l_cluster %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "ESRI National Geographic World Map") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
      addPolygons(
        color = "grey", weight = 1,
        fillColor = ~hotspot_colors(map1@data$hotspot),
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 4),
        label = labels,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px", direction = "auto"
        )
      ) %>%
      addLegend(
        colors = c(input$colorScheme_hotspot,input$colorScheme_nonhotspot),
        labels = c("Hotspot", "Non-hotspot"),
        opacity = 0.7,
        title = "Hotspot Classification",
        position = "bottomright"
      ) %>%
      addLayersControl(baseGroups = c("Open Street Map", "ESRI World Imagery", "ESRI National Geographic World Map", "CartoDB Positron"),
                       position = c("topleft"),
                       options = layersControlOptions(collapsed = TRUE))
    
  
  })
  
  
  ################Assosiation###############
  
  covariate_df <- eventReactive(input$Preview_data, {
    req(uploadStatus$success)
    req(rv$map)
    req(rv$datosOriginal)
    
    selected_covariate <- c(input$covariate_1, input$covariate_2, input$covariate_3, input$covariate_4,
                            input$covariate_5, input$covariate_6, input$covariate_7, input$covariate_8)
    
    print(selected_covariate)
    # ตรวจสอบว่าคอลัมน์ที่เลือกมีอยู่ใน dataframe หรือไม่
    selected_covariate <- selected_covariate[selected_covariate %in% names(rv$datosOriginal)]
    print(selected_covariate)
    if (length(selected_covariate) > 0) {
      covariate_df <- rv$datosOriginal[, selected_covariate, drop = FALSE] 
      
      print(selected_covariate)
      return(covariate_df)
    } else {
      return(data.frame())
    }
  })
  
  observe({
    if (is.null(names(rv$covariate_df)))
      xd3 <- character(0)
    else
      xd3 <- names(rv$covariate_df)
    
    updateSelectInput(session, "Risk_Factor", choices = xd3)
  })
  
}

shinyApp(ui = ui, server = server)







