library(shiny)
library(tidyverse)
library(bs4Dash)
library(DT)
library(shinyjs)
library(shinyBS)
library(sodium)
library(RMariaDB)
library(matrixStats)
library(shinycssloaders)
library(DT)
library(shinyWidgets)
library(readxl)
library(highcharter)
library(shinyalert)
library(openxlsx)
library(lubridate)
library(slickR)
library(svglite)
library(RCurl)
library(plotly)


options(shiny.autoreload = TRUE)
options(scipen = 99999)
options(shiny.maxRequestSize = 30*1024^2)

######################################### GLOBAL #########################################

source("Function/hc_theme_sparkline_vb.R")
source("Function/valueBoxSpark.R")

total_line = c("Line 1","Line 2","Line 3")
sequence_proses = c("GreenVeneer","PressDryer","SupplyCore", "RepairCore",
                    "CekMC","Scraff","FingerJoint","Setting","GlueSpreader","ColdPress",
                    "HotPress","Dempul","PanelSaw",
                    "Sander","Seleksi")

##########################################################################################


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 box(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#FFFFFF; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Dashboard Analytical Production WCT Unit. Malang") 
                   ), width = 12, background = "gray-dark", collapsible = F) 
)

credentials = data.frame(
  username_id = c("user_ptb", "management_wct"),
  passod   = sapply(c("mypass1", "qwerty12345"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader(
  title = dashboardBrand(
    title = "KPI PT. WCT Malang",
    color = "primary",
    href = "https://wijayacahayatimber.com/id/",
    image = "https://www.linkpicture.com/q/white-plywood.png",
  ),
  skin = "light",
  status = "white",
  border = TRUE,
  sidebarIcon = icon("bars"),
  controlbarIcon = icon("th"),
  fixed = FALSE,
  leftUi = tagList(
    dropdownMenu(
      badgeStatus = "info",
      type = "notifications",
      notificationItem(
        inputId = "trigger_info_1",
        text = tags$div("Add notif setiap ada karyawan",tags$br(),"yang terkena SP"),
        status = "info"
      ),
      notificationItem(
        inputId = "trigger_info_2",
        text = tags$div("Reset otomatis setiap pergantian",tags$br(),"tahun"),
        status = "info"
      )
    ),
    dropdownMenu(
      badgeStatus = "info",
      type = "tasks",
      taskItem(
        inputId = "triggerAction3",
        text = "Adham progress",
        color = "orange",
        value = 70
      )
    )
  ),
  rightUi = dropdownMenu(
    badgeStatus = "danger",
    type = "messages",
    messageItem(
      inputId = "triggerAction1",
      message = "message 1",
      from = "Farhan Adham",
      image = "https://www.linkpicture.com/q/IMG20201112112225.jpg",
      time = "today",
      color = "lime"
    )
  )
)

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"),skin = "light",
                            status = "primary",
                            elevation = 3)

body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

controlbar <- dashboardControlbar(
  skin = "light",
  pinned = FALSE,
  overlay = FALSE,
  uiOutput("Controlbar") 
) 

ui<-dashboardPage(header, sidebar, body, controlbar, 
                  skin = "blue", fullscreen = T)

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            disconnectMessage(
              text = "Something gone wrong, contact Analyst (Adham).",
              refresh = "Refresh now",
              background = "#f89f43",
              colour = "white",
              overlayColour = "white",
              top = "center",
              overlayOpacity = 0.5,
              refreshColour = "brown"
            ),
            includeCSS("styles.css"),
            tags$style(HTML(".datepicker {z-index:99999 !important;}"),
                       HTML("hr {border-top: 1px solid #D9D9D9;}")),
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 0px;")
    
    
  })
  
  
  output$Controlbar <- renderUI({
    if (USER$login == TRUE & credentials[credentials$username_id == input$userName,]$permission %in% c("advanced","basic")){ 
      controlbarMenu(
        id = "controlbarmenu",
        controlbarItem(
          "How to use",
          tags$div( 
            tags$h3("Cara Penggunaan"),
            "Pilih secara berurutan untuk",
            tags$ol(
              tags$li("Nama Departement"), 
              tags$li("Nama Karyawan"), 
              tags$li("Jenis Pelanggaran")
            ),
            "Setelah itu klik tombol",tags$b("save"),
            tags$br(),
            tags$br(),
            tags$h3(tags$hr("Rules Terkait SP")),
            "Untuk menentukan aturan berapa point SP 1 hingga SP 3, 
            itu semua ditentukan oleh HR Manager dan perubahannya sendiri
            hanya bisa dilakukan oleh Developer",
            tags$br(),
            tags$br()
          )
        ),
        
        controlbarItem(
          "Accumulation",
          tags$h3(tags$i("Cara Melihat Akumulasi")),
          "Ketik button sidebar di sebelah kiri",tags$b("General"),"lalu 
            akan muncul 2 button secara otomatis",
          tags$ol(
            tags$li(tags$code("Nama Departement")), 
            tags$li(tags$code("Nama Karyawan"))
          ),
          tags$em("hanya berlaku user Advance")
        )
      )
    }
  })
  
  
  output$sidebarpanel <- renderUI({
    
    tryCatch({
      
      (if (USER$login == TRUE & credentials[credentials$username_id == input$userName,]$permission == "advanced"){ 
        sidebarMenu(id = "sidebar",
                    menuItem("Pilihan 1", tabName = "dashboard", icon = icon("chart-pie")),
                    
                    shiny::conditionalPanel(condition="input.sidebar == 'dashboard'",
                                            dateInput("dash_date_input",
                                                      label = "Choose Date",
                                                      value = "2023-05-21",
                                                      width='100%'),
                                            hr()),
                    
                    menuItem("Pilihan 2", tabName = "dashboard2", icon = icon("chart-simple")),
                    
                    shiny::conditionalPanel(condition="input.sidebar == 'dashboard2'",
                                            selectInput("dash2_select_input",
                                                        label = "Choose Line",
                                                        choices = total_line,
                                                        width='100%'),
                                            
                                            dateInput("dash2_date_input",
                                                      label = "Choose Date",
                                                      value = "2023-05-21",
                                                      width='100%'),
                                            
                                            br(),
                                            
                                            actionButton("execute_pilihan2",
                                                         label = "Execute",
                                                         status = "info",
                                                         width='87%',
                                                         outline = TRUE, 
                                                         flat = TRUE),
                                            hr()),
                    
                    menuItem("Pilihan 3", tabName = "dashboard3", icon = icon("cubes")),
                    
                    shiny::conditionalPanel(condition="input.sidebar == 'dashboard3'",
                                            dateInput("dash3_date_input",
                                                      label = "Choose Date",
                                                      value = "2023-05-21",
                                                      width='100%'),
                                            
                                            actionButton("modify_plot",
                                                         label = "Modify Plot",
                                                         status = "success",
                                                         width='87%',
                                                         outline = TRUE,
                                                         flat = TRUE),
                                            hr()),
                  
                    menuItem("Setting", tabName = "setting", icon = icon("gear"))
                    
                    
        )} else if (USER$login == TRUE & credentials[credentials$username_id == input$userName,]$permission == "basic"){
          
          sidebarMenu(id = "sidebar",
                      menuItem("General", tabName = "dashboard", icon = icon("dollar-sign"))  
                      
          )})},
      
      error = function(e)
        print(""))
    
  })
  
  
  output$body <- renderUI({
    if (USER$login == TRUE) {
      tabItems(
        tabItem(tabName ="dashboard",
                
                fluidRow(
                  column(
                    width = 12,
                    shinycustomloader::withLoader(
                      slickROutput("mainplot_kpi_wct", width = "100%",height='500px'), type = "html",
                      loader = "dnaspin",
                    )
                  )
                ),
                br(),
                fluidRow(
                  ##valueBoxOutput("vbox"),
                  ##valueBoxOutput("vbox2"),
                  ##valueBoxOutput("vbox3")
                )
                
        ),
        
        tabItem(tabName ="dashboard2",
                
                fluidRow(
                  column(
                    width = 12,
                    box(plotlyOutput("test1", height = '100%'), 
                        maximizable = T, closable = F, 
                        width = 12, height = 500)
                  )
                )
        ),
        
        tabItem(tabName ="dashboard3",
                tags$head(includeCSS("styles.css")),
                fluidRow(
                  column(
                    width = 12,
                    
                    highchartOutput("modify_dash3", height = 700),
                    
                    shiny::conditionalPanel(condition="input.sidebar == 'dashboard3' && input.modify_plot%2 == 1",
                    
                    absolutePanel(id = "controls", class = "panel panel-default",
                                  top = 75, left = 300, width = 250, fixed=TRUE,
                                  draggable = TRUE, height = "auto",
                                  
                                  span(tags$i(h6("Modify plot based on your needs")), style="color:#045a8d"),
                                  selectizeInput("modify_select_line",
                                                 label = "Line",
                                                 choices = total_line,
                                                 width = '100%', multiple = T),
                                  
                                  selectizeInput("modify_select_shift",
                                                 label = "Shift",
                                                 choices = c("A","B","C"),
                                                 width = '100%', multiple = T),
                                  
                                  selectizeInput(inputId = "modify_proses_plot",
                                                 label = "Proses", width = '100%',
                                                 choices = "",
                                                 multiple = T),
                                  
                                  selectizeInput(inputId = "modify_mesin_plot",
                                                 label = "Mesin", width = '100%',
                                                 choices = "",
                                                 multiple = T),
                                  hr(),
                                  
                                  selectInput(inputId = "type_plot",
                                              label = "Type of Plot",
                                              choices = c("Horizontal","Vertical"),
                                              selected = "Horizontal"),
                                  
                                  selectInput(inputId = "bar_type_plot",
                                              label = "Bar Type",
                                              choices = c("Stacked","One"),
                                              selected = "One")
                    )           
                    )
                    
                  )
                )
        ),
        
        tabItem(tabName ="setting",
                
                fluidRow(       
                  box(collapsible = F, width = 4,
                      fluidRow(
                        column(width = 4,
                               actionButton("create_target",
                                            label = "Create",
                                            status = "success",
                                            width='100%',
                                            outline = TRUE, 
                                            flat = TRUE)),
                        
                        column(width = 4,
                               actionButton("update_target",
                                            label = "Update",
                                            status = "info",
                                            width='100%',
                                            outline = TRUE, 
                                            flat = TRUE)),
                        
                        column(width = 4,
                               actionButton("delete_target",
                                            label = "Delete",
                                            status = "danger",
                                            width='100%',
                                            outline = TRUE, 
                                            flat = TRUE)
                        ),
                        
                        hidden(
                          
                          fluidRow(id = "box_create",
                                   style = "margin-left: 7px; padding-top:7px", 
                                   ## Inputan untuk create proses
                                   textInput(inputId = "create_proses",
                                             label = "Proses", width = '92%'),
                                   textInput(inputId = "create_line",
                                             label = "Line", width = '92%'),
                                   textInput(inputId = "create_mesin",
                                             label = "Mesin", width = '92%'),
                                   textInput(inputId = "create_ketebalan_kayu",
                                             label = "Ketebalan Kayu", width = '92%'),
                                   textInput(inputId = "create_ukuran_kayu",
                                             label = "Ukuran Kayu", width = '92%'),
                                   textInput(inputId = "create_target_jam",
                                             label = "Target/Jam", width = '92%'),
                                   br(),
                                   actionButton("create_save",
                                                label = "Save",
                                                status = "success",
                                                width = '92%',
                                                outline = TRUE, 
                                                flat = TRUE))
                          
                          ,
                          fluidRow(id = "box_update",
                                   style = "margin-left: 7px; padding-top:7px", 
                                   ## Inputan untuk update proses
                                   textInput(inputId = "update_input",
                                             label = "Input ID", width = '92%'),
                                   textInput(inputId = "update_proses",
                                             label = "Proses", width = '92%'),
                                   textInput(inputId = "update_line",
                                             label = "Line", width = '92%'),
                                   textInput(inputId = "update_mesin",
                                             label = "Mesin", width = '92%'),
                                   textInput(inputId = "update_ketebalan_kayu",
                                             label = "Ketebalan Kayu", width = '92%'),
                                   textInput(inputId = "update_ukuran_kayu",
                                             label = "Ukuran Kayu", width = '92%'),
                                   textInput(inputId = "update_target_jam",
                                             label = "Target/Jam", width = '92%'),
                                   br(),
                                   actionButton("update_save",
                                                label = "Save",
                                                status = "info",
                                                width='92%',
                                                outline = TRUE, 
                                                flat = TRUE))
                          
                          ,
                          fluidRow(id = "box_delete",
                                   style = "margin-left: 7px; padding-top:7px", 
                                   ## Inputan untuk delete proses
                                   selectizeInput(inputId = "delete_input",
                                                  label = "Delete ID", width = 250,
                                                  choices = "",
                                                  multiple = T),
                                   br(),
                                   actionButton("delete_save",
                                                label = "Delete",
                                                status = "danger",
                                                width = 250,
                                                outline = TRUE, 
                                                flat = TRUE)
                                   
                          )))
                  ),
                  
                  box(collapsible = F, width = 8,
                      fluidRow(
                        DT::dataTableOutput("data_history_target", height = 486)))
                ))
        
      )
    }
    else {
      loginpage
    }
  })
  
  
  ##################################### SERVER #####################################
  
  ## Data asli adalah data yg ini, namun masih belum di convert
  #data <- eventReactive(USER$login == TRUE, {
  #data <- readRDS("data.rds")
  
  #data$urutan_jam = convert_jam(data$Jam)
  #return(data)
  #})
  
  
  data <- eventReactive(USER$login, {
    data <- as.data.frame(readRDS("data.rds"))
    target <- as.data.frame(readRDS("data_target.rds"))
    
    data %>%
      mutate(`Ketebalan Kayu` = as.numeric(`Ketebalan Kayu`),
             gabungan = paste(Proses,Line,Mesin,`Ketebalan Kayu`,`Ukuran Kayu`, sep = ",")) %>% 
      left_join(target %>%
                  mutate(gabungan = paste(Proses,Line,Mesin,Ketebalan.Kayu,Ukuran.Kayu, sep = ",")) %>%
                  select(-c(Id, Proses,Line,Mesin,Ketebalan.Kayu,Ukuran.Kayu)), by = "gabungan") %>% 
      select(-gabungan) -> data
    
  return(data) 
  })
  
  
  observeEvent(input$sidebar == "dashboard", {
    
    output$mainplot_kpi_wct <- renderSlickR({
      
      data() %>%
        dplyr::filter(Line == "Line 1",
                      Date_Clean == input$dash_date_input) %>% 
        ungroup() %>% 
        group_by(Proses) %>%
        summarise(n = sum(as.numeric(`Total Pcs`))) %>%
        ungroup() %>%
        ggplot() + geom_bar(aes(x = factor(Proses, levels = sequence_proses), 
                                y = n,
                                fill = Proses),
                            stat = "identity") +
        geom_text(aes(x = Proses, y = n+max(n)*0.03, 
                      label = format(n, big.mark = ".", scientific = F),
                      fontface = "bold"),
                  size = 5) +
        labs(fill = NULL, x = "", y = NULL, 
             title = paste("\n LINE 1  (",format(as.Date(input$dash_date_input), "%d-%b-%Y"),")")) + 
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(size = 13),
              axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 25, hjust = .5)) -> plot1
      
      
      data() %>%
        dplyr::filter(Line == "Line 2",
                      Date_Clean == input$dash_date_input) %>% 
        ungroup() %>% 
        group_by(Proses) %>%
        summarise(n = sum(as.numeric(`Total Pcs`))) %>%
        ungroup() %>%
        ggplot() + geom_bar(aes(x = factor(Proses, levels = sequence_proses), 
                                y = n,
                                fill = Proses),
                            stat = "identity") +
        geom_text(aes(x = Proses, y = n+max(n)*0.03, 
                      label = format(n, big.mark = ".", scientific = F),
                      fontface = "bold"),
                  size = 5) +
        labs(fill = NULL, x = "", y = NULL, 
             title = paste("\n LINE 2  (",format(as.Date(input$dash_date_input), "%d-%b-%Y"),")")) + 
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(size = 13),
              axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 25, hjust = .5)) -> plot2
      
      
      data() %>%
        dplyr::filter(Line == "Line 3",
                      Date_Clean == input$dash_date_input) %>% 
        ungroup() %>% 
        group_by(Proses) %>%
        summarise(n = sum(as.numeric(`Total Pcs`))) %>%
        ungroup() %>%
        ggplot() + geom_bar(aes(x = factor(Proses, levels = sequence_proses), 
                                y = n,
                                fill = Proses),
                            stat = "identity") +
        geom_text(aes(x = Proses, y = n+max(n)*0.03, 
                      label = format(n, big.mark = ".", scientific = F),
                      fontface = "bold"),
                  size = 5) +
        labs(fill = NULL, x = "", y = NULL, 
             title = paste("\n LINE 3  (",format(as.Date(input$dash_date_input), "%d-%b-%Y"),")")) + 
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(size = 13),
              axis.text.y = element_text(size = 12),
              plot.title = element_text(size = 25, hjust = .5)) -> plot3
      
      
      image_to_plot = c("plot1","plot2","plot3")
      
      sapply(seq(length(image_to_plot)), function(x) {
        paste0("data:image/svg+xml;base64,",
               xmlSVG({show(get(image_to_plot[x]))},
                      standalone = T,height = 8,width = 14) %>% as.character() %>% base64()) 
      }) -> imgs
      
      invalidateLater(50000, session) 
      
      slickR(imgs, height = 575, width = "100%") + settings(slidesToShow=1,
                                                            autoplay = TRUE,
                                                            autoplaySpeed = 10000,
                                                            infinite = T,
                                                            cssEase = 'linear',
                                                            pauseOnFocus = F,
                                                            pauseOnDotsHover = F,
                                                            pauseOnHover = F) 
      
    })
    
    # output$vbox <- renderValueBox({
    #  
    #  data() %>%
    #    dplyr::filter(as.Date(Tgl) == "2023-05-23") %>%
    #    group_by(Jam) %>%
    #    summarise(y = sum(as.numeric(`Total Pcs`))) -> df
    # 
    #  hc <- hchart(df, "area", hcaes(Jam, y), name = "lines of code")  %>% 
    #    hc_size(height = 100) %>% 
    #    hc_credits(enabled = FALSE) %>% 
    #    hc_add_theme(hc_theme_sparkline_vb()) 
    #  
    #  valueBoxSpark(
    #    value = "1,345",
    #    title = toupper("Lines of code written"),
    #    sparkobj = hc,
    #    subtitle = tagList(HTML("&uarr;"), "25% Since last day"),
    #    info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
    #    icon = icon("code"),
    #    width = 4,
    #   color = "teal",
    #    href = NULL
    #  )
    #  
    # })
    
    
    # output$vbox2 <- renderValueBox({
    #  
    #  data() %>%
    #    dplyr::filter(as.Date(Tgl) == "2023-05-23") %>%
    #    group_by(Jam) %>%
    #    summarise(y = sum(as.numeric(`Total Pcs`))) -> df
    #  
    #  hc <- hchart(df, "column", hcaes(Jam, y), name = "lines of code")  %>% 
    #    hc_size(height = 100) %>% 
    #    hc_credits(enabled = FALSE) %>% 
    #    hc_add_theme(hc_theme_sparkline_vb()) 
    #  
    #  valueBoxSpark(
    #    value = "1,3 Hrs.",
    #    title = toupper("Thinking time"),
    #    sparkobj = hc,
    #    subtitle = tagList(HTML("&darr;"), "5% Since last year"),
    #    info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
    #    icon = icon("hourglass-half"),
    #    width = 4,
    #    color = "red",
    #    href = NULL
    #  )
    #  
    # })
    
    
    # output$vbox3 <- renderValueBox({
    #  
    #  data() %>%
    #    dplyr::filter(as.Date(Tgl) == "2023-05-23") %>%
    #    group_by(Jam) %>%
    #    summarise(y = sum(as.numeric(`Total Pcs`))) -> df
    #  
    #  hc <- hchart(df, "line", hcaes(Jam, y), name = "lines of code")  %>% 
    #    hc_size(height = 100) %>% 
    #    hc_credits(enabled = FALSE) %>% 
    #    hc_add_theme(hc_theme_sparkline_vb()) 
    #  
    #  valueBoxSpark(
    #    value = "1,3 Hrs.",
    #    title = toupper("Thinking time"),
    #    sparkobj = hc,
    #    subtitle = tagList(HTML("&darr;"), "5% Since last year"),
    #    info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
    #    icon = icon("hourglass-half"),
    #    width = 4,
    #    color = "yellow",
    #    href = NULL
    #  )
    # 
    # })
    
  })
  
  
  data2 <- reactive({
    
    data() %>%    
      filter(as.Date(Date_Clean) == as.character(input$dash2_date_input), 
             Line == as.character(input$dash2_select_input)) %>%
      group_by(Proses, Jam, urutan_jam2, Tgl) %>%
      summarise(n = sum(as.numeric(`Total Pcs`))) %>% 
      pivot_wider(names_from = Proses, values_from = n, values_fill = 0) %>% 
      pivot_longer(-c(Jam,urutan_jam2,Tgl)) %>% 
      mutate(jam_tgl = paste(substr(Tgl,9,10), Jam, sep = "- ")) -> fig2
    
    return(fig2)
  })
  
  
  v <- reactiveValues(plot = NULL)
  
  
  observeEvent(input$execute_pilihan2, {
    
    showModal(modalDialog("Please wait for few second", footer=NULL))
    
    v$plot <-  data2() %>%
      plot_ly(
        x = ~factor(name, levels = sequence_proses),
        y = ~value,
        color = ~name,
        frame = ~reorder(jam_tgl, urutan_jam2),
        type = 'bar',
        mode = 'markers'
      ) %>% 
      layout(title = paste("All Process in", input$dash2_select_input) , 
             xaxis = list(title = ''), 
             yaxis = list(title = 'Total in pcs'), 
             legend = list(title=list(text='<b> Process </b>'))) %>%
      animation_opts(1500, easing = "circle-out", redraw = FALSE) %>%
      animation_button(
        x = 1, xanchor = "left", y = 0, yanchor = "left"
      ) %>%
      animation_slider(
        currentvalue = list(prefix = "TGL ", font = list(color="red"))
      )
    
    removeModal()
    
  })
  
  output$test1 <- renderPlotly({
    if (is.null(v$plot)) return()
    v$plot
  })
  
  
  ################################## DATA FOR PILIHAN 3 ###############################
  
  data_filtered_pilihan3 <- reactive({
    data()[data()$Date_Clean == as.character(input$dash3_date_input),] -> data_filtered
    return(data_filtered)
  })
  
  
  observe({
    updateSelectizeInput(session, "modify_proses_plot", 
                         choices = unique(data_filtered_pilihan3()[data_filtered_pilihan3()$Line == as.character(input$modify_select_line),]$Proses))
  })
  
  observe({
    updateSelectizeInput(session, "modify_mesin_plot",
                         choices = unique(data_filtered_pilihan3()[data_filtered_pilihan3()$Proses %in% as.character(input$modify_proses_plot) & data_filtered_pilihan3()$Line == as.character(input$modify_select_line),]$Mesin),
                         selected = unique(data_filtered_pilihan3()[data_filtered_pilihan3()$Proses %in% as.character(input$modify_proses_plot) & data_filtered_pilihan3()$Line == as.character(input$modify_select_line),]$Mesin))
  })
  
  data_modify_plot <- reactive({
    
    data_filtered_pilihan3() %>%
      dplyr::filter(Line %in% as.character(input$modify_select_line),
                    Shift %in% as.character(input$modify_select_shift),
                    Proses %in% as.character(input$modify_proses_plot),
                    Mesin %in% as.character(input$modify_mesin_plot)) -> data_modify_plot
    
    data_modify_plot[data_modify_plot$Mesin == "-",]$Mesin = data_modify_plot[data_modify_plot$Mesin == "-",]$Proses
    
    return(data_modify_plot)
  })
  
  
  output$modify_dash3 <- renderHighchart({
    
    data_modify_plot() %>%
      group_by(Line, Proses, Mesin) %>%
      summarise(n = sum(as.numeric(`Total Pcs`))) %>%
      hchart(
        ifelse(as.character(input$type_plot) == "Horizontal", "column", "bar"), 
        hcaes(x = Mesin, y = n, group = Line),
        stacking = ifelse(as.character(input$bar_type_plot) == "One", "normal", ""),
        label = list(x = NULL)
      ) %>%
      hc_title(
        text = paste0("<b>",input$modify_select_line,"</b>"),
        margin = 20,
        align = "center",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_xAxis(title = list(text = NULL))
    
  })
  
  ###################################################################################
  
  
  ##################################### Setting #####################################
  
  
  observeEvent(input$create_target, {
    if(input$create_target%% 2 == 1){
      shinyjs::show(id = "box_create")
      shinyjs::hide(id = "box_update")
      shinyjs::hide(id = "box_delete")
      
    } else {
      shinyjs::hide(id = "box_create")
      shinyjs::hide(id = "box_update")
      shinyjs::hide(id = "box_delete")
    }})
  
  
  ## Logic untuk create new data
  observeEvent(input$create_save, {
    
    data_target <- readRDS("data_target.rds")
    new_value_target =  data.frame(Id = as.numeric(max(data_target$Id)+1),
                                   Proses = trimws(as.character(input$create_proses), which = "both"),
                                   Line = trimws(as.character(input$create_line), which = "both"),
                                   Mesin = trimws(as.character(input$create_mesin), which = "both"),
                                   Ketebalan.Kayu = trimws(as.numeric(input$create_ketebalan_kayu), which = "both"),
                                   Ukuran.Kayu = trimws(as.character(input$create_ukuran_kayu), which = "both"),
                                   Target.Jam = trimws(as.numeric(input$create_target_jam), which = "both")) 

    
    data_target <- rbind(data_target, new_value_target)
    saveRDS(data_target, file = "data_target.rds")
    
  }, ignoreInit = TRUE)
  
  
  ## Logic untuk update data berdasar nomor ID nya
  observeEvent(input$update_save, {
    
    data_target <- readRDS("data_target.rds")
    
    data_target[data_target$Id == as.numeric(
      input$update_input),]$Proses = as.character(input$update_proses)
    
    data_target[data_target$Id == as.numeric(
      input$update_input),]$Line = as.character(input$update_line)
    
    data_target[data_target$Id == as.numeric(
      input$update_input),]$Mesin = as.character(input$update_mesin)
    
    data_target[data_target$Id == as.numeric(
      input$update_input),]$Ketebalan.Kayu = as.numeric(input$update_ketebalan_kayu)
    
    data_target[data_target$Id == as.numeric(
      input$update_input),]$Ukuran.Kayu = as.character(input$update_ukuran_kayu)
    
    data_target[data_target$Id == as.numeric(
      input$update_input),]$Target.Jam = as.numeric(input$update_target_jam)
    
    saveRDS(data_target, file = "data_target.rds")
  }, ignoreInit = TRUE)
  
  
  ## Logic untuk delete data
  observeEvent(input$delete_save, {
    
    data_target <- readRDS("data_target.rds")
    
    data_target <- data_target[!data_target$Id %in% c(input$delete_input),]
    saveRDS(data_target, file = "data_target.rds")
    
  }, ignoreInit = TRUE) 
  
  
  observeEvent(input$update_target, {
    if(input$update_target%% 2 == 1){
      shinyjs::hide(id = "box_create")
      shinyjs::show(id = "box_update")
      shinyjs::hide(id = "box_delete")
      
    } else {
      shinyjs::hide(id = "box_create")
      shinyjs::hide(id = "box_update")
      shinyjs::hide(id = "box_delete")
    }})
  
  observeEvent(input$delete_target, {
    if(input$delete_target%% 2 == 1){
      shinyjs::hide(id = "box_create")
      shinyjs::hide(id = "box_update")
      shinyjs::show(id = "box_delete")
      
    } else {
      shinyjs::hide(id = "box_create")
      shinyjs::hide(id = "box_update")
      shinyjs::hide(id = "box_delete")
    }})
  
  
  observe({
    
    data_target <- readRDS("data_target.rds")
    
    ## Update box
    updateSelectInput(session, "update_proses",
                      selected = ifelse(is.na(input$update_input) == TRUE, "",
                                        data_target[data_target$Id == as.numeric(input$update_input),]$Proses))
    updateSelectInput(session, "update_line",
                      selected = ifelse(is.na(input$update_input) == TRUE, "",
                                        data_target[data_target$Id == as.numeric(input$update_input),]$Line))
    updateSelectInput(session, "update_mesin",
                      selected = ifelse(is.na(input$update_input) == TRUE, "",
                                        data_target[data_target$Id == as.numeric(input$update_input),]$Mesin))
    updateSelectInput(session, "update_ketebalan_kayu",
                      selected = ifelse(is.na(input$update_input) == TRUE, "",
                                        data_target[data_target$Id == as.numeric(input$update_input),]$Ketebalan.Kayu))
    updateSelectInput(session, "update_ukuran_kayu",
                      selected = ifelse(is.na(input$update_input) == TRUE, "",
                                        data_target[data_target$Id == as.numeric(input$update_input),]$Ukuran.Kayu))
    updateSelectInput(session, "update_target_jam",
                      selected = ifelse(is.na(input$update_input) == TRUE, "",
                                        data_target[data_target$Id == as.numeric(input$update_input),]$Target.Jam))
    
    ## Delete box
    updateSelectizeInput(session, "delete_input",
                         choices = sort(data_target$Id, decreasing = T))
  })
  
  
  output$data_history_target <- renderDataTable({
    
    data_target <- readRDS("data_target.rds")
    data_target %>%
      arrange(desc(Id)) -> data_target
    
    data_target %>%
      DT::datatable(filter = "none", rownames = F
                    ,extensions = 'Buttons'
                    ,options = list(
                      scrollX = T,
                      paging = TRUE,
                      searching = TRUE,
                      fixedColumns = T,
                      autoWidth = F,
                      ordering = TRUE,
                      dom = 'Bfrtip',
                      pageLength=-1,
                      buttons = list(
                        list(extend = 'excel', filename = "data_target", title = NULL),
                        list(extend = 'copy', filename = "data_target", title = NULL))
                    ),
                    class = "display",
                    fillContainer = TRUE,
                    escape=F) -> data_table
    
    if(input$create_save == TRUE | input$update_save == TRUE | input$delete_save == TRUE) {
      data_table
    } else {
      data_table
    }
    
  })
  
}

shinyApp(ui = ui, server = server) 