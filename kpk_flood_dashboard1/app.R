#########################################################
# KPK Flood Dashboard - Elite Edition
# Prepared by: Owais Ali Shah
# Empowering Policy Through Data
#########################################################

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(highcharter)
library(waiter)
library(shinycssloaders)
library(shinyWidgets)
library(bslib)
library(stringr)
library(shinyjs)
library(tibble) # Needed for 'tribble' to create the data frame

# ======================
# Translation Setup
# ======================
# Create translation dictionary for flood data context
translations <- list(
  en = list(
    dashboard_title = "KPK Flood Impact Dashboard",
    overview_details = "Overview & Details",
    trends = "Trend Analysis",
    explorer = "Data Explorer",
    about = "About",
    select_district = "Select District:",
    year_range = "Year Range:",
    total_districts = "Total Districts Affected",
    time_period = "Time Period Covered",
    data_points = "Total Data Points",
    total_area = "Total Affected Area (Ha)",
    key_indicators = "Affected Area by District",
    quick_insights = "Quick Insights",
    recent_updates = "Top Affected Districts",
    district_trends = "District Trends Over Time",
    trend_options = "Trend Analysis Options",
    statistical_summary = "Statistical Summary",
    indicator_data = "Raw Flood Data",
    download_data = "Download Data",
    about_title = "About KPK Flood Dashboard",
    about_project_details = "This dashboard provides a comprehensive view of flood-affected areas in Khyber Pakhtunkhwa (KPK), enabling analysis of historical trends and impact by district.",
    about_author_details = "This dashboard is a data visualization project by Owais Ali Shah, an economics graduate and a data analyst with a passion for using data to understand and solve real-world problems. This dashboard is a reflection of that commitment."
  ),
  ur = list(
    dashboard_title = "خیبر پختونخوا سیلاب اثرات ڈیش بورڈ",
    overview_details = "جائزہ اور تفصیلات",
    trends = "رجحان کا تجزیہ",
    explorer = "ڈیٹا ایکسپلورر",
    about = "کے بارے میں",
    select_district = "ضلع منتخب کریں:",
    year_range = "سال کی رینج:",
    total_districts = "متاثرہ اضلاع کی کل تعداد",
    time_period = "احاطہ شدہ وقت",
    data_points = "کل ڈیٹا پوائنٹس",
    total_area = "کل متاثرہ رقبہ (ہیکٹر)",
    key_indicators = "ضلع کے لحاظ سے متاثرہ رقبہ",
    quick_insights = "فوری بصیرت",
    recent_updates = "سرفہرست متاثرہ اضلاع",
    district_trends = "اضلاع کا وقت کے ساتھ رجحان",
    trend_options = "رجحان کے تجزیہ کے اختیارات",
    statistical_summary = "اعداد و شمار کا خلاصہ",
    indicator_data = "خام سیلاب ڈیٹا",
    download_data = "ڈیٹا ڈاؤن لوڈ کریں",
    about_title = "خیبر پختونخوا سیلاب ڈیش بورڈ کے بارے میں",
    about_project_details = "یہ ڈیش بورڈ خیبر پختونخوا (کے پی کے) میں سیلاب سے متاثرہ علاقوں کا ایک جامع جائزہ فراہم کرتا ہے، جو تاریخی رجحانات اور ضلع کے لحاظ سے اثرات کا تجزیہ کرنے کے قابل بناتا ہے۔",
    about_author_details = "یہ ڈیش بورڈ اویس علی شاہ کا ایک ڈیٹا ویژولائزیشن پراجیکٹ ہے، جو ایک اقتصادیات کا گریجویٹ اور ڈیٹا تجزیہ کار ہے اور حقیقی دنیا کے مسائل کو سمجھنے اور حل کرنے کے لیے ڈیٹا کا استعمال کرنے کا جذبہ رکھتا ہے۔ یہ ڈیش بورڈ اسی عزم کا عکاس ہے۔"
  )
)

# ======================
# Load & Clean Data
# ======================
# Set up waiting screen
waiting_screen <- tagList(
  div(
    style = "text-align: center; color: white;",
    div(
      style = "margin-bottom: 20px;",
      img(src = "https://upload.wikimedia.org/wikipedia/commons/3/32/Flag_of_Pakistan.svg",
          height = "60px")
    ),
    h4("Loading KPK Flood Database", style = "margin-bottom: 20px;"),
    spin_orbiter(),
    p("Preparing data for analysis...", style = "margin-top: 20px;")
  )
)

# Load the KPK flood data using the provided content
data <- tribble(
  ~Year, ~district, ~Affected_Area_Ha,
  2010, "Charsadda", 55000,
  2010, "Nowshera", 48000,
  2010, "Peshawar", 32000,
  2010, "Swat", 41000,
  2010, "Dir Lower", 25000,
  2010, "D.I.Khan", 38000,
  2010, "Tank", 21000,
  2011, "Kohistan", 12000,
  2011, "Mansehra", 9000,
  2012, "Chitral", 15000,
  2012, "Peshawar", 5000,
  2013, "Nowshera", 8500,
  2014, "D.I.Khan", 11000,
  2014, "Tank", 7500,
  2015, "Chitral", 22000,
  2015, "Shangla", 13000,
  2016, "Swat", 9500,
  2017, "Mansehra", 6000,
  2018, "Peshawar", 4000,
  2019, "Dir Upper", 7000,
  2020, "Kohistan", 14000,
  2020, "Bannu", 8000,
  2021, "Mardan", 5500,
  2022, "Swat", 62000,
  2022, "Dir Lower", 45000,
  2022, "Charsadda", 41000,
  2022, "Nowshera", 38000,
  2022, "D.I.Khan", 35000,
  2022, "Tank", 29000,
  2022, "Kohistan", 25000,
  2023, "Chitral", 18000,
  2023, "Shangla", 11000
)

# Get unique years and districts
available_years <- unique(data$Year)
available_districts <- unique(data$district)

# ======================
# Custom Theme & Styling
# ======================
custom_css <- "
/* Main styling */
body {
  font-family: 'Inter', sans-serif;
  background-color: #f8f9fa;
  color: #333;
}

/* Header styling with professional gradient */
.skin-blue .main-header .navbar {
  background: linear-gradient(135deg, #4caf50 0%, #388e3c 100%) !important;
}

.skin-blue .main-header .logo {
  background-color: transparent !important;
  color: #fff !important;
  font-weight: 700;
}

.skin-blue .main-header .logo:hover {
  background-color: transparent !important;
}

/* Professional sidebar */
.skin-blue .main-sidebar {
  background-color: #2c3e50 !important;
}

.skin-blue .sidebar-menu > li > a {
  border-left: 3px solid transparent;
  color: #ecf0f5;
}

.skin-blue .sidebar-menu > li.active > a, 
.skin-blue .sidebar-menu > li > a:hover {
  border-left-color: #4caf50;
  background-color: #34495e !important;
  color: #fff;
}

/* Enhanced boxes with subtle shadows */
.box {
  border-top: 3px solid #4caf50;
  border-radius: 8px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08) !important;
  transition: transform 0.2s, box-shadow 0.2s;
}

.box:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 16px rgba(0, 0, 0, 0.12) !important;
}

.box-header {
  border-bottom: 1px solid #e9ecef;
  padding: 15px;
  font-weight: 600;
}

/* Professional value boxes */
.small-box {
  border-radius: 8px;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  transition: transform 0.2s;
}

.small-box:hover {
  transform: translateY(-3px);
}

.small-box h3 {
  font-size: 24px;
  font-weight: 700;
}

.small-box .icon {
  font-size: 70px;
  top: -10px;
}

/* Enhanced buttons */
.btn {
  border-radius: 4px;
  font-weight: 500;
  transition: all 0.2s;
}

.btn-primary {
  background-color: #4caf50;
  border-color: #4caf50;
}

.btn-primary:hover,
.btn-primary:focus {
  background-color: #388e3c;
  border-color: #388e3c;
}

/* Switch styling for language toggle */
.form-switch .form-check-input {
  width: 2.5em;
  height: 1.25em;
  background-color: #ccc;
  border-radius: 1.25em;
  transition: all 0.2s ease-in-out;
  cursor: pointer;
}

.form-switch .form-check-input:checked {
  background-color: #4caf50;
}

/* DT table styling */
.dataTables_wrapper .dataTables_filter input {
  border-radius: 4px;
  border: 1px solid #ccc;
  padding: 6px 12px;
}

/* Custom spinners */
.waiter-overlay-content .fa-spinner {
  font-size: 3em;
  color: #fff;
}

/* Footer styling */
.main-footer {
  background-color: #2c3e50;
  color: #fff;
  padding: 15px;
  text-align: center;
}
"
# ======================
# UI Definition
# ======================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = div(
      style = "text-align: left; padding: 10px;",
      h4(textOutput("dashboard_title"), style = "color: white; margin: 0; padding: 0;"),
      h6("by Owais Ali Shah", style = "color: white; margin: 0; padding: 0;")
    ),
    titleWidth = 350,
    tags$li(
      class = "dropdown",
      div(
        style = "margin-top: 10px; margin-right: 15px; display: inline-block;",
        span(style = "color:white;", "EN"),
        switchInput(
          inputId = "lang_toggle",
          value = FALSE,
          size = "mini"
        ),
        span(style = "color:white;", "UR")
      )
    )
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem(textOutput("overview_details"), tabName = "overview_details", icon = icon("info-circle")),
      menuItem(textOutput("trends"), tabName = "trends", icon = icon("chart-line")),
      menuItem(textOutput("explorer"), tabName = "explorer", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    use_waiter(),
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "overview_details",
        fluidRow(
          valueBoxOutput("total_districts_box"),
          valueBoxOutput("time_period_box"),
          valueBoxOutput("data_points_box"),
          valueBoxOutput("total_area_box")
        ),
        fluidRow(
          box(
            title = textOutput("key_indicators"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            column(
              width = 7,
              withSpinner(highchartOutput("overview_chart", height = "400px"))
            ),
            column(
              width = 5,
              h4(textOutput("quick_insights")),
              htmlOutput("insights_text", class = "insights-box"),
              br(),
              downloadButton("download_overview_data", textOutput("download_data"), class = "btn-primary")
            )
          )
        ),
        fluidRow(
          box(
            title = textOutput("recent_updates"),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("top_districts_table")
          )
        ),
        # Merged content from the 'About' tab
        fluidRow(
          box(
            title = textOutput("about_title"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h4(strong("Project Details")),
            HTML(paste0("<h4>", textOutput("about_project_details"), "</h4>")),
            br(),
            h4(strong("About the Author")),
            HTML(paste0("<h4>", textOutput("about_author_details"), "</h4>"))
          )
        )
      ),
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = textOutput("trend_options"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            column(
              width = 4,
              selectInput("district_select", textOutput("select_district"),
                          choices = available_districts)
            ),
            column(
              width = 4,
              sliderInput("year_range", textOutput("year_range"),
                          min = min(available_years), max = max(available_years),
                          value = range(available_years), sep = "")
            ),
            column(
              width = 4,
              radioButtons("trend_type", "Select Chart Type:",
                           choices = c("Line" = "line", "Area" = "area", "Bar" = "column"),
                           selected = "line", inline = TRUE)
            )
          )
        ),
        fluidRow(
          box(
            title = textOutput("district_trends"),
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            withSpinner(highchartOutput("trend_chart", height = "400px"))
          ),
          box(
            title = textOutput("statistical_summary"),
            status = "info",
            solidHeader = TRUE,
            width = 4,
            verbatimTextOutput("trend_summary")
          )
        )
      ),
      tabItem(
        tabName = "explorer",
        fluidRow(
          box(
            title = textOutput("indicator_data"),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            withSpinner(DTOutput("data_table"))
          )
        )
      )
    ),
    # Footer section with your name
    tags$footer(
      tags$div(
        class = "main-footer",
        HTML("© 2025 Owais Ali Shah - All rights reserved.")
      )
    )
  )
)

# ======================
# Server Logic
# ======================
server <- function(input, output, session) {
  
  # Reactive value for current language
  current_language <- reactiveVal("en")
  
  # Initialize waiter
  waiter <- Waiter$new(
    id = c("overview_chart", "trend_chart", "data_table", "top_districts_table"),
    html = waiting_screen,
    color = "#333333"
  )
  
  # Observe language toggle
  observeEvent(input$lang_toggle, {
    if (input$lang_toggle) {
      current_language("ur")
      showNotification("Language switched to Urdu", type = "message")
    } else {
      current_language("en")
      showNotification("Language switched to English", type = "message")
    }
  })
  
  # Function to get translated text
  t <- function(key) {
    translations[[current_language()]][[key]]
  }
  
  # Reactive menu item labels (and other translated text)
  output$dashboard_title <- renderText({ t("dashboard_title") })
  output$trends <- renderText({ t("trends") })
  output$explorer <- renderText({ t("explorer") })
  output$overview_details <- renderText({ t("overview_details") })
  output$about_title <- renderText({ t("about_title") })
  output$about_project_details <- renderText({ t("about_project_details") })
  output$about_author_details <- renderText({ t("about_author_details") })
  output$select_district <- renderText({ t("select_district") })
  output$year_range <- renderText({ t("year_range") })
  output$key_indicators <- renderText({ t("key_indicators") })
  output$quick_insights <- renderText({ t("quick_insights") })
  output$recent_updates <- renderText({ t("recent_updates") })
  output$download_data <- renderText({ t("download_data") })
  output$trend_options <- renderText({ t("trend_options") })
  output$district_trends <- renderText({ t("district_trends") })
  output$statistical_summary <- renderText({ t("statistical_summary") })
  output$indicator_data <- renderText({ t("indicator_data") })
  
  # Reactive data based on filters (for Trend Analysis)
  filtered_data <- reactive({
    req(input$district_select)
    data %>%
      filter(
        district == input$district_select,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })
  
  # ================= Value Boxes =================
  output$total_districts_box <- renderValueBox({
    valueBox(
      value = length(unique(data$district)),
      subtitle = t("total_districts"),
      icon = icon("map-marker-alt"),
      color = "purple",
      width = 3
    )
  })
  
  output$time_period_box <- renderValueBox({
    valueBox(
      value = paste(min(data$Year), "-", max(data$Year)),
      subtitle = t("time_period"),
      icon = icon("calendar-alt"),
      color = "green",
      width = 3
    )
  })
  
  output$data_points_box <- renderValueBox({
    valueBox(
      value = format(nrow(data), big.mark = ","),
      subtitle = t("data_points"),
      icon = icon("database"),
      color = "blue",
      width = 3
    )
  })
  
  output$total_area_box <- renderValueBox({
    valueBox(
      value = format(sum(data$Affected_Area_Ha, na.rm = TRUE), big.mark = ","),
      subtitle = t("total_area"),
      icon = icon("chart-area"),
      color = "red",
      width = 3
    )
  })
  
  # ================= Overview Tab Content =================
  
  # Top affected districts table
  output$top_districts_table <- renderDT({
    waiter$show()
    on.exit(waiter$hide())
    
    top_districts <- data %>%
      group_by(district) %>%
      summarize(
        Total_Area_Ha = sum(Affected_Area_Ha, na.rm = TRUE),
        Max_Year = Year[which.max(Affected_Area_Ha)]
      ) %>%
      arrange(desc(Total_Area_Ha)) %>%
      head(5) %>%
      rename(
        District = district,
        `Total Affected Area (Ha)` = Total_Area_Ha,
        `Year of Max Impact` = Max_Year
      )
    
    datatable(
      top_districts,
      options = list(
        dom = 't',
        pageLength = 5,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 'Total Affected Area (Ha)', digits = 0)
  })
  
  # Quick Insights text
  output$insights_text <- renderUI({
    # Use all data for a general insight
    total_area <- sum(data$Affected_Area_Ha, na.rm = TRUE)
    
    # District with max total affected area
    max_district <- data %>%
      group_by(district) %>%
      summarise(Total = sum(Affected_Area_Ha)) %>%
      filter(Total == max(Total)) %>%
      pull(district) %>%
      first()
    
    # Year with max total affected area
    max_year <- data %>%
      group_by(Year) %>%
      summarise(Total = sum(Affected_Area_Ha)) %>%
      filter(Total == max(Total)) %>%
      pull(Year) %>%
      first()
    
    HTML(paste0(
      "<div style='font-size: 14px; line-height: 1.6'>",
      "<p><strong>Total Affected Area:</strong> ", format(total_area, big.mark = ","), " Ha</p>",
      "<p><strong>Most Affected District:</strong> <span style='color:#dc3545;'>", max_district, "</span></p>",
      "<p><strong>Worst Flood Year:</strong> <span style='color:#dc3545;'>", max_year, "</span></p>",
      "<p><strong>Average Area per Event:</strong> ", format(round(mean(data$Affected_Area_Ha, na.rm = TRUE), 0), big.mark = ","), " Ha</p>",
      "</div>"
    ))
  })
  
  # Overview chart - Total affected area by district (Bar chart)
  output$overview_chart <- renderHighchart({
    waiter$show()
    on.exit(waiter$hide())
    
    summary_data <- data %>%
      group_by(district) %>%
      summarize(Total_Area = sum(Affected_Area_Ha, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Area))
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = t("key_indicators")) %>%
      hc_xAxis(categories = summary_data$district, title = list(text = "District")) %>%
      hc_yAxis(title = list(text = "Total Affected Area (Ha)")) %>%
      hc_add_series(
        name = "Affected Area (Ha)",
        data = summary_data$Total_Area,
        color = "#388e3c" # Green
      ) %>%
      hc_tooltip(
        valueDecimals = 0,
        shared = TRUE,
        crosshairs = TRUE,
        pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f} Ha</b><br/>'
      ) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = FALSE)))
  })
  
  # Download handler for raw data
  output$download_overview_data <- downloadHandler(
    filename = function() {
      paste0("kpk_flood_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # ================= Trend Analysis Tab Content =================
  
  # Trend chart
  output$trend_chart <- renderHighchart({
    waiter$show()
    on.exit(waiter$hide())
    
    current_data <- filtered_data()
    
    if (nrow(current_data) == 0) {
      return(highchart() %>% hc_title(text = "No data available for the selected district and year range."))
    }
    
    plot_data <- current_data %>%
      group_by(Year) %>%
      summarise(Total_Affected_Area = sum(Affected_Area_Ha, na.rm = TRUE), .groups = 'drop')
    
    hc_type <- ifelse(input$trend_type == "line", "line",
                      ifelse(input$trend_type == "area", "area", "column"))
    
    highchart() %>%
      hc_chart(type = hc_type) %>%
      hc_title(text = paste(t("district_trends"), ":", input$district_select)) %>%
      hc_xAxis(categories = plot_data$Year, title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Total Affected Area (Ha)")) %>%
      hc_add_series(
        name = input$district_select,
        data = plot_data$Total_Affected_Area,
        color = "#4caf50"
      ) %>%
      hc_tooltip(
        valueDecimals = 0,
        shared = TRUE,
        crosshairs = TRUE,
        pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:,.0f} Ha</b><br/>'
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # Trend summary
  output$trend_summary <- renderPrint({
    current_data <- filtered_data()
    summary(current_data$Affected_Area_Ha)
  })
  
  # ================= Data Explorer Tab Content =================
  
  # Data table
  output$data_table <- renderDT({
    waiter$show()
    on.exit(waiter$hide())
    
    datatable(
      data,
      extensions = c('Buttons', 'Scroller', 'Responsive'),
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', className = 'btn-sm'),
          list(extend = 'csv', className = 'btn-sm'),
          list(extend = 'excel', className = 'btn-sm'),
          list(extend = 'pdf', className = 'btn-sm')
        ),
        scrollX = TRUE,
        scrollY = "500px",
        scroller = TRUE,
        pageLength = 10,
        responsive = TRUE,
        autoWidth = TRUE,
        language = list(
          search = "Filter:",
          paginate = list(
            `first` = "First", `last` = "Last", `next` = "Next", `previous` = "Previous"
          )
        )
      ),
      class = "cell-border stripe hover",
      rownames = FALSE,
      filter = 'top'
    ) %>%
      formatRound(columns = 'Affected_Area_Ha', digits = 2)
  })
}

shinyApp(ui, server)