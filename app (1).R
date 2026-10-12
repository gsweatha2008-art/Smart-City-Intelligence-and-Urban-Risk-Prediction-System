# Smart City Intelligence and Urban Risk Prediction System
# Save as app.R and run in RStudio

required_packages <- c(
  "shiny", "shinydashboard", "dplyr", "ggplot2",
  "plotly", "DT", "lubridate", "knitr", "knitr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Please install these packages first: ",
      paste(missing_packages, collapse = ", "),
      "\nRun: install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)

set.seed(123)

# Sample demonstration data. Replace with real, validated city data for real-world use.
make_sample_data <- function(n = 365) {
  dates <- seq.Date(Sys.Date() - n + 1, Sys.Date(), by = "day")
  n <- length(dates)
  data.frame(
    Date = dates,
    Area = sample(c("Central", "North", "South", "East", "West"), n, TRUE),
    Traffic_Count = pmax(50, round(rnorm(n, 720, 180))),
    Avg_Speed_kmh = pmin(70, pmax(5, round(rnorm(n, 32, 8), 1))),
    AQI = pmin(350, pmax(10, round(rnorm(n, 105, 38)))),
    PM25 = pmax(3, round(rnorm(n, 42, 15), 1)),
    Crime_Count = pmax(0, rpois(n, lambda = 4)),
    Water_Level = pmax(0.2, round(rnorm(n, 2.1, 0.6), 2)),
    stringsAsFactors = FALSE
  )
}

calculate_risk <- function(data, traffic_weight = 35, pollution_weight = 35,
                           crime_weight = 30) {
  # Normalize each measure to a 0-100 scale; thresholds are illustrative.
  traffic_score <- pmin(100, pmax(0, (data$Traffic_Count - 200) / 10))
  pollution_score <- pmin(100, pmax(0, (data$AQI - 20) / 2.3))
  crime_score <- pmin(100, pmax(0, data$Crime_Count * 10))
  weight_sum <- traffic_weight + pollution_weight + crime_weight
  if (weight_sum <= 0) weight_sum <- 1

  data %>%
    mutate(
      Traffic_Score = traffic_score,
      Pollution_Score = pollution_score,
      Crime_Score = crime_score,
      Risk_Score = round(
        (traffic_score * traffic_weight +
           pollution_score * pollution_weight +
           crime_score * crime_weight) / weight_sum,
        1
      ),
      Risk_Level = case_when(
        Risk_Score >= 70 ~ "High",
        Risk_Score >= 40 ~ "Medium",
        TRUE ~ "Low"
      )
    )
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Smart City Intelligence", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Traffic Analysis", tabName = "traffic", icon = icon("car")),
      menuItem("Pollution Monitoring", tabName = "pollution", icon = icon("cloud")),
      menuItem("Crime Statistics", tabName = "crime", icon = icon("shield")),
      menuItem("Risk Prediction", tabName = "risk", icon = icon("exclamation-triangle")),
      menuItem("Data & Reports", tabName = "reports", icon = icon("download"))
    ),
    hr(),
    fileInput("file", "Upload city CSV data", accept = ".csv"),
    helpText("CSV upload replaces the current dataset. Expected columns: Date, Area, Traffic_Count, Avg_Speed_kmh, AQI, PM25, Crime_Count, Water_Level."),
    hr(),
    h4("Add City Information"),
    dateInput("entry_date", "Date", value = Sys.Date()),
    textInput("entry_area", "Area Name", value = "Central", placeholder = "e.g. Central"),
    numericInput("entry_traffic", "Traffic Count", value = 500, min = 0, step = 1),
    numericInput("entry_speed", "Average Speed (km/h)", value = 30, min = 0, max = 200, step = 1),
    numericInput("entry_aqi", "Air Quality Index (AQI)", value = 100, min = 0, max = 500, step = 1),
    numericInput("entry_pm25", "PM2.5", value = 35, min = 0, step = 0.1),
    numericInput("entry_crime", "Crime Count", value = 2, min = 0, step = 1),
    numericInput("entry_water", "Water Level", value = 2, min = 0, step = 0.1),
    actionButton("add_city_data", "Add City Data", icon = icon("plus"), class = "btn-success"),
    br(), br(),
    helpText("Manual entries are added to the current data for this app session only. They are not saved after the app session ends."),
    actionButton("reset", "Reset to sample data", icon = icon("refresh"))
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f7fb; }
      .small-box { border-radius: 8px; }
      .box { border-radius: 8px; }
      .main-header .logo { font-weight: bold; }
      .content { padding-top: 18px; }
    "))),
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("traffic_box", width = 3),
          valueBoxOutput("aqi_box", width = 3),
          valueBoxOutput("crime_box", width = 3),
          valueBoxOutput("risk_box", width = 3)
        ),
        fluidRow(
          box(title = "Urban Risk Trend", status = "primary", solidHeader = TRUE,
              width = 8, plotlyOutput("risk_trend", height = 300)),
          box(title = "Risk Level Distribution", status = "warning", solidHeader = TRUE,
              width = 4, plotlyOutput("risk_pie", height = 300))
        ),
        fluidRow(
          box(title = "Area-wise City Indicators", status = "info", solidHeader = TRUE,
              width = 12, plotlyOutput("area_chart", height = 330))
        )
      ),
      tabItem(
        tabName = "traffic",
        fluidRow(
          box(title = "Traffic Analysis", status = "primary", solidHeader = TRUE,
              width = 8, plotlyOutput("traffic_trend", height = 350)),
          box(title = "Traffic Scenario", status = "info", solidHeader = TRUE,
              width = 4,
              sliderInput("traffic_change", "Simulated traffic change (%)",
                          min = -50, max = 100, value = 10, step = 5),
              verbatimTextOutput("traffic_simulation"))
        ),
        fluidRow(
          box(title = "Traffic Data", status = "primary", solidHeader = TRUE,
              width = 12, DTOutput("traffic_table"))
        )
      ),
      tabItem(
        tabName = "pollution",
        fluidRow(
          box(title = "Air Quality Index Over Time", status = "primary",
              solidHeader = TRUE, width = 8, plotlyOutput("aqi_trend", height = 350)),
          box(title = "Pollution Indicator Summary", status = "info",
              solidHeader = TRUE, width = 4, plotlyOutput("pollution_bar", height = 350))
        ),
        fluidRow(
          box(title = "Pollution Data", status = "primary", solidHeader = TRUE,
              width = 12, DTOutput("pollution_table"))
        )
      ),
      tabItem(
        tabName = "crime",
        fluidRow(
          box(title = "Daily Crime Counts", status = "primary", solidHeader = TRUE,
              width = 8, plotlyOutput("crime_trend", height = 350)),
          box(title = "Crime by Area", status = "warning", solidHeader = TRUE,
              width = 4, plotlyOutput("crime_area", height = 350))
        ),
        fluidRow(
          box(title = "Crime Statistics", status = "primary", solidHeader = TRUE,
              width = 12, DTOutput("crime_table"))
        )
      ),
      tabItem(
        tabName = "risk",
        fluidRow(
          box(title = "Risk Scoring Weights", status = "primary", solidHeader = TRUE,
              width = 4,
              sliderInput("traffic_weight", "Traffic weight", min = 0, max = 100, value = 35),
              sliderInput("pollution_weight", "Pollution weight", min = 0, max = 100, value = 35),
              sliderInput("crime_weight", "Crime weight", min = 0, max = 100, value = 30),
              helpText("Weights are normalized automatically. Scores are illustrative, not official city safety ratings.")),
          box(title = "Risk Score by Area", status = "warning", solidHeader = TRUE,
              width = 8, plotlyOutput("risk_area", height = 350))
        ),
        fluidRow(
          box(title = "Highest-Risk Records", status = "danger", solidHeader = TRUE,
              width = 12, DTOutput("high_risk_table"))
        )
      ),
      tabItem(
        tabName = "reports",
        fluidRow(
          box(title = "Data Quality", status = "primary", solidHeader = TRUE,
              width = 6, verbatimTextOutput("data_summary")),
          box(title = "Export Reports", status = "success", solidHeader = TRUE,
              width = 6,
              downloadButton("download_csv", "Download analyzed CSV"),
              br(), br(),
              downloadButton("download_report", "Download HTML report"),
              br(), br(),
              helpText("The HTML report can be opened in a browser and printed or saved as PDF."))
        ),
        fluidRow(
          box(title = "Full Dataset", status = "primary", solidHeader = TRUE,
              width = 12, DTOutput("full_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactiveVal(make_sample_data())

  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      uploaded <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      required <- c("Date", "Area", "Traffic_Count", "Avg_Speed_kmh",
                    "AQI", "PM25", "Crime_Count", "Water_Level")
      missing <- setdiff(required, names(uploaded))
      if (length(missing)) {
        showNotification(
          paste("CSV is missing columns:", paste(missing, collapse = ", ")),
          type = "error", duration = 8
        )
      } else {
        uploaded$Date <- as.Date(uploaded$Date)
        numeric_cols <- setdiff(required, c("Date", "Area"))
        for (nm in numeric_cols) uploaded[[nm]] <- suppressWarnings(as.numeric(uploaded[[nm]]))
        uploaded <- uploaded[complete.cases(uploaded[, required]), required, drop = FALSE]
        if (nrow(uploaded) == 0) stop("No complete rows found after checking required columns.")
        raw_data(uploaded)
        showNotification("CSV loaded successfully.", type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Could not read CSV:", conditionMessage(e)),
                       type = "error", duration = 8)
    })
  })

  observeEvent(input$add_city_data, {
    area_name <- trimws(if (is.null(input$entry_area)) "" else input$entry_area)
    if (!nzchar(area_name)) {
      showNotification("Please enter an area name.", type = "error")
      return()
    }

    values <- c(input$entry_traffic, input$entry_speed, input$entry_aqi,
                input$entry_pm25, input$entry_crime, input$entry_water)
    if (any(vapply(values, function(x) is.null(x) || length(x) != 1 ||
                   !is.finite(x) || is.na(x), logical(1)))) {
      showNotification("Please enter valid numeric values in all city fields.", type = "error")
      return()
    }

    new_row <- data.frame(
      Date = as.Date(input$entry_date),
      Area = area_name,
      Traffic_Count = as.numeric(input$entry_traffic),
      Avg_Speed_kmh = as.numeric(input$entry_speed),
      AQI = as.numeric(input$entry_aqi),
      PM25 = as.numeric(input$entry_pm25),
      Crime_Count = as.numeric(input$entry_crime),
      Water_Level = as.numeric(input$entry_water),
      stringsAsFactors = FALSE
    )

    updated <- bind_rows(raw_data(), new_row)
    raw_data(updated)
    showNotification(
      paste("City information added for", area_name, "- dashboard updated."),
      type = "message"
    )
  })

  observeEvent(input$reset, {
    raw_data(make_sample_data())
    showNotification("Sample demonstration data restored.", type = "message")
  })

  data <- reactive({
    req(raw_data())
    calculate_risk(raw_data(), input$traffic_weight,
                   input$pollution_weight, input$crime_weight)
  })

  output$traffic_box <- renderValueBox({
    d <- data()
    valueBox(format(round(mean(d$Traffic_Count, na.rm = TRUE)), big.mark = ","),
             "Average Traffic Count", icon = icon("car"), color = "blue")
  })
  output$aqi_box <- renderValueBox({
    valueBox(round(mean(data()$AQI, na.rm = TRUE), 1), "Average AQI",
             icon = icon("cloud"), color = "yellow")
  })
  output$crime_box <- renderValueBox({
    valueBox(round(sum(data()$Crime_Count, na.rm = TRUE)), "Recorded Crime Events",
             icon = icon("shield"), color = "red")
  })
  output$risk_box <- renderValueBox({
    d <- data()
    high <- sum(d$Risk_Level == "High", na.rm = TRUE)
    valueBox(high, "High-Risk Records", icon = icon("exclamation-triangle"),
             color = if (high > 0) "red" else "green")
  })

  output$risk_trend <- renderPlotly({
    d <- data() %>% group_by(Date) %>% summarise(Risk_Score = mean(Risk_Score), .groups = "drop")
    p <- ggplot(d, aes(Date, Risk_Score)) +
      geom_line(linewidth = 0.8, color = "#2471A3") +
      geom_smooth(method = "loess", se = FALSE, color = "#E67E22", linewidth = 0.7) +
      labs(x = NULL, y = "Average risk score", caption = "Orange line shows a smoothed historical trend.") +
      theme_minimal()
    ggplotly(p)
  })

  output$risk_pie <- renderPlotly({
    d <- data() %>% count(Risk_Level)
    plot_ly(d, labels = ~Risk_Level, values = ~n, type = "pie",
            textinfo = "label+percent", hovertemplate = "%{label}: %{value}<extra></extra>")
  })

  output$area_chart <- renderPlotly({
    d <- data() %>% group_by(Area) %>%
      summarise(Traffic = mean(Traffic_Count), AQI = mean(AQI),
                Crimes = mean(Crime_Count), Risk = mean(Risk_Score), .groups = "drop")
    p <- ggplot(d, aes(x = Area, y = Risk, text = paste0(
      "Area: ", Area, "<br>Risk: ", round(Risk, 1),
      "<br>Average traffic: ", round(Traffic),
      "<br>Average AQI: ", round(AQI, 1),
      "<br>Average daily crimes: ", round(Crimes, 1)))) +
      geom_col(fill = "#2874A6") + labs(x = NULL, y = "Average risk score") + theme_minimal()
    ggplotly(p, tooltip = "text")
  })

  output$traffic_trend <- renderPlotly({
    d <- data() %>% group_by(Date) %>% summarise(
      Traffic = mean(Traffic_Count), Speed = mean(Avg_Speed_kmh), .groups = "drop")
    p <- ggplot(d, aes(Date, Traffic)) + geom_line(color = "#2874A6") +
      geom_smooth(se = FALSE, color = "#E67E22") +
      labs(x = NULL, y = "Traffic count", title = "Historical traffic trend") + theme_minimal()
    ggplotly(p)
  })
  output$traffic_simulation <- renderPrint({
    d <- data()
    change <- input$traffic_change
    current <- mean(d$Traffic_Count, na.rm = TRUE)
    simulated <- current * (1 + change / 100)
    cat("Average observed traffic:", round(current), "\n")
    cat("Scenario change:", change, "%\n")
    cat("Simulated traffic count:", round(simulated), "\n")
    cat("This is a what-if calculation, not a live traffic forecast.")
  })
  output$traffic_table <- renderDT({
    datatable(data() %>% select(Date, Area, Traffic_Count, Avg_Speed_kmh, Risk_Score, Risk_Level),
              options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$aqi_trend <- renderPlotly({
    d <- data()
    p <- ggplot(d, aes(Date, AQI, color = Area)) +
      geom_point(alpha = 0.45, size = 1.5) +
      geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "black") +
      labs(x = NULL, y = "Air Quality Index", color = "Area") + theme_minimal()
    ggplotly(p)
  })
  output$pollution_bar <- renderPlotly({
    d <- data() %>% group_by(Area) %>%
      summarise(AQI = mean(AQI), PM2.5 = mean(PM25), .groups = "drop")
    plot_ly(d, x = ~Area) %>%
      add_bars(y = ~AQI, name = "Average AQI") %>%
      add_bars(y = ~`PM2.5`, name = "Average PM2.5") %>%
      layout(barmode = "group", yaxis = list(title = "Average reading"))
  })
  output$pollution_table <- renderDT({
    datatable(data() %>% select(Date, Area, AQI, PM25, Risk_Score, Risk_Level),
              options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$crime_trend <- renderPlotly({
    d <- data() %>% group_by(Date) %>% summarise(Crimes = sum(Crime_Count), .groups = "drop")
    p <- ggplot(d, aes(Date, Crimes)) + geom_line(color = "#922B21") +
      geom_smooth(se = FALSE, color = "#E67E22") +
      labs(x = NULL, y = "Crime count", title = "Historical reported crime counts") +
      theme_minimal()
    ggplotly(p)
  })
  output$crime_area <- renderPlotly({
    d <- data() %>% group_by(Area) %>% summarise(Crimes = sum(Crime_Count), .groups = "drop")
    p <- ggplot(d, aes(reorder(Area, Crimes), Crimes)) +
      geom_col(fill = "#922B21") + coord_flip() +
      labs(x = NULL, y = "Total recorded crime events") + theme_minimal()
    ggplotly(p)
  })
  output$crime_table <- renderDT({
    datatable(data() %>% select(Date, Area, Crime_Count, Risk_Score, Risk_Level),
              options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$risk_area <- renderPlotly({
    d <- data() %>% group_by(Area) %>% summarise(
      Risk = mean(Risk_Score), Traffic = mean(Traffic_Score),
      Pollution = mean(Pollution_Score), Crime = mean(Crime_Score), .groups = "drop")
    p <- ggplot(d, aes(reorder(Area, Risk), Risk, fill = Risk)) +
      geom_col() + coord_flip() +
      scale_fill_gradient(low = "#58D68D", high = "#C0392B") +
      labs(x = NULL, y = "Average risk score (0-100)", fill = "Risk") + theme_minimal()
    ggplotly(p)
  })
  output$high_risk_table <- renderDT({
    d <- data() %>% filter(Risk_Level == "High") %>%
      arrange(desc(Risk_Score)) %>%
      select(Date, Area, Traffic_Count, AQI, Crime_Count, Risk_Score, Risk_Level)
    datatable(d, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$data_summary <- renderPrint({
    d <- data()
    cat("Rows:", nrow(d), "\n")
    cat("Date range:", as.character(min(d$Date)), "to", as.character(max(d$Date)), "\n")
    cat("Areas:", paste(sort(unique(d$Area)), collapse = ", "), "\n")
    cat("Missing values in analyzed data:", sum(is.na(d)), "\n")
    cat("Average risk score:", round(mean(d$Risk_Score), 1), "/ 100\n")
    cat("\nNote: Built-in sample data is simulated for demonstration.")
  })

  output$full_table <- renderDT({
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("smart_city_analysis_", Sys.Date(), ".csv"),
    content = function(file) write.csv(data(), file, row.names = FALSE)
  )

  output$download_report <- downloadHandler(
    filename = function() paste0("smart_city_report_", Sys.Date(), ".html"),
    content = function(file) {
      d <- data()
      area_summary <- d %>% group_by(Area) %>% summarise(
        Records = n(),
        Avg_Traffic = round(mean(Traffic_Count), 1),
        Avg_AQI = round(mean(AQI), 1),
        Total_Crimes = sum(Crime_Count),
        Avg_Risk = round(mean(Risk_Score), 1),
        .groups = "drop"
      )
      risk_summary <- d %>% count(Risk_Level)
      con <- base::file(file, open = "wt", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)
      writeLines(c(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>Smart City Intelligence Report</title>",
        "<style>body{font-family:Arial;margin:35px;color:#222}table{border-collapse:collapse;width:100%;margin-bottom:24px}th,td{border:1px solid #ccc;padding:8px;text-align:left}th{background:#eaf2f8}h1,h2{color:#1f618d}</style>",
        "</head><body>",
        "<h1>Smart City Intelligence and Urban Risk Report</h1>",
        paste0("<p>Generated: ", Sys.time(), "</p>"),
        "<p>This report summarizes the dataset currently loaded into the app. Risk scores use illustrative, configurable rules and are not official safety ratings.</p>",
        paste0("<p><b>Records:</b> ", nrow(d),
               " &nbsp; <b>Average risk score:</b> ", round(mean(d$Risk_Score), 1),
               " &nbsp; <b>Average AQI:</b> ", round(mean(d$AQI), 1), "</p>"),
        "<h2>Risk Level Distribution</h2>",
        paste(capture.output(print(knitr::kable(risk_summary, format = "html"))), collapse = "\n"),
        "<h2>Area Summary</h2>",
        paste(capture.output(print(knitr::kable(area_summary, format = "html"))), collapse = "\n"),
        "<h2>Recommendations</h2><ul>",
        "<li>Review areas with consistently high risk scores.</li>",
        "<li>Compare traffic and pollution patterns across dates.</li>",
        "<li>Validate source data and thresholds before making operational decisions.</li>",
        "</ul></body></html>"
      ), con)
    }
  )
}

shinyApp(ui, server)
