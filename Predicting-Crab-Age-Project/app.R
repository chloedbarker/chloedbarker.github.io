library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(data.table)
library(DT)
library(dplyr)
library(olsrr)

options(scipen = 999)

#### Load Data ####
data <- fread(file.path("www", "data.csv"))
setDT(data)

#### Data Cleanup ####
data$id <- NULL

#### Pre Model (Replace Missing) ####
# Sex is Character (Make Factor)
data <- data %>%
  mutate(Sex = factor(Sex, levels = c("I", "F", "M")))

#### Check Missing ####
# # Missing Values (NA)
# data %>%
#   filter(if_any(everything(), is.na)) # No NA
# 
# # Zero Values (0)
# data %>%
#   filter(if_any(where(is.numeric), ~ . == 0)) # Diameter and Height

#### Height
# Split data
train_nonzero <- data %>% filter(Height != 0)
train_zero <- data %>% filter(Height == 0)

# Fit
model <- lm(Height ~ ., data = train_nonzero)

# Predict
predict <- predict(model, newdata = train_zero)

# Replace 0 Height values
data$Height[data$Height == 0] <- predict

#### Diameter
# Split data
train_nonzero <- data %>% filter(Diameter != 0)
train_zero <- data %>% filter(Diameter == 0)

# Fit
model <- lm(Diameter ~ ., data = train_nonzero)

# Predict
predict <- predict(model, newdata = train_zero)

# Replace 0 Diameter values
data$Diameter[data$Diameter == 0] <- predict

# Round
num_cols <- names(data)[sapply(data, is.numeric)]
data[ , (num_cols) := lapply(.SD, function(x) round(x, 2)), .SDcols = num_cols]

# Theme
theme <- bs_theme(
  bootswatch = "cerulean",
  base_font = "calibri"
) %>% 
  bs_add_variables(
    "primary" = "#0055A4",
    "danger"  = "#E31B23",
    "success" = "#0055A4",
    "info"    = "#0055A4"
  )
theme <- bs_add_rules(theme, "
  .navbar-brand {
    margin-left: 0px; 
    display: block; 
    text-align: center;
    font-size: 24px; 
    font-weight: bold;
  }
  .nav-item {
    margin-left: 25px;
    font-size: 20px;
  }
")

# UI -------------------------------------------------------------------------
ui <- tagList(
  tags$head(
    tags$style(HTML("
      table.dataTable { width: 100% !important; }
    "))
  ),
  page_navbar(
    theme = theme,
    title = tags$span(
      tags$img(src = "smu_logo.png", height = "40px",
               style = "display:inline; margin-right:2px;"),
      "Crab Analysis"
    ),
    window_title = "Crab Analysis",
    sidebar = sidebar(
      tags$div("Filters", style = "text-align: center; font-weight: bold; font-size: 24px;"),
      selectInput("input_sex", "Sex:", 
                  choices = c("All", sort(unique(data$Sex))), 
                  selected = "All"),
      sliderInput("input_age", "Age:",
                  min = min(data$Age, na.rm = TRUE),
                  max = max(data$Age, na.rm = TRUE),
                  value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE)),
                  step = 2
      ),
      sliderInput("input_weight", "Weight:",
                  min = min(data$Weight, na.rm = TRUE),
                  max = max(data$Weight, na.rm = TRUE),
                  value = c(min(data$Weight, na.rm = TRUE), max(data$Weight, na.rm = TRUE)),
                  step = 5
      ),
      hr(),
      actionButton("reset_filters", "Reset Page", class = "btn-danger")
    ),
    nav_panel(
      title = "Overview",
      
      # Row 1: Value Boxes
      fluidRow(
        column(width = 4,
               value_box(
                 title = "Average Age",
                 value = uiOutput("avg_age"),
                 showcase = icon("hourglass"),
                 theme = "bg-primary"
               )
        ),
        column(width = 4,
               value_box(
                 title = "Average Weight",
                 value = uiOutput("avg_weight"),
                 showcase = icon("weight-scale"),
                 theme = "bg-success"
               )
        ),
        column(width = 4,
               value_box(
                 title = "Average Height",
                 value = uiOutput("avg_height"),
                 showcase = icon("ruler-vertical"),
                 theme = "bg-info"
               )
        )
      ),
      # Row 2: Plots
      fluidRow(
        column(width = 6,
               card(fill = TRUE,
                    full_screen = TRUE,
                    card_header("Age Histogram"),
                    card_body(fill = TRUE, plotlyOutput("hist_plot"))
               )
        ),
        column(width = 6,
               card(fill = TRUE,
                    full_screen = TRUE,
                    card_header("Age Box Plot"),
                    card_body(fill = TRUE, plotlyOutput("box_plot"))
               )
        )
      ),
      # Row 3: Data Table and Model Metrics
      fluidRow(
        column(width = 6,
               card(
                    card_header("Crab Data"),
                    card_body(
                      DTOutput("table", fill = FALSE))
                    )
        ),
        column(width = 3,
               card(
                    card_header("Model 1 Summary"),
                    textOutput("mae_unlog_text_1"),
                    card_body(
                      DTOutput("model_summary_1", fill = FALSE))
                    )
        ),
        column(width = 3,
               card(
                    card_header("Model 2 Summary"),
                    textOutput("mae_unlog_text_2"),
                    card_body(
                      DTOutput("model_summary_2", fill = FALSE))
                    )
               )
        )
      )
    )
  )

# Server ---------------------------------------------------------------------
server <- function(input, output, session) {
  reactive_data <- reactive({
    data
  })
  
  filtered <- reactive({
    req(reactive_data())
    dt <- reactive_data()
    if (input$input_sex != "All") {
      dt <- dt[Sex == input$input_sex]
    }
    dt <- dt[Age >= input$input_age[1] & Age <= input$input_age[2] &
               Weight >= input$input_weight[1] & Weight <= input$input_weight[2]]
    list(
      data = dt,
      avg_age = round(mean(dt$Age, na.rm = TRUE), 1),
      avg_weight = round(mean(dt$Weight, na.rm = TRUE), 2),
      avg_height = round(mean(dt$Height, na.rm = TRUE), 2)
    )
  })
  
  # Reset Button
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "input_sex", selected = "All")
    updateSliderInput(session, "input_age", value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE)))
    updateSliderInput(session, "input_weight", value = c(min(data$Weight, na.rm = TRUE), max(data$Weight, na.rm = TRUE)))
  })
  
  output$avg_age <- renderUI({
    filtered()$avg_age
  })
  
  output$avg_weight <- renderUI({
    filtered()$avg_weight
  })
  
  output$avg_height <- renderUI({
    filtered()$avg_height
  })
  
  output$table <- renderDataTable({
    datatable(
      filtered()$data,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = 'tip',
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      )
    )
  })
  
  output$hist_plot <- renderPlotly({
    req(filtered())
    df <- filtered()$data
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No Data Available", size = 6, color = "#E31B23") +
        theme_void()
      return(ggplotly(p))
    }
    p <- ggplot(df, aes(x = Age)) +
      geom_histogram(fill = "#E31B23", bins = 30, color = "gray", na.rm = TRUE) +
      theme_minimal(base_size = 16) +
      labs(x = "Age", y = "Count")
    ggplotly(p)
  })
  
  output$box_plot <- renderPlotly({
    req(filtered())
    df <- filtered()$data
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No Data Available", size = 6, color = "#E31B23") +
        theme_void()
      return(ggplotly(p))
    }
    p <- ggplot(df, aes(y = Age)) +
      geom_boxplot(fill = "#0055A4", color = "gray", na.rm = TRUE) +
      theme_minimal(base_size = 16) +
      labs(y = "Age")
    ggplotly(p)
  })
  
 
  #### LM Models ####
  model1 <- reactive({
    df <- filtered()$data
    req(nrow(df) > 1)
    df <- df %>%
      mutate(
        log_Age = log(Age),
        Sex       = factor(Sex, levels = c("I","F","M")),
        Sex_Female = ifelse(Sex == "F", 1, 0),
        Sex_Male  = ifelse(Sex == "M", 1, 0),
        BodySize  = Length * Diameter,
        Sex_Female_BodySize = Sex_Female * BodySize,
        Sex_Male_BodySize = Sex_Male * BodySize,
        height2   = Height^2,
        weight2   = Weight^2,
        shell2    = `Shell Weight`^2,
        shucked3  = `Shucked Weight`^3,
        shellshuck= `Shell Weight`   * `Shucked Weight`,
      )
    lm(log_Age ~ Sex + BodySize + Sex_Female_BodySize + Sex_Male_BodySize +  height2 + weight2 + shell2 + shucked3 + shellshuck,  data = df)
  })
  
  # OLS summary for model1
  ols1 <- reactive({ req(model1()); ols_regress(model1()) })
  
  model_summary_df_1 <- reactive({
    o <- ols1()[c("r","rsq","adjr","prsq","mae","rmse","mse","cv","aic","sbc")]
    names(o) <- c("R","R‑Squared","Adj. R‑Squared","Pred R‑Squared","MAE","RMSE","MSE","Coef. Var","AIC","SBC")
    data.frame(Statistic = names(o), Value = round(unlist(o), 3), row.names = NULL)
  })
  
  # Regular MAE Scale
  mae_unlog_1 <- reactive({
    round(exp(ols1()$mae), 3)
  })
  
  output$mae_unlog_text_1 <- renderText({
    if (input$input_sex != "All") {
      "Please select 'All' for Sex filter to view MAE."
    } else {
      paste("MAE (original scale):", mae_unlog_1())
    }
  })
  
  output$model_summary_1 <- renderDataTable({
    if (input$input_sex != "All") {
      msg <- data.frame(
        Statistic = "Message",
        Value     = "Please select 'All' for Sex filter to view model metrics."
      )
      return(
        datatable(
          msg,
          rownames = FALSE,
          options  = list(
            dom        = 't',
            paging     = FALSE,
            ordering   = FALSE,
            autoWidth  = TRUE,
            columnDefs = list(list(className = 'dt-center', targets = '_all'))
          )
        )
      )
    }
    
    datatable(
      model_summary_df_1(),
      rownames = FALSE,
      options = list(
        dom        = 't',
        autoWidth  = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      )
    )  %>%
      formatStyle(
        "Statistic",
        target = "cell",
        backgroundColor = styleEqual("MAE", "#fff3cd"),
        fontWeight      = styleEqual("MAE", "bold")
      )%>%                                 
      formatStyle(
        "Value", "Statistic",
        target = "cell",
        backgroundColor = styleEqual("MAE", "#fff3cd"),
        fontWeight      = styleEqual("MAE", "bold")
      )
  })
  
  model2 <- reactive({
    df <- filtered()$data %>%
      mutate(
        log_Age = log(Age),
        Sex       = factor(Sex, levels = c("I","F","M")),
        height2   = Height^2,
        weight2   = Weight^2,
        shucked2  = `Shucked Weight`^2,
        viscera2  = `Viscera Weight`^2,
        shell2    = `Shell Weight`^2,
        shell3    = `Shell Weight`^3,
        dh_int    = Diameter * Height,
        shellww   = `Shell Weight` * Weight,
        shellshuck= `Shell Weight`   * `Shucked Weight`,
        shellvisc = `Shell Weight`   * `Viscera Weight`,
        Shell_Density = `Shell Weight` / Weight,
        
      )
    lm(log_Age ~ Sex + `Shucked Weight` + Diameter + Height + Weight + `Shell Weight` + `Viscera Weight` + height2 + weight2 + shucked2 + 
         viscera2 + shell2 + shell3 + dh_int + shellww + shellshuck + shellvisc + Shell_Density, data = df)
  })
  
  # OLS summary for model2
  ols2 <- reactive({ req(model2()); ols_regress(model2()) })
  
  model_summary_df_2 <- reactive({
    o <- ols2()[c("r","rsq","adjr","prsq","mae","rmse","mse","cv","aic","sbc")]
    names(o) <- c("R","R‑Squared","Adj. R‑Squared","Pred R‑Squared","MAE","RMSE","MSE","Coef. Var","AIC","SBC")
    data.frame(Statistic = names(o), Value = round(unlist(o), 3), row.names = NULL)
  })
  
  # Regular MAE Scale
  mae_unlog_2 <- reactive({
    round(exp(ols2()$mae), 3)
  })
  
  output$mae_unlog_text_2 <- renderText({
    if (input$input_sex != "All") {
      "Please select 'All' for Sex filter to view MAE."
    } else {
      paste("MAE (original scale):", mae_unlog_2())
    }
  })
  
  output$model_summary_2 <- renderDataTable({
    if (input$input_sex != "All") {
      msg <- data.frame(
        Statistic = "Message",
        Value     = "Please select 'All' for Sex filter to view model metrics."
      )
      return(
        datatable(
          msg,
          rownames = FALSE,
          options  = list(
            dom        = 't',
            paging     = FALSE,
            ordering   = FALSE,
            autoWidth  = TRUE,
            columnDefs = list(list(className = 'dt-center', targets = '_all'))
          )
        )
      )
    }
    datatable(
      model_summary_df_2(),
      rownames = FALSE,
      options = list(
        dom        = 't',
        autoWidth  = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      )
    )  %>%
      formatStyle(
        "Statistic",
        target = "cell",
        backgroundColor = styleEqual("MAE", "#fff3cd"),
        fontWeight      = styleEqual("MAE", "bold")
      )%>%                                 
      formatStyle(
        "Value", "Statistic",
        target = "cell",
        backgroundColor = styleEqual("MAE", "#fff3cd"),
        fontWeight      = styleEqual("MAE", "bold")
      )
  })
  
}

shinyApp(ui, server)