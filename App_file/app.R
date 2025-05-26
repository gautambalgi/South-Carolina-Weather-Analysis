library(shiny)
library(ggplot2)
library(randomForest)
library(caret)
library(tidyverse)

# Load Data
dataf <- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/static_house_info.parquet")
gclimate_data <- read_csv("C:/Users/siddh/OneDrive/Documents/IDS_Submission/data/julyweather_data_updated1.csv")
july_data <- read_csv("C:/Users/siddh/OneDrive/Documents/IDS_Submission/data/july_data.csv")

# Data Preparation
july_data$total_consumption <- rowSums(july_data[, 2:43])
july_data <- july_data[,-2:-43]
july_data$bldg_id <- july_data$building_id
july_data <- july_data[,-1]
merged_df_ener_hou <- merge.data.frame(july_data, dataf, by = 'bldg_id', sort = TRUE, all = FALSE)
merged_df_ener_hou <- merged_df_ener_hou[merged_df_ener_hou$total_consumption >= 0, ]
merged_df_ener_hou$in.cooling_setpoint <- as.numeric(gsub("F", "", merged_df_ener_hou$in.cooling_setpoint))
merged_df_ener_hou$in.heating_setpoint <- as.numeric(gsub("F", "", merged_df_ener_hou$in.heating_setpoint))

# Sample Data for Random Forest
set.seed(123)
train_indices <- createDataPartition(merged_df_ener_hou$total_consumption, p = 0.7, list = FALSE)
training <- merged_df_ener_hou[train_indices, ]
testing <- merged_df_ener_hou[-train_indices, ]
sampled_data <- training[sample(1:nrow(training), size = 50000), ]

# Train Random Forest Model
rf_model <- randomForest(
  total_consumption ~ in.heating_setpoint + in.cooling_setpoint + in.sqft + in.bedrooms,
  data = sampled_data,
  ntree = 200,
  mtry = 2,
  importance = TRUE
)
rf_predictions <- predict(rf_model, newdata = testing)

# UI
ui <- fluidPage(
  titlePanel("Energy Consumption Analysis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Interactive plots based on energy data.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("histogramPlot")),
        tabPanel("Boxplot", plotOutput("boxplotPlot")),
        tabPanel("Time Series (65, 121)", plotOutput("timeSeriesPlot1")),
        tabPanel("Time Series (500, 504)", plotOutput("timeSeriesPlot2")),
        tabPanel("Predicted vs Actual", plotOutput("scatterPlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$histogramPlot <- renderPlot({
    ggplot(merged_df_ener_hou, aes(x = total_consumption)) +
      geom_histogram(fill = "blue", bins = 30) +
      labs(title = "Distribution of Total Consumption", x = "Total Consumption", y = "Frequency") +
      theme_minimal()
  })
  
  output$boxplotPlot <- renderPlot({
    ggplot(merged_df_ener_hou, aes(y = total_consumption)) +
      geom_boxplot(fill = "cyan") +
      labs(title = "Boxplot of Total Consumption", y = "Total Consumption") +
      theme_minimal()
  })
  
  output$timeSeriesPlot1 <- renderPlot({
    building_ids <- c(65, 121)
    filtered_data <- merged_df_ener_hou[merged_df_ener_hou$bldg_id %in% building_ids, ]
    filtered_data <- filtered_data[order(filtered_data$time), ]
    ggplot(filtered_data, aes(x = time, y = total_consumption, color = factor(bldg_id))) +
      geom_line() +
      labs(title = "Time Series for Building IDs (65, 121)",
           x = "Time", y = "Total Consumption", color = "Building ID") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$timeSeriesPlot2 <- renderPlot({
    building_ids <- c(500, 504)
    filtered_data <- merged_df_ener_hou[merged_df_ener_hou$bldg_id %in% building_ids, ]
    filtered_data <- filtered_data[order(filtered_data$time), ]
    ggplot(filtered_data, aes(x = time, y = total_consumption, color = factor(bldg_id))) +
      geom_line() +
      labs(title = "Time Series for Building IDs (500, 504)",
           x = "Time", y = "Total Consumption", color = "Building ID") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(testing, aes(x = rf_predictions, y = total_consumption)) +
      geom_point(alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "Predicted vs Actual Total Consumption",
           x = "Predicted Total Consumption", y = "Actual Total Consumption") +
      theme_minimal()
  })
}

# Run App
shinyApp(ui = ui, server = server)
