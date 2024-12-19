library(shiny)
library(tidyverse)
library(cluster)
library(factoextra)

# Load data
white_wine <- read.csv("winequality-white.csv", sep = ";")
white_wine2 <- white_wine[, 1:11]  # Features only
quality <- white_wine$quality

# PCA and Clustering
pca_result <- princomp(white_wine2, cor = TRUE)
pca_scores <- as.data.frame(pca_result$scores[, 1:4])
set.seed(123)
kmeans_result <- kmeans(pca_scores, centers = 3)
white_wine$Cluster <- as.factor(kmeans_result$cluster)
pca_scores$Cluster <- as.factor(kmeans_result$cluster)

# UI
ui <- fluidPage(
  titlePanel("White Wine Quality Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_clusters", "Number of Clusters:", 
                  min = 2, max = 5, value = 3),
      selectInput("pca_x", "X-axis (PCA Component):", 
                  choices = paste0("Comp.", 1:4), selected = "Comp.1"),
      selectInput("pca_y", "Y-axis (PCA Component):", 
                  choices = paste0("Comp.", 1:4), selected = "Comp.2")
    ),
    
    mainPanel(
      plotOutput("cluster_plot"),
      plotOutput("boxplot"),
      tableOutput("cluster_summary")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive clustering
  clustered_data <- reactive({
    kmeans_result <- kmeans(pca_scores[, 1:4], centers = input$num_clusters)
    pca_scores$Cluster <- as.factor(kmeans_result$cluster)
    white_wine$Cluster <- as.factor(kmeans_result$cluster)
    list(pca_scores = pca_scores, white_wine = white_wine)
  })
  
  # Scatterplot of clusters
  output$cluster_plot <- renderPlot({
    data <- clustered_data()
    ggplot(data$pca_scores, aes_string(x = input$pca_x, y = input$pca_y, color = "Cluster")) +
      geom_point(alpha = 0.6) +
      labs(title = "Clusters Based on PCA", x = input$pca_x, y = input$pca_y) +
      theme_minimal()
  })
  
  # Boxplot of quality by cluster
  output$boxplot <- renderPlot({
    data <- clustered_data()
    ggplot(data$white_wine, aes(x = Cluster, y = quality, fill = Cluster)) +
      geom_boxplot() +
      labs(title = "Quality Distribution by Cluster", x = "Cluster", y = "Quality") +
      theme_minimal()
  })
  
  # Cluster summary table
  output$cluster_summary <- renderTable({
    data <- clustered_data()
    data$white_wine %>%
      group_by(Cluster) %>%
      summarise(
        Mean_Quality = mean(quality),
        Median_Quality = median(quality),
        Min_Quality = min(quality),
        Max_Quality = max(quality),
        Count = n()
      )
  })
}
library(rsconnect)
rsconnect::deployApp('C:/Users/pinto/Desktop/Biostatistics Masters/Biostatistics Laboratory/Group Project 2/Shiny App.Rmd')

