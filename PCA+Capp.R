library(shiny)
library(tidyverse)
library(cluster)
library(factoextra)

ui <- fluidPage(
  titlePanel("PCA and Clustering Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Upload CSV File:", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      checkboxInput("scale_data", "Scale Data (Recommended)", value = TRUE),
      sliderInput("num_clusters", "Number of Clusters:", min = 2, max = 10, value = 3),
      selectInput("pca_x", "X-axis (PCA Component):", choices = NULL),
      selectInput("pca_y", "Y-axis (PCA Component):", choices = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Variance Explained", plotOutput("variance_plot")),
        tabPanel("PCA Scores and Clusters", plotOutput("cluster_plot")),
        tabPanel("Cluster Summary", tableOutput("cluster_summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$dataset)
    read.csv(input$dataset$datapath, sep = ",") %>%
      select(where(is.numeric)) %>%
      na.omit()  # Remove rows with missing values
  })
  
  pca_result <- reactive({
    req(dataset())
    if (input$scale_data) {
      princomp(dataset(), cor = TRUE)
    } else {
      princomp(dataset(), cor = FALSE)
    }
  })
  
  pca_scores <- reactive({
    req(pca_result())
    as.data.frame(pca_result()$scores)
  })
  
  observe({
    req(pca_scores())
    components <- colnames(pca_scores())
    updateSelectInput(session, "pca_x", choices = components, selected = components[1])
    updateSelectInput(session, "pca_y", choices = components, selected = components[2])
  })
  
  output$variance_plot <- renderPlot({
    req(pca_result())
    explained <- pca_result()$sdev^2 / sum(pca_result()$sdev^2) * 100
    qplot(x = seq_along(explained), y = explained, geom = "line") +
      geom_point() +
      labs(title = "Scree Plot", x = "Principal Component", y = "Explained Variance (%)") +
      theme_minimal()
  })
  
  clustered_data <- reactive({
    req(pca_scores())
    set.seed(123)
    kmeans_result <- kmeans(pca_scores(), centers = input$num_clusters)
    scores_with_clusters <- pca_scores()
    scores_with_clusters$Cluster <- as.factor(kmeans_result$cluster)
    list(data = scores_with_clusters, kmeans = kmeans_result)
  })
  
  output$cluster_plot <- renderPlot({
    req(clustered_data(), input$pca_x, input$pca_y)
    ggplot(clustered_data()$data, 
           aes_string(x = input$pca_x, y = input$pca_y, color = "Cluster")) +
      geom_point(alpha = 0.7, size = 3) +
      labs(title = "Clusters Based on PCA", x = input$pca_x, y = input$pca_y) +
      theme_minimal()
  })
  
  output$cluster_summary <- renderTable({
    req(clustered_data())
    clustered_data()$data %>%
      group_by(Cluster) %>%
      summarise(across(where(is.numeric), list(mean = mean, median = median, min = min, max = max)))
  })
}

shinyApp(ui = ui, server = server)

```

