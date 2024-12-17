library(shiny)
library(ggplot2)
library(dplyr)
library(car)
library(effectsize)
library(sjstats)

#######  Define custom functions #######
effect_direction <- function(data, effect) {
  # Ensure valid effect input
  if (!effect %in% c("A", "B", "AB")) {
    stop("Effect must be one of 'A', 'B', or 'AB'")
  }
  
  # For main effect of A
  if (effect == "A") {
    mean_A1 <- mean(data$DV[data$A == "A1"])
    mean_A2 <- mean(data$DV[data$A == "A2"])
    return(ifelse(mean_A1 <= mean_A2, 1, -1))
  }
  
  # For main effect of B
  if (effect == "B") {
    mean_B1 <- mean(data$DV[data$B == "B1"])
    mean_B2 <- mean(data$DV[data$B == "B2"])
    return(ifelse(mean_B1 <= mean_B2, 1, -1))
  }
  
  # For interaction effect AB
  if (effect == "AB") {
    mean_A1B1 <- mean(data$DV[data$A == "A1" & data$B == "B1"])
    mean_A1B2 <- mean(data$DV[data$A == "A1" & data$B == "B2"])
    mean_A2B1 <- mean(data$DV[data$A == "A2" & data$B == "B1"])
    mean_A2B2 <- mean(data$DV[data$A == "A2" & data$B == "B2"])
    
    interaction_A1 <- mean_A1B2 - mean_A1B1
    interaction_A2 <- mean_A2B2 - mean_A2B1
    
    return(ifelse(interaction_A2 >= interaction_A1, 1, -1))
  }
}
#################

ui <- fluidPage(
  titlePanel("2x2 Between-Subjects Data Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of subjects per condition:", value = 25, min = 1),
      numericInput("baseline_mean", "Grand Mean:", value = 10),
      numericInput("effect_size_A", "Effect size for Factor A (Cohen's d):", value = 0.5),
      numericInput("effect_size_B", "Effect size for Factor B (Cohen's d):", value = 0.5),
      numericInput("effect_size_AB", "Effect size for Interaction (Cohen's d):", value = 0.2),
      numericInput("sd_error", "Standard deviation of error:", value = 2),
      numericInput("randseed", "Random seed (0 for random):", value = 0),
      actionButton("generate", "Generate Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Simulated Data", 
                 downloadButton("download_data", "Download Data"),
                 br(), br(),
                 tableOutput("data_table")
        ),
        
        tabPanel("Cell Means", tableOutput("cell_means")),
        tabPanel("ANOVA Results", verbatimTextOutput("anova_results")),
        tabPanel("Effect Sizes", tableOutput("effect_sizes")),
        tabPanel("Plot", plotOutput("diagnostic_plot"))
      )
    )
  )
)

server <- function(input, output) {
  generate_data <- eventReactive(input$generate, {
    n <- input$n
    Baseline_Mean <- input$baseline_mean
    effect_A <- input$effect_size_A * input$sd_error
    effect_B <- input$effect_size_B * input$sd_error
    effect_AB <- input$effect_size_AB * input$sd_error
    sd_error <- input$sd_error
    
    if (input$randseed > 0) set.seed(input$randseed)
    
    simulated_data <- expand.grid(A = c("A1", "A2"), B = c("B1", "B2"))
    simulated_data <- simulated_data[rep(1:nrow(simulated_data), each = n), ]
    simulated_data$Subject <- 1:nrow(simulated_data)
    
    simulated_data$DV <- with(simulated_data, {
      mu <- Baseline_Mean
      mu <- mu - (A == "A1") * effect_A / 2 + (A == "A2") * effect_A / 2
      mu <- mu - (B == "B1") * effect_B / 2 + (B == "B2") * effect_B / 2
      mu <- mu - (A == "A1" & B == "B2") * effect_AB / 2 + (A == "A2" & B == "B2") * effect_AB / 2
      mu <- mu + (A == "A1" & B == "B1") * effect_AB / 2 - (A == "A2" & B == "B1") * effect_AB / 2
      rnorm(nrow(simulated_data), mean = mu, sd = sd_error)
    })
    
    simulated_data$A <- factor(simulated_data$A)
    simulated_data$B <- factor(simulated_data$B)
    simulated_data
  })
  
  output$data_table <- renderTable({
    head(generate_data(), 30)
  })
  
  output$download_data <- downloadHandler(
    filename = function() { "simulated_data.csv" },
    content = function(file) {
      write.csv(generate_data(), file, row.names = FALSE)
    }
  )
  
  output$cell_means <- renderTable({
    generate_data() %>%
      group_by(A, B) %>%
      summarize(Mean = mean(DV), SD = sd(DV), .groups = "drop")
  })
  
  output$anova_results <- renderPrint({
    data <- generate_data()
    options(contrasts = c("contr.sum", "contr.poly"))
    anova_result <- Anova(lm(DV ~ A * B, data = data), type = "III")
    print(anova_result)
  })
  
  output$effect_sizes <- renderTable({
    data <- generate_data()
    
    options(contrasts = c("contr.sum", "contr.poly"))
    anova_result <- Anova(lm(DV ~ A * B, data = data), type = "III")
    partial_eta <- eta_squared(anova_result, partial = TRUE)
    # determine whether effect sizes are positive or negative
    sign_A <- effect_direction(data, "A")
    sign_B <- effect_direction(data, "B")
    sign_AB <- effect_direction(data, "AB")
    signs <- c(sign_A, sign_B, sign_AB)
    partial_eta$signs <- signs
    partial_eta <- partial_eta %>% mutate(d = signs * 2 * sqrt(Eta2_partial / (1 - Eta2_partial)))
    # re-read the inputs for the true effect sizes
    isolate(
      true_ds <- c(input$effect_size_A, input$effect_size_B, input$effect_size_AB)
    )
    partial_eta$"True d" <- true_ds
    partial_eta %>% select(Parameter,	Eta2_partial,	d, "True d")
  })
  
  output$diagnostic_plot <- renderPlot({
    data <- generate_data()
    ggplot(data, aes(x = A, y = DV, color = B, group = B)) + 
      stat_summary(fun = mean, geom = "line", size = 1) + 
      stat_summary(fun = mean, geom = "point", size = 3) + 
      theme_minimal() +
      labs(title = "Group Means for DV", x = "Factor A", y = "Dependent Variable (DV)")
  })
}

shinyApp(ui = ui, server = server)
