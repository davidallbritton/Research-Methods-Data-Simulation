library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(car)
library(effectsize)
library(sjstats)
# library(DT)  # referenced directly with :: rather than loaded as a library
# library(shinydashboard)  # maybe try using for a different format sometime?

#######  Define custom functions #######

## determine whether the effect size d should be positive or negative:
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

## Add a column to the simulated data with likert ratings from 1 to [scale_length]
# The dataframe df must contain a column "DV" that contains gaussian (normal) data
add_likert <- function(df, scale_length){
  n <- scale_length
  cname <- paste0("DV_likert", n)
  df[[cname]] <- cut(df$DV, 
                     breaks = seq(min(df$DV), 
                                  max(df$DV), 
                                  length.out = n+1), 
                     labels = 1:n, 
                     include.lowest = TRUE)
  # make it numeric rather than a factor
  df[[cname]] <- as.numeric(as.character(df[[cname]]))
  df
}

create_analysis_tab <- function(data_column, tab_name) {
  tabPanel(
    tab_name,
    fluidRow(
      column(6, tableOutput(paste0(data_column, "_cell_means"))),
      column(6, p(em("Effect Sizes:")), tableOutput(paste0(data_column, "_effect_sizes")))
    ),
    fluidRow(
      column(6, plotOutput(paste0(data_column, "_diagnostic_plot"), height = "250px")),
      column(6, verbatimTextOutput(paste0(data_column, "_anova_results")))
    )
  )
}

render_analysis_outputs <- function(output, input, data, column_name) {
  output[[paste0(column_name, "_cell_means")]] <- renderTable({
    data %>%
      group_by(A, B) %>%
      summarize(Mean = mean(.data[[column_name]]), SD = sd(.data[[column_name]]), .groups = "drop")
  })
  
  output[[paste0(column_name, "_anova_results")]] <- renderPrint({
    options(contrasts = c("contr.sum", "contr.poly"))
    anova_result <- Anova(lm(reformulate(c("A", "B", "A:B"), response = column_name), data = data), type = "III")
    print(anova_result)
  })
  
  output[[paste0(column_name, "_effect_sizes")]] <- renderTable({
    options(contrasts = c("contr.sum", "contr.poly"))
    anova_result <- Anova(lm(reformulate(c("A", "B", "A:B"), response = column_name), data = data), type = "III")
    partial_eta <- eta_squared(anova_result, partial = TRUE)
    sign_A <- effect_direction(data, "A")
    sign_B <- effect_direction(data, "B")
    sign_AB <- effect_direction(data, "AB")
    signs <- c(sign_A, sign_B, sign_AB)
    partial_eta$signs <- signs
    partial_eta <- partial_eta %>% mutate(d = signs * 2 * sqrt(Eta2_partial / (1 - Eta2_partial)))
    true_ds <- c(input$effect_size_A, input$effect_size_B, input$effect_size_AB)
    partial_eta$"True d" <- true_ds
    partial_eta %>% select(Parameter, Eta2_partial, d, "True d")
  })
  
  output[[paste0(column_name, "_diagnostic_plot")]] <- renderPlot({
    ggplot(data, aes(x = A, y = .data[[column_name]], color = B, group = B)) + 
      stat_summary(fun = mean, geom = "line", size = 1) + 
      stat_summary(fun = mean, geom = "point", size = 3) + 
      theme_minimal() +
      labs(title = paste("Group Means for", column_name), x = "Factor A", y = column_name)
  })
}

#################

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  titlePanel("Generating Simulated Data for a 2x2 between-subjects design"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("generate", "Generate Data"),
      numericInput("n", "Number of subjects per condition:", value = 25, min = 1),
      numericInput("baseline_mean", "Grand Mean:", value = 10),
      numericInput("effect_size_A", "Effect size for Factor A (Cohen's d):", value = 0.5),
      numericInput("effect_size_B", "Effect size for Factor B (Cohen's d):", value = -0.5),
      numericInput("effect_size_AB", "Effect size for Interaction (Cohen's d):", value = 0.2),
      numericInput("sd_error", "Standard deviation:", value = 1),
      numericInput("randseed", "Random seed (0 for random):", value = 0),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Simulated Data", 
                 downloadButton("download_data", "Download Data"),
                 br(), br(),
                 DT::dataTableOutput("data_table")
        ),
        create_analysis_tab("DV", "Analysis of DV")
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
    
    # create rows for subjects and columns for IVs A and B
    simulated_data <- expand.grid(Subject = 0, A = c("A1", "A2"), B = c("B1", "B2"))
    simulated_data <- simulated_data[rep(1:nrow(simulated_data), each = n), ]
    simulated_data$Subject <- 1:nrow(simulated_data)
    
    # generate the "true means" mu for each subject
    simulated_data$mu <- with(simulated_data, {
      mu <- Baseline_Mean
      # Explanation of how these statements change the mean mu, using (A == "A1") as an example:
      # Note that (A == "A1") will be 1 if true, 0 if false.  So the mean mu will
      # only be changed by subtracting half of effect_A on rows where (A == "A1") is true.
      mu <- mu - (A == "A1") * effect_A / 2 + (A == "A2") * effect_A / 2
      mu <- mu - (B == "B1") * effect_B / 2 + (B == "B2") * effect_B / 2
      mu <- mu - (A == "A1" & B == "B2") * effect_AB / 2 + (A == "A2" & B == "B2") * effect_AB / 2
      mu <- mu + (A == "A1" & B == "B1") * effect_AB / 2 - (A == "A2" & B == "B1") * effect_AB / 2
      mu
    })
    
    # these need to be factors rather than numbers:
    simulated_data$A <- factor(simulated_data$A)
    simulated_data$B <- factor(simulated_data$B)
    
    # Generate gaussian data for DV from column "mu" of the dataframe "simulated_data"
    simulated_data$DV <- rnorm(nrow(simulated_data), mean = simulated_data[["mu"]], sd = sd_error) # Add Gaussian noise
    
    simulated_data <- add_likert(simulated_data, 7)
    simulated_data <- add_likert(simulated_data, 5)
    
    simulated_data
  })
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(generate_data(), options = list(pageLength = 10))
  })
  
  output$download_data <- downloadHandler(
    filename = function() { "simulated_data.csv" },
    content = function(file) {
      write.csv(generate_data(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$generate, {
    data <- generate_data()
    render_analysis_outputs(output, input, data, "DV")
  })
}

shinyApp(ui = ui, server = server)
