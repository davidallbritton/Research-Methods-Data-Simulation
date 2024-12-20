library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(car)
library(effectsize)
library(sjstats)
# library(DT)  # referenced directly with :: rather than loaded as a library
# library(scales) # referenced directly but not loaded
# library(shinydashboard)  # not used. maybe try using for a different format sometime?

##############################################
#######  Define custom functions #######

###########
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

###########
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

###########
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

###########
render_analysis_outputs <- function(output, input, data, column_name) {
  output[[paste0(column_name, "_cell_means")]] <- renderTable({
    summary_data <- data %>%
      group_by(A, B) %>%
      summarize(
        Mean = mean(.data[[column_name]]),
        SD = sd(.data[[column_name]]),
        mu = mean(.data[["mu"]]),
        .groups = "drop"
      )
    #
    if (column_name != "DV") {
      summary_data <- summary_data %>%
        select(-mu)
    }
    #
    summary_data
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

###########
## Function to simulate reaction time data from Gaussian data input
transform_to_simulated_RT <- function(df, input_column_name, output_column_name, min_RT, max_RT) {
  # Shift the Gaussian data to ensure all values are positive
  shift <- abs(min(df[[input_column_name]])) + 1  # Ensure all values are > 0
  shifted_data <- df[[input_column_name]] + shift
  
  # Apply the log transformation
  df[[output_column_name]] <- log(shifted_data)
  
  # Rescale the data to a realistic range for reaction times
  df[[output_column_name]] <- scales::rescale(df[[output_column_name]], to = c(min_RT, max_RT))
  
  # Return the modified dataframe
  return(df)
}

#################   End of custom functions

##############################################
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
        create_analysis_tab("DV", "Analysis of DV"),
        create_analysis_tab("DV_likert7", "Analysis of DV_likert7"),
        create_analysis_tab("DV_likert5", "Analysis of DV_likert5"),
        create_analysis_tab("DV_rt", "Analysis of DV_rt"),
        
        tabPanel(
          "Explanation",
          p("This app generates simulated data for a 2x2 fully randomized",
            "(all between-subjects) design. It also shows some analyses for",
            "the simulated data. The first output panel shows the entire",
            "simulated dataset and has a button for downloading the simulated",
            "data as a .csv file. Each of the other output panels shows",
            "analyses of a different version of the simulated dependent",
            "variable."
          ),
          p("The first version of the simulated dependent variable is",
            "labeled DV. DV is a gaussian (normally distributed) variable",
            "created using the R function rnorm. The values for N, means,",
            "and standard deviation that are passed to rnorm are based on the",
            "user input in the left sidebar. Each of the other output panels",
            "shows another simulated dependent variable that was created by",
            "transforming DV. Thus, each time the user presses the button to",
            "generate a new dataset, a new set of simulated data points are",
            "generated for DV, and then all the other dependent variables are",
            "created by transforming those simulated data points. The",
            "versions of the dependent variable currently implemented are:"
          ),
          tags$ul(
            tags$li("DV - gaussian (normally distributed)"),
            tags$li("DV_likert7 - A 7-point scale"),
            tags$li("DV_likert5 - A 5-point scale"),
            tags$li("DV_rt - A reaction time measure ranging from roughly",
                    "200 to 800 milliseconds")
          ),
          p("Using a single random generation process (rnorm) to produce all",
            "of the dependent variables makes it possible to easily compare",
            "what the 'same' sample looks like when measured in different",
            "ways, by switching between output tabs. The trade-off is that",
            "the transformed versions of the dependent variable may have",
            "distributions that are different from real data. The reaction",
            "time variable DV_rt, for example, is a log transform of the",
            "gaussian DV. But more realistic reaction time distributions",
            "might result from instead using the R rinvgauss function for",
            "randomly generating inverse gaussian data. The tradeoff is that",
            "using a separate random data generation process for DV_rt would",
            "mean that DV_rt would have different sample means and subject",
            "deviations than DV -- they would no longer refer to the 'same'",
            "(simulated) sample, in other words. The way it is currently",
            "implemented, on the other hand, allows the user to think of each",
            "press of 'generate' as drawing a single sample, and each",
            "analysis tab as what the results might look like for that sample",
            "if it were measured in different ways."
          ),
          p("Most of the output panel content is self-explanatory, but a few",
            "things are worth noting. The effect size section displays the",
            "partial eta squared ('Eta2_partial') calculated from the ANOVA",
            "output. Cohen’s d ('d') is then calculated from the partial eta",
            "squared. Recall that 'd' is the size of the difference between",
            "two means in standard deviation units, and 'd' is how the user",
            "specifies the main effect and interaction effect sizes that are",
            "used to generate the simulated data."
          ),
          p("All the results in the output panels are based on the simulated",
            "sample of data, except the columns labeled 'mu' and 'True d'.",
            "'mu' is the 'true' cell means that were used to generate the",
            "simulated data (shown only for the gaussian variable DV). The",
            "'mu' for each cell of the design is determined from the",
            "user-specified grand mean and effect sizes. 'True d' is the",
            "effect size that was specified by the user. Comparing 'mu' to",
            "observed sample data cell means, and 'True d' to the observed",
            "sample data effect size 'd' allows the user to easily see the",
            "effects of sampling error. Repeatedly clicking 'generate' with",
            "various values of N and observing the fluctuation of the sample",
            "means and effect sizes may be a useful exercise to temper one’s",
            "enthusiasm about 'significant' results from small sample sizes."
          ),
          p("By David Allbritton, 2024")
        )

      )
    )
  )
)

##############################################
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
    
    # Generate likert scale data
    simulated_data <- add_likert(simulated_data, 7)
    simulated_data <- add_likert(simulated_data, 5)
    
    # Generate reaction time data
    simulated_data <- transform_to_simulated_RT(simulated_data, "DV", "DV_rt", 200, 800)
    
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
    render_analysis_outputs(output, input, data, "DV_likert7")
    render_analysis_outputs(output, input, data, "DV_likert5")
    render_analysis_outputs(output, input, data, "DV_rt")
  })
}

shinyApp(ui = ui, server = server)
