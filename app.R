# app.R ---------------------------------------------------------------
# ABM-SEIR Simulation Platform Inspired by COVID-ABS

library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)    
library(broom)
library(plotly)   
library(viridis)  

# ======================
# 0. Global Settings & Helper function
# ======================


my_colors <- c(
  "S" = "#4682B4",
  "E" = "#FFA500",
  "I" = "#B22222",
  "R" = "#228B22",
  "D" = "#696969"
)

# Agent-based SEIR simulator
simulate_abm_single <- function(params, n_days, time_step) {
  
  N      <- params$n_agents
  beta   <- params$beta
  sigma  <- params$sigma
  gamma  <- params$gamma
  c_bar  <- params$contact_rate
  sd     <- params$social_distancing
  mask_e <- params$mask_effectiveness
  
  # --- 1. Initialize Agents (Random Spatial Grid for Visualization) ---
  agents <- data.frame(
    id = 1:N,
    x = runif(N),
    y = runif(N),
    state = "S",
    stringsAsFactors = FALSE
  )
  
  n_I0 <- round(N * params$init_infected / 100)
  n_E0 <- round(N * params$init_exposed / 100)
  
  initial_infected_idx <- sample(1:N, n_I0 + n_E0)
  if(n_I0 > 0) agents$state[initial_infected_idx[1:n_I0]] <- "I"
  if(n_E0 > 0) agents$state[initial_infected_idx[(n_I0+1):(n_I0+n_E0)]] <- "E"
  
  states_mat <- matrix(NA_character_, nrow = length(seq(0, n_days, time_step)), ncol = N)
  states_mat[1, ] <- agents$state
  
  # Parameters
  times <- seq(0, n_days, by = time_step)
  n_t   <- length(times)
  
  beta_eff <- max(beta * (1 - mask_e) * (1 - sd), 0)
  p_EI <- max(min(1 - exp(-sigma * time_step), 1), 0)
  p_IR <- max(min(1 - exp(-gamma * time_step), 1), 0)
  
  # --- 2. Simulation Loop ---
  current_states <- agents$state
  
  for (k in 2:n_t) {
    S_idx <- which(current_states == "S")
    E_idx <- which(current_states == "E")
    I_idx <- which(current_states == "I")
    
    # S -> E
    I_frac    <- length(I_idx) / N
    lambda_SI <- beta_eff * c_bar * I_frac
    p_inf     <- max(min(1 - exp(-lambda_SI * time_step), 1), 0)
    
    new_E <- integer(0)
    if (length(S_idx) > 0 && p_inf > 0) {
      is_infected <- rbinom(length(S_idx), size = 1, prob = p_inf) == 1
      new_E <- S_idx[is_infected]
    }
    
    # E -> I
    new_I <- integer(0)
    if (length(E_idx) > 0 && p_EI > 0) {
      becomes_I <- rbinom(length(E_idx), size = 1, prob = p_EI) == 1
      new_I <- E_idx[becomes_I]
    }
    
    # I -> R
    new_R <- integer(0)
    if (length(I_idx) > 0 && p_IR > 0) {
      recovers <- rbinom(length(I_idx), size = 1, prob = p_IR) == 1
      new_R <- I_idx[recovers]
    }
    
    current_states[new_E] <- "E"
    current_states[new_I] <- "I"
    current_states[new_R] <- "R"
    states_mat[k, ] <- current_states
  }
  
  # --- 3. Aggregate Results (Reduce memory usage) ---
  # Only store counts for plots, keeping raw data minimal if possible
  counts <- apply(states_mat, 1, function(x) table(factor(x, levels = c("S","E","I","R"))))
  summary_df <- as.data.frame(t(counts))
  summary_df$time <- times
  summary_df <- summary_df %>%
    pivot_longer(cols = c("S","E","I","R"), names_to = "compartment", values_to = "count")
  
  list(
    summary = summary_df,
    states_mat = states_mat, 
    agent_coords = agents[, c("id", "x", "y")] 
  )
}

# Scenario Metadata
scenario_ids    <- paste0("scen", 1:7)
scenario_names  <- c(
  "1: No Prevention Measures", "2: Lockdown", "3: Conditional Lockdown", 
  "4: Vertical Isolation", "5: Partial Isolation", "6: Mask Only", "7: Mask + Isolation"
)
scenario_choices <- setNames(scenario_ids, scenario_names)
scenario_labels  <- setNames(scenario_names, scenario_ids)


# ======================
# 1. UI
# ======================

ui <- navbarPage(
  title = "ABM-SEIR Simulation Platform",
  theme = shinytheme("flatly"),
  
header = tags$head(
  tags$style(HTML("

    .slider-animate-container .slider-animate-button {
      width: 30px !important;
      height: 30px !important;
      background-color: #3498db !important;
      border-radius: 50% !important;
      display: flex !important;
      justify-content: center !important;
      align-items: center !important;
      cursor: pointer !important;
      box-shadow: 0 4px 10px rgba(0,0,0,0.25) !important;
      margin-left: 10px !important;
      position: relative !important;
    }

    .slider-animate-container .slider-animate-button::before,
    .slider-animate-container .slider-animate-button .glyphicon,
    .slider-animate-container .slider-animate-button .fa,
    .slider-animate-container .slider-animate-button span {
      display: none !important;
      opacity: 0 !important;
      visibility: hidden !important;
    }

    .slider-animate-container .slider-animate-button::after {
      content: '';
      width: 0;
      height: 0;
      border-top: 10px solid transparent;
      border-bottom: 10px solid transparent;
      border-left: 16px solid white;
      margin-left: 6px;
    }

    /* Hover */
    .slider-animate-container .slider-animate-button:hover {
      background-color: #2e86c1 !important;
      box-shadow: 0 6px 12px rgba(0,0,0,0.3) !important;
    }

  "))
)


  ,
  
  # ----------------------
  # Documentation (ReadMe)
  # ----------------------
  tabPanel(
    "ReadMe",
    withMathJax(), 
    
    # --- Custom CSS for pretty look ---
    tags$head(
      tags$style(HTML("
        .doc-container { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333; }
        .doc-card { 
          background-color: #ffffff; 
          border-radius: 8px; 
          box-shadow: 0 4px 12px rgba(0,0,0,0.1); 
          padding: 25px; 
          margin-bottom: 25px; 
          border-left: 6px solid #2c3e50; /* Signature Dark Blue */
          transition: transform 0.2s;
        }
        .doc-card:hover { transform: translateY(-2px); } /* Subtle hover effect */
        .doc-header { color: #2c3e50; font-weight: 700; border-bottom: 1px solid #eee; padding-bottom: 10px; margin-top: 0; }
        .doc-subhead { color: #e67e22; font-weight: 600; margin-top: 20px; margin-bottom: 10px; } /* Orange accents */
        .math-box { 
          background-color: #f8f9fa; 
          border: 1px solid #e9ecef; 
          border-radius: 6px; 
          padding: 15px; 
          margin: 15px 0; 
          text-align: center;
        }
        .ref-list li { margin-bottom: 10px; line-height: 1.5; }
        a { text-decoration: none; color: #2980b9; font-weight: 500; }
        a:hover { text-decoration: underline; color: #c0392b; }
      "))
    ),
    
    fluidPage(class = "doc-container",
              column(10, offset = 1,
                     
                     br(),
                     div(
                       style = "text-align: center; margin-bottom: 30px;",
                       h1("ABM-SEIR Simulation Platform", 
                          style = "font-weight: 800; color: #2c3e50; letter-spacing: 1px;"),
                       p("Methodology, Mathematical Framework, and Statistical Analysis", 
                         style = "font-size: 1.05em; color: #7f8c8d;")
                     )
                     ,
                     
                     # --- 1. Background and Methods ---
                     div(class = "doc-card",
                         h3("1. Background and Methods", class = "doc-header"),
                         p("This application simulates the dynamics of the COVID-19 pandemic using an ", strong("Agent-Based Model (ABM)"), 
                           " framework. Unlike aggregate equation-based models, ABM simulates individual agents interacting within a virtual environment. "),
                         p("The simulation logic is rooted in the ", strong("SEIR compartmental framework"), " and inspired by the methodology presented in ",
                           em("COVID-ABS: An agent-based model of COVID-19 epidemic."), " (Silva et al., 2020)."),
                         
                         div(style = "background-color: #eaf2f8; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db; margin-top: 15px;",
                             strong("Methodological Approach: Monte Carlo Simulations"),
                             p("Agent-based models are inherently stochastic. A single simulation run represents only one possible trajectory of the epidemic. To ensure statistical robustness, this application utilizes ", strong("Monte Carlo methods"), ". 
                             For each scenario, the simulation is executed over \\(N_{reps}\\) independent repetitions. The results are aggregated to compute the Mean and 
                               95% Confidence Intervals (CI).")
                         ),
                         
                        
                         br(),
                         div(style = "text-align: center; margin-top: 20px; padding-top: 20px; border-top: 1px dashed #eee;",
                            
                             tags$img(src = "flow.png", 
                                      style = "max-width: 90%; height: auto; border: 1px solid #ddd; padding: 5px; border-radius: 4px; box-shadow: 0 4px 8px rgba(0,0,0,0.05);"),
                            
                             p(style = "margin-top: 10px; font-size: 0.9em; color: #666; font-style: italic;",
                               "Figure 1: Workflow of the Agent-Based Model Simulation.")
                         )
                     ),

                     
                     # --- 2. Key Concepts ---
                     div(class = "doc-card",
                         h3("2. Key Concepts & Parameters", class = "doc-header"),
                         
                         h4("Epidemiological Compartments", class = "doc-subhead"),
                         tags$ul(
                           tags$li(strong("Susceptible (S):"), " Vulnerable individuals. Probability of infection depends on local prevalence."),
                           tags$li(strong("Exposed (E):"), " Infected but not yet infectious (Latent period)."),
                           tags$li(strong("Infected (I):"), " Infectious individuals capable of transmitting the virus."),
                           tags$li(strong("Recovered (R):"), " Immune individuals removed from the transmission chain.")
                         ),
                         
                         p("You may find more compartment SEIR model information ", 
                           tags$a("here", href = "https://doi.org/10.1038/s41592-020-0856-2", target = "_blank"), 
                           "."),
                         
                         h4("Core Parameters", class = "doc-subhead"),
                         p("In our simulation, Transmission Rate (\\(\\beta\\)), Incubation Rate (\\(\\sigma\\)), and Recovery Rate (\\(\\gamma\\)) are the main input parameters. However, lots of research articles implement Basic Reproduction Number \\(R_0\\), 
                           Incubation Period \\(T_{inc}\\) and Infectious Period \\(T_{inf}\\). Here, we provide some explanations of the relationships between them."),
                         tags$ul(
                           tags$li(strong("Transmission Rate (\\(\\beta\\)):"), " Controls the speed of spread. Related to the Basic Reproduction Number by \\(R_0 = \\frac{\\beta}{\\gamma}\\)."),
                           tags$li(strong("Incubation Rate (\\(\\sigma\\)):"), " Inverse of the incubation period: \\(\\sigma\\ = \\frac{1}{T_{inc}}\\). (e.g., 5-day incubation \\(\\to \\sigma = 0.2\\))."),
                           tags$li(strong("Recovery Rate (\\(\\gamma\\)):"), " Inverse of the infectious period: \\(\\gamma = \\frac{1}{T_{inf}}\\). (e.g., 5-day infectiousness \\(\\to \\gamma = 0.2\\)).")
                         )
                     ),
                     
                     # --- 3. Mathematical Framework ---
                     div(class = "doc-card",
                         h3("3. Mathematical Framework", class = "doc-header"),
                         p("The simulation operates in discrete time steps (\\(\\Delta t = 1 \\text{ day}\\)). Transitions are stochastic (Bernoulli trials) based on the following probabilities."),
                         
                         h4("A. The Effective Transmission Rate", class = "doc-subhead"),
                         p("The raw transmission rate \\(\\beta\\) is reduced by interventions (Masks and Social Distancing):"),
                         div(class = "math-box",
                             p("$$ \\beta_{\\text{eff}} = \\beta \\times (1 - \\text{MaskEffect}) \\times (1 - \\text{SocialDistancing}) $$")
                         ),
                         
                         h4("B. The Force of Infection (\\(\\lambda\\))", class = "doc-subhead"),
                         p("The probability of a Susceptible agent meeting an Infected agent depends on the Contact Rate (\\(c\\)) and current Prevalence:"),
                         div(class = "math-box",
                             p("$$ \\lambda = \\beta_{\\text{eff}} \\times c \\times \\frac{I_{\\text{total}}}{N} $$")
                         ),
                         
                         h4("C. State Transition Probabilities", class = "doc-subhead"),
                         p("Probabilities per time step derived from exponential decay assumptions:"),
                         div(class = "math-box",
                             p("$$ P(S \\to E) = 1 - e^{-\\lambda \\Delta t} $$"),
                             p("$$ P(E \\to I) = 1 - e^{-\\sigma \\Delta t} $$"),
                             p("$$ P(I \\to R) = 1 - e^{-\\gamma \\Delta t} $$")
                         )
                     ),
                     
                     # --- 4. Statistical Analysis ---
                     div(class = "doc-card",
                         h3("4. Statistical Analysis", class = "doc-header"),
                         p("Simulation analysis involves fitting a linear regression model to the aggregate dataset (\\(N = \\text{Scenarios} \\times \\text{Repetitions}\\)). The model quantifies the impact of interventions on outcomes Peak Prevalence and Day of Peak."),
                         
                         h4("Model Specification with Interactions", class = "doc-subtitle"),
                         p("To capture the potential effects of combined interventions, the model includes pairwise interaction terms between all selected predictors (Social Distancing, Mask Usage, and Lockdown). The full model takes the form:"),
                         
                         div(class = "math-block",
                             p("$$ Y = \\beta_0 + \\underbrace{\\beta_1 SD + \\beta_2 Mask + \\beta_3 Lockdown}_{\\text{Main Effects}} + \\underbrace{\\beta_{ij} (X_i \\times X_j)}_{\\text{Interaction Terms}} + \\epsilon $$")
                         ),
                         
                         h4("Statistical Interpretation: Interaction Effects", class = "doc-subtitle"),
                         tags$ul(
                           tags$li(strong("The Baseline Trap:"), " In models with interactions, the main effect coefficient represents the outcome only when the interacting variable is zero. If this baseline is theoretical or rare, the coefficient's sign may not reflect the policy's true effect in practice."),
                           tags$li(strong("Avoid Isolated Interpretation:"), " When an interaction term is significant, coefficients cannot be interpreted in isolation. The true effect is dynamic and determined jointly by the main and interaction effects. Ignoring the interaction leads to erroneous conclusions."),
                           tags$li(strong("Focus on Net Effect:"), " Evaluate true effectiveness based on the net effect under specific conditions. A positive baseline coefficient often reverses into a beneficial net effect when combined with high-intensity implementation measures, confirming the strategy's efficacy.")
                         )
                     ),
                     
                     br(),
                     
                     # --- 5. Limitations & Future Work ---
                     div(class = "doc-card",
                         h3("5. Limitations & Future Work", class = "doc-header"),
                         p("While this simulation provides valuable insights into intervention dynamics, the following limitations should be considered when interpreting the results and might be improved in the future:"),
                         
                         tags$ul(
                           tags$li(strong("Computational Constraints (Population Size):"), 
                                   " Agent-Based Models (ABM) are inherently memory-intensive. To ensure the responsiveness of this Shiny application, we capped the simulation population at 5,000 individuals. This scale may limit the observation of rare stochastic events that would be visible in larger, real-world population scales."),
                           
                           tags$li(strong("Scenario Scope & Data Limitations:"), 
                                   " The simulation structure is strictly based on the seven predefined scenarios and fixed parameter sets from Silva et al. (2020). This results in a constrained dataset and parameter space, potentially overlooking the variability found in broader epidemiological contexts (e.g., age-stratified contact patterns)."),
                           
                           tags$li(strong("Modeling Assumptions:"), 
                                   " The statistical analysis relies on standard Linear Regression (OLS). While effective for estimating effect sizes and interactions, this linear approach serves as an approximation and may not fully capture the complex non-linear dynamics and feedback loops characteristic of viral transmission.")
                         )
                     ),
                     
                     br(),
                     
                     # --- 6. References ---
                     div(class = "doc-card",
                         h3("6. References & Data Sources", class = "doc-header"),
                         tags$ul(class = "ref-list",
                                 
                                 tags$li("Kucharski, A. J., et al. (2020). Early dynamics of transmission and control of COVID-19: a mathematical modelling study. ", em("The Lancet Infectious Diseases"), ". ", a(href="https://doi.org/10.1016/S1473-3099(20)30144-4", target="_blank", "DOI Link")),
                                 tags$li("Li, Q., et al. (2020). Early Transmission Dynamics in Wuhan, China. ", em("NEJM"), ". ", a(href="https://www.nejm.org/doi/full/10.1056/NEJMoa2001316", target="_blank", "DOI Link")),
                                 tags$li("Wu, J. T., Leung, K., & Leung, G. M. (2020). Nowcasting and forecasting the potential spread. ", em("The Lancet"), ". ", a(href="https://doi.org/10.1016/S0140-6736(20)30260-9", target="_blank", "DOI Link")),
                                 tags$li("WHO. (2020). Statement on IHR Emergency Committee meeting. ", a(href="https://www.who.int/news-room/detail/23-01-2020-statement-on-the-meeting-of-the-international-health-regulations-(2005)-emergency-committee-regarding-the-outbreak-of-novel-coronavirus-(2019-ncov)", target="_blank", "View Statement")),
                                 tags$li("WHO-China Joint Mission. (2020). Final Report. ", a(href="https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf", target="_blank", "View PDF")),
                                 tags$li("Liu, T., et al. (2020). Transmission dynamics of 2019-nCoV. ", em("bioRxiv"), ". ", a(href="https://www.biorxiv.org/content/10.1101/2020.01.25.919787v2", target="_blank", "DOI Link")),
                                 tags$li("Rocklöv, J., et al. (2020). COVID-19 outbreak on the Diamond Princess. ", em("Journal of Travel Medicine"), ". ", a(href="https://doi.org/10.1093/jtm/taaa030", target="_blank", "DOI Link")),
                                 tags$li("Backer, J. A., et al. (2020). Incubation period of 2019-nCoV. ", em("Eurosurveillance"), ". ", a(href="https://doi.org/10.2807/1560-7917.ES.2020.25.5.2000062", target="_blank", "DOI Link")),
                                 tags$li("Read, J. M., et al. (2020). Novel coronavirus 2019-nCoV: early estimation. ", em("medRxiv"), ". ", a(href="https://doi.org/10.1101/2020.01.23.20018549", target="_blank", "DOI Link")),
                                 tags$li("Bi, Q., et al. (2020). Epidemiology and transmission of COVID-19 in Shenzhen. ", em("The Lancet Infectious Diseases"), ". ", a(href="https://doi.org/10.1016/S1473-3099(20)30287-5", target="_blank", "DOI Link")),
                                 tags$li("Tang, B., et al. (2020). Estimation of the Transmission Risk. ", em("Journal of Clinical Medicine"), ". ", a(href="https://www.mdpi.com/2077-0383/9/2/462", target="_blank", "DOI Link")),
                                 tags$li("Goh, G. (2020). Epidemic Calculator. ", a(href="https://gabgoh.github.io/COVID/?utm_source=catalyzex.com", target="_blank", "Interactive Tool")),
                                 tags$li("Silva, P. C. L., et al. (2020). COVID-ABS: An agent-based model of COVID-19 epidemic. ", em("Chaos, Solitons & Fractals"), ". ", a(href="https://doi.org/10.1016/j.chaos.2020.110088", target="_blank", "DOI Link"))
                         )
                     ),
                     br(), br()
              )
    )
  ),
  
  # --- Step 1: Global Setup ---
  tabPanel(
    "Step 1: Setup & Run",
    sidebarLayout(
      sidebarPanel(
        h4("Global Parameters"),
        sliderInput("n_agents", "Population Size (N):", 0, 5000, 2500, step=100),
        sliderInput("n_reps", "Repetitions per Scenario (for Stats):", 
                    min=5, max=100, value=50, step=5),
        helpText("Running more repetitions improves statistical validity but takes longer."),
        
        hr(),
        h4("Epidemiology"),
        sliderInput("init_infected", "Initial % Infected:", 0, 1, 0.05, step=0.01),
        sliderInput("beta", "Transmission Rate (beta):", 0, 2.5, 1.03, step=0.01),
        sliderInput("sigma", "Incubation Rate (sigma):", 0, 1, 0.19, step=0.01),
        sliderInput("gamma", "Recovery Rate (gamma):", 0, 1, 0.34, step=0.01),
        
        hr(),
        h4("Simulation"),
        sliderInput("n_days", "Horizon (Days):", 30, 200, 60, step=10),
        actionButton("run_sim", "Run Simulation (All Scenarios)", 
                     class = "btn-primary btn-lg btn-block", icon = icon("play")),
        br(),
        actionButton("reset_sim", "Reset", class = "btn-secondary btn-block")
      ),
      mainPanel(
        h2("Step 1: Configure & Simulate"),
        p("Configure the simulation using the sliders on the left, or select a reference study below to auto-populate parameters."),
        
        # --- Part A: Reference Parameters (Interactive) ---
        div(class = "panel panel-primary",
            div(class = "panel-heading", strong("Reference Epidemiological Parameters (Click to Load)")),
            div(class = "panel-body",
                p("Select a row to automatically set the global parameters Beta, Sigma, Gamma based on literature estimates."),
                DTOutput("ref_param_table"),
                helpText("Calculated Parameters: \\(\\beta = R_0 * \\frac{1}{T_{Inf}}\\), \\(\\sigma\\ = \\frac{1}{T_{inc}}\\), \\(\\gamma\\ = \\frac{1}{T_{inf}}\\). Rates are inverses of periods (1/Days)."),
                div(
                  style = "
                            margin-top: 10px; 
                            font-size: 0.95em; 
                            background-color: #eaf6ff; 
                            border-left: 5px solid #6bb7ff; 
                            padding: 12px 15px; 
                            border-radius: 5px;
                          ",
                  HTML("
                          <strong>Note:</strong><br>
                          Values in parentheses indicate 95% Confidence Intervals (CI) or Range where available.<br><br>
                          If a study reports <em>N/A</em> for any parameter, the app will use default estimates from
                          <em>Early dynamics of transmission and control of COVID-19: a mathematical modelling study</em>
                          (Kucharski et al., 2020) to prevent computational errors:<br>
                          <strong>R₀ = 3.0, Incubation Period = 5.2 days, Infectious Period = 2.9 days.</strong><br><br>
                       ")
                )
                
            )
        ),
        
        hr(),
        
        # --- Part B: Scenario Definitions ---
        h3("Scenario Definitions (Interventions)"),
        p("The following 7 scenarios will be simulated simultaneously to compare outcomes."),
        tableOutput("scenario_desc_table"),
        
        
      )
    )
  ),
  
  # Step 2
  tabPanel("Step 2: Simulation Results",
           fluidPage(
             br(),
             conditionalPanel("output.hasSimData == false",
                              div(class="jumbotron", style="text-align:center; background:#f8f9fa; padding:40px;",
                                  h2("No Data Available"), p("Please run the simulation in Step 1."), icon("arrow-circle-left", "fa-3x"))
             ),
             conditionalPanel("output.hasSimData == true",
                              fluidRow(
                                column(3,
                                       wellPanel(
                                         h4("Scenario Selection"),
                                         selectInput("view_scen", "Choose Scenario:", choices = scenario_choices, selected = "scen1"),
                                         hr(),
                                         h4("Playback Control"),
                                         sliderInput("anim_time", "Day:",
                                                     min=0, max=100, value=0, step=1,
                                                     animate = animationOptions(interval = 200, loop = FALSE)
                                         ),
                                         tags$div(
                                           style = "font-size:0.9em; color:#555; margin-top:8px; margin-bottom:12px;",
                                           "Use the playback control above to view the animated progression of the epidemic over time."
                                         ),
                                         br(),
                                         div(style="text-align:center; font-size:1.3em; color:#2c3e50; font-weight:bold;",
                                             textOutput("current_day_display"))
                                       )
                                ),
                                column(9,
                                       fluidRow(column(12,
                                                       h4(icon("chart-line"), "Epidemic Curve (Mean ± 95% CI)", style="text-align:center; color:#2c3e50; margin-bottom:10px;"),
                                                       plotlyOutput("plot_ts_ribbon", height="420px")
                                       )),
                                       br(),
                                       fluidRow(
                                         column(8,
                                                h4(icon("virus"), "Spatial Spread Animation", style="text-align:center; color:#2c3e50; margin-bottom:10px;"),
                                                plotOutput("plot_spatial", height="500px")
                                         ),
                                         column(4,
                                                h4(icon("table"), "Live Compartment Counts", style="text-align:center; color:#2c3e50;"),
                                                tableOutput("spatial_stats_table"),
                                                br(),
                                                div(style="background:#f8f9fa; padding:15px; border-radius:8px; text-align:center;",
                                                    strong("Current Day: "), textOutput("current_day_display2", inline=TRUE), br(), br(),
                                                    span(style="color:#4682B4;", "Blue = Susceptible"), br(),
                                                    span(style="color:#FFA500;", "Orange = Exposed"), br(),
                                                    span(style="color:#B22222;", "Red = Infected"), br(),
                                                    span(style="color:#228B22;", "Green = Recovered")
                                                )
                                         )
                                       )
                                )
                              )
             )
           )
  ),
  # --- Step 3: Comparison ---
  tabPanel(
    "Step 3: Scenario Comparison",
    fluidPage(
      br(),
      conditionalPanel(
        "output.hasSimData == false",
        div(class="jumbotron", 
            style = "text-align:center; background-color: #f8f9fa; border: 1px solid #ddd;",
            h2("No Data Available"), 
            p("Please run the simulation in Step 1."),
            icon("arrow-circle-left", "fa-3x")
        )
      ),
      conditionalPanel("output.hasSimData == true",
                       h3("Comparing All Scenarios"),
                       p("Comparison of the 'Infected' curves (Mean + CI) across all intervention strategies."),
                       plotlyOutput("plot_comparison", height = "600px")
      ),
      
      hr(),
      uiOutput("auto_interpretation_ui")
    )
  ),
  
  # --- Step 4: Statistical Analysis ---
  tabPanel(
    "Step 4: Statistical Analysis",
    fluidPage(
      br(),
      conditionalPanel(
        "output.hasSimData == false",
        div(class="jumbotron", 
            style = "text-align:center; background-color: #f8f9fa; border: 1px solid #ddd;",
            h2("No Data Available"), 
            p("Please run the simulation in Step 1."),
            icon("arrow-circle-left", "fa-3x")
        )
      ),
      conditionalPanel(
        "output.hasSimData == true",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h4("Model Setup", style="color:#2c3e50; font-weight:bold; border-bottom: 3px solid #2c3e50; padding-bottom:5px;"),
            
            # 1. Outcome Selection
            div(class = "panel panel-default", style="border-left: 5px solid #3498db;",
                div(class = "panel-body", style="padding:15px;",
                    tags$label("Outcome Variable (Y):", style="font-size:1.1em;"),
                    selectInput("stat_outcome", NULL, 
                                choices = c("Peak Prevalence (%)" = "peak_prev", 
                                            "Day of Peak" = "time_peak"),
                                selected = "peak_prev")
                )
            ),
            
            # 2. Predictor Selection
            div(class = "panel panel-default", style="border-left: 5px solid #e67e22;",
                div(class = "panel-body", style="padding:15px;",
                    tags$label("Predictors (X):", style="font-size:1.1em;"),
                    checkboxGroupInput("stat_predictors", NULL,
                                       choices = c("Social Distancing (sd)" = "sd", 
                                                   "Mask Effectiveness (mask)" = "mask",
                                                   "Lockdown (Binary)" = "is_lockdown"),
                                       selected = c("sd", "mask", "is_lockdown")),
                    hr(),
                    checkboxInput("use_interactions", strong("Fit Full Interactions (X*X)"), value = TRUE)
                )
            ),
            
            actionButton("run_lm", "Run Analysis", 
                         class="btn-primary btn-lg btn-block", 
                         style="font-weight:bold; font-size:1.1em;", icon = icon("rocket")),
            
            br(),
            # Diagnostics Box
            div(style="background:#fff; border:2px solid #333; padding:15px; border-radius:0px; margin-top:20px;",
                h4("Model Fit Stats", style="margin-top:0; font-weight:bold; color:#333;"),
                uiOutput("lm_metrics_box")
            )
          ),
          
          mainPanel(
            width = 9,
            
            # --- SECTION 1: EDA & HEATMAP ---
            h3("1. Data Exploration & Correlations", style = "font-weight:800; color:#2c3e50; letter-spacing:-0.5px;"),
            fluidRow(
              column(7, 
                     h5("Outcome Distributions (Boxplot + Points)", style="font-weight:bold; color:#555;"),
                     plotlyOutput("plot_boxplot_main", height = "400px")
              ),
              column(5, 
                     h5("Correlation Heatmap (Predictors vs Outcomes)", style="font-weight:bold; color:#555;"),
                     plotOutput("plot_heatmap", height = "400px")
              )
            ),
            
            hr(style="border-top: 2px solid #ccc; margin: 40px 0;"),
            
            # --- SECTION 2: INTERACTION EFFECTS ---
            h3("2. Interaction Effects (Visualizing Synergy)", style = "font-weight:800; color:#2c3e50; letter-spacing:-0.5px;"),
            p("Non-parallel lines indicate that the effectiveness of one intervention depends on the presence of another.", style="font-size:1.1em; color:#666;"),
            
            uiOutput("interaction_plots_ui"),
            
            hr(style="border-top: 2px solid #ccc; margin: 40px 0;"),
            
            # --- SECTION 3: COEFFICIENTS & DIAGNOSTICS ---
            h3("3. Model Coefficients & Diagnostics", style = "font-weight:800; color:#2c3e50; letter-spacing:-0.5px;"),
            
            fluidRow(
              column(6,
                     h4("Effect Sizes (Coefficients)", style="font-weight:bold;"),
                     plotOutput("plot_coefs_fancy", height = "500px")
              ),
              column(6,
                     h4("Residual Diagnostics", style="font-weight:bold;"),
                     tabsetPanel(
                       tabPanel("Detailed Summary", br(), verbatimTextOutput("lm_summary_full")),
                       tabPanel("Residuals vs Fitted", br(), plotOutput("plot_resid_fancy", height = "420px")),
                       tabPanel("Normal Q-Q Plot", br(), plotOutput("plot_qq_fancy", height = "420px"))
                     )
              )
            ),
            br(),
            uiOutput("model_limitation_alert"),
            
            hr(style="border-top: 2px solid #ccc; margin: 40px 0;"),
            
            # --- SECTION 4: DATASET ---
            h3("4. Simulation Dataset", style = "font-weight:800; color:#2c3e50;"),
            div(style = "margin-bottom: 10px;",
                downloadButton("download_csv", "Download Data (.csv)", class = "btn-primary")
            ),
            DTOutput("data_table_full")
          )
        )
      )
    )
  ),
)













# ======================
# 2. Server
# ======================

server <- function(input, output, session) {
  
  # Reactive Values to store data
  # store_all_reps: list of list(summary=df, results=matrix, params=list) for every single run
  # organized by scenario -> repetition
  data_store <- reactiveVal(NULL) 
  
  # Output flag
  output$hasSimData <- reactive({ !is.null(data_store()) })
  outputOptions(output, "hasSimData", suspendWhenHidden = FALSE)
  
  # =======================================================
  # --- START: Step 1 UI Logic (Literature & Scenarios) ---
  # =======================================================
  
  # 1. Create literature data reference
  ref_data <- reactive({
    # basic data frame
    df <- data.frame(
      Source = c("Kucharski et al.", "Li, Leung and Leung", "Wu et al.", "WHO Initial Estimate", 
                 "WHO-China Joint Mission", "Liu et al.", "Rocklöv, Sjödin and Wilder-Smith", 
                 "Backer, Klinkenberg, Wallinga", "Read et al.", "Bi et al.", "Tang et al."),
      
      Location = c("Wuhan", "Wuhan", "Greater Wuhan", "Hubei", 
                   "Hubei", "Guangdong", "Princess Diamond", 
                   "Wuhan", "Wuhan", "Shenzhen", "China"),
      
      
      R0_disp   = c("3.0 (1.5 — 4.5)", "2.2 (1.4 — 3.9)", "2.68 (2.47 — 2.86)", "1.95 (1.4 — 2.5)", 
                    "2.25 (2.0 — 2.5)", "4.5 (4.4 — 4.6)", "14.8", 
                    "N/A", "3.11 (2.39 — 4.13)", "N/A", "6.47 (5.71 — 7.23)"),
      
      T_inc_disp = c("5.2", "5.2 (4.1 — 7.0)", "6.1", "N/A", 
                     "5.5 (5.0 — 6.0)", "4.8 (2.2 — 7.4)", "5.0", 
                     "6.5 (5.6 — 7.9)", "N/A", "4.8 (4.2 — 5.4)", "N/A"),
      
      T_inf_disp = c("2.9", "2.3 (0.0 — 14.9)", "2.3", "N/A", 
                     "N/A", "2.9 (0 — 5.9)", "10.0", 
                     "N/A", "N/A", "1.5 (0 — 3.4)", "N/A"),
      
      # --- (Mean) ---
      # if (N/A)，use default number: R0=3.0, Inc=5.2, Inf=2.9
      R0_val    = c(3.0,  2.2,  2.68, 1.95, 2.25, 4.5, 14.8, 2.5,  3.11, 2.5,  6.47),
      T_inc_val = c(5.2,  5.2,  6.1,  5.2,  5.5,  4.8, 5.0,  6.5,  5.2,  4.8,  5.2),
      T_inf_val = c(2.9,  2.3,  2.3,  2.9,  2.9,  2.9, 10.0, 2.9,  2.9,  1.5,  2.9),
      
      # DOI
      doi_link = c(
        "https://doi.org/10.1016/S1473-3099(20)30144-4", # Kucharski
        "https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",     # Li
        "https://doi.org/10.1016/S0140-6736(20)30260-9", # Wu
        "https://www.who.int/news-room/detail/23-01-2020-statement-on-the-meeting-of-the-international-health-regulations-(2005)-emergency-committee-regarding-the-outbreak-of-novel-coronavirus-(2019-ncov)",# WHO
        "https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf", # WHO Joint
        "https://www.biorxiv.org/content/10.1101/2020.01.25.919787v2", # Liu
        "https://doi.org/10.1093/jtm/taaa030",           # Rocklöv
        "https://doi.org/10.2807/1560-7917.ES.2020.25.5.2000062", # Backer
        "https://doi.org/10.1101/2020.01.23.20018549",   # Read (Preprint)
        "https://doi.org/10.1016/S1473-3099(20)30287-5", # Bi
        "https://www.mdpi.com/2077-0383/9/2/462"     # Tang
      )
    )
    
    
    df$Source_HTML <- paste0("<a href='", df$doi_link, "' target='_blank'>", df$Source, "</a>")
    
    # calculate Gamma, Sigma, Beta
    df %>%
      mutate(
        gamma_calc = round(1 / T_inf_val, 2),
        sigma_calc = round(1 / T_inc_val, 2),
        beta_calc  = round(R0_val * (1 / T_inf_val), 2)
      )
  })
  
  # 2. DT table for literature
  output$ref_param_table <- renderDT({

    display_df <- ref_data() %>%
      select(Source_HTML, Location, R0_disp, T_inc_disp, T_inf_disp, beta_calc, sigma_calc, gamma_calc)
    
    datatable(display_df, 
              selection = 'single',
              escape = FALSE, 
              options = list(dom = 't', paging = FALSE, ordering = FALSE),
              rownames = FALSE,
              colnames = c("Source Study", "Location", "R0 (95% CI)", 
                           "Incubation Days (CI)", "Infectious Days (CI)", 
                           "Beta", "Sigma", "Gamma")
    ) %>%
      formatStyle(
        c('beta_calc', 'sigma_calc', 'gamma_calc'),
        backgroundColor = styleInterval(0, c('white', '#e0f7fa')),
        fontWeight = 'bold'
      )
  })
  
  # 3. Interaction: Clicking a table row automatically updates the sliders.
  observeEvent(input$ref_param_table_rows_selected, {
    selected_idx <- input$ref_param_table_rows_selected
    req(selected_idx)
    
    row_data <- ref_data()[selected_idx, ]
    
    updateSliderInput(session, "beta", value = row_data$beta_calc)
    updateSliderInput(session, "sigma", value = row_data$sigma_calc)
    updateSliderInput(session, "gamma", value = row_data$gamma_calc)
    
    showNotification(HTML(paste("Loaded parameters from: <b>", row_data$Source, "</b>")), type = "message")
  })
  
  # 4. Render Scenario Definition Table (Step 1 Part B)
  output$scenario_desc_table <- renderTable({
    data.frame(
      Scenario = c("1. No Prevention Measures", "2. Lockdown", "3. Conditional Lockdown", 
                   "4. Vertical Isolation", "5. Partial Isolation", "6. Mask Only", "7. Mask + Isolation"),
      Description = c(
        "Baseline: No interventions (SD=0, Mask=0).",
        "Strict Stay-at-Home: Contacts reduced by 90% (SD=0.9).",
        "Dynamic: Lockdown triggered if infected population is high.",
        "Vulnerable groups isolate (Mild SD=0.2).",
        "50% of population isolates (SD=0.5).",
        "Universal masking (Eff=0.6) with mild distancing.",
        "Combined Strategy: Masks (Eff=0.6) + 50% Isolation."
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  
  # Helper function: Generate Scenario Parameters
  get_scen_params <- function(scen_id, base_input) {
    p <- list(
      n_agents = base_input$n_agents, 
      init_infected = base_input$init_infected, init_exposed = 0,
      beta = base_input$beta, sigma = base_input$sigma, gamma = base_input$gamma,
      contact_rate = 15, social_distancing = 0, mask_effectiveness = 0,
      is_lockdown = 0 # Dummy for regression
    )
    
    # Define scenario logic (Simplified for clarity)
    if(scen_id == "scen1") { # Do Nothing
    } else if(scen_id == "scen2") { # Lockdown
      p$social_distancing <- 0.9; p$is_lockdown <- 1
    } else if(scen_id == "scen3") { # Conditional
      p$social_distancing <- 0.7; p$is_lockdown <- 1
    } else if(scen_id == "scen4") { # Vertical
      p$social_distancing <- 0.2
    } else if(scen_id == "scen5") { # Partial
      p$social_distancing <- 0.5
    } else if(scen_id == "scen6") { # Mask
      p$mask_effectiveness <- 0.6; p$social_distancing <- 0.2
    } else if(scen_id == "scen7") { # Mask + Iso
      p$mask_effectiveness <- 0.6; p$social_distancing <- 0.5
    }
    return(p)
  }
  
  # --- RUN SIMULATION ---
  observeEvent(input$run_sim, {
    safe_n_agents <- min(input$n_agents, 5000)
    n_reps <- input$n_reps
    all_data <- list()
    
    withProgress(message = 'Running Simulations...', value = 0, {
      
      for(scen_id in scenario_ids) {
        
        # Scenario Parameters
        params <- get_scen_params(scen_id, input)
        params$n_agents <- safe_n_agents
        reps_list <- list()
        
        for(r in 1:n_reps) {
          # Run Single Simulation
          # Note: We don't set a fixed seed here, so every rep is different!
          res <- simulate_abm_single(params, input$n_days, 1) # time_step=1 for speed
          res$rep_id <- r
          res$params <- params
          #save memory
          if (r > 1) {
            res$states_mat <- NULL
            res$agent_coords <- NULL
          }
          reps_list[[r]] <- res
        }
        
        all_data[[scen_id]] <- list(
          name = scenario_labels[[scen_id]],
          reps = reps_list
        )
        
        gc()
        
        incProgress(1/7, detail = paste("Scenario", scen_id))
      }
    })
    
    data_store(all_data)
    
    # Update Time Slider range
    updateSliderInput(session, "anim_time", max = input$n_days, value = 0)
    showNotification("Simulation Complete! Check Results tab.", type = "message")
  })
  
  
  
  # --- Data Aggregation Helper function ---
  # Combine all repetitions into one long DF for plotting
  get_aggregated_df <- reactive({
    req(data_store())
    lst <- data_store()
    
    big_df <- purrr::map_dfr(names(lst), function(sid) {
      scen_obj <- lst[[sid]]
      
      # Extract summaries from all reps
      purrr::map_dfr(scen_obj$reps, function(r) {
        r$summary %>% mutate(rep = r$rep_id)
      }) %>%
        mutate(scenario = scen_obj$name, scen_id = sid)
    })
    return(big_df)
  })
  
  
  
  # --- Step 2: Interactive Time Series (Plotly) ---
  output$plot_ts_ribbon <- renderPlotly({
    req(data_store(), input$anim_time)
    
    df <- get_aggregated_df() %>%
      filter(scen_id == input$view_scen)
    
    # mean and CI
    df_sum <- df %>%
      group_by(time, compartment) %>%
      summarise(
        mean_count = mean(count),
        sd_count = sd(count),
        .groups = 'drop'
      ) %>%
      mutate(
        ci_lower = pmax(0, mean_count - 1.96 * sd_count),
        ci_upper = mean_count + 1.96 * sd_count
      )
    
    current_day <- round(input$anim_time)
    
    # Mean value for the current day(obtained by averaging across all simulation repetitions)
    current_snapshot <- df %>%
      filter(time == current_day) %>%
      group_by(compartment) %>%
      summarise(mean_count = mean(count), .groups = 'drop')
    
    p <- plot_ly()
    
    # 1. Ribbon + line
    for(comp in c("S", "E", "I", "R")) {
      d <- df_sum %>% filter(compartment == comp)
      col <- my_colors[[comp]]
      
      p <- p %>%
        add_ribbons(data = d, x = ~time, ymin = ~ci_lower, ymax = ~ci_upper,
                    fillcolor = col, opacity = 0.15, line = list(width = 0),
                    name = paste(comp, "95% CI"), showlegend = FALSE) %>%
        add_lines(data = d, x = ~time, y = ~mean_count,
                  line = list(color = col, width = 3), name = comp)
    }
    
    # 2. Add a vertical line for the current day (highlighted)
    p <- p %>% add_lines(x = c(current_day, current_day), y = c(0, max(df_sum$ci_upper)),
                         line = list(color = "black", width = 2, dash = "dash"),
                         name = "Current Day", showlegend = FALSE)
    
    # 3. Add 4 large markers for the current day (S/E/I/R)
    for(comp in c("S", "E", "I", "R")) {
      val <- current_snapshot$mean_count[current_snapshot$compartment == comp]
      if(length(val) == 0) val <- NA
      p <- p %>% add_markers(x = current_day, y = val,
                             marker = list(color = my_colors[[comp]], size = 12,
                                           line = list(color = "black", width = 2)),
                             name = paste(comp, "(Current)"),
                             showlegend = FALSE)
    }
    
    p %>% layout(
      title = paste("Dynamics:", scenario_labels[[input$view_scen]], 
                    "  |  Current Day:", current_day),
      xaxis = list(title = "Time (Days)"),
      yaxis = list(title = "Population Count"),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2)
    )
  })
  
  # --- Step 2: Spatial/Grid Visualization-Animation ---
  output$plot_spatial <- renderPlot({
    req(data_store(), input$anim_time)
    scen_data <- data_store()[[input$view_scen]]
    rep1 <- scen_data$reps[[1]]
    
    day <- round(input$anim_time)
    day <- pmin(day, input$n_days) 
    
    row_idx <- day + 1  # day 0 → row 1, day 100 → row 101
    
    current_states <- rep1$states_mat[row_idx, ]
    
    plot_df <- rep1$agent_coords
    plot_df$state <- factor(current_states, levels = c("S","E","I","R"))
    
    ggplot(plot_df, aes(x=x, y=y, color=state)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(values = my_colors, drop = FALSE) +
      theme_void() +
      labs(title = paste("Spatial Spread - Day", day)) +
      theme(plot.title = element_text(size=20, face="bold", hjust=0.5),
            legend.position = "bottom")
  })
  
  output$spatial_stats_table <- renderTable({
    req(data_store(), input$anim_time)
    scen_data <- data_store()[[input$view_scen]]
    rep1 <- scen_data$reps[[1]]
    
    day <- pmin(round(input$anim_time), input$n_days)
    
    rep1$summary %>%
      filter(time == day) %>%
      select(compartment, count) %>%
      pivot_wider(names_from = compartment, values_from = count)
  }, digits = 0)
  
  
  # --- Step 3: Comparison Plot (Plotly) ---
  output$plot_comparison <- renderPlotly({
    req(data_store())
    df <- get_aggregated_df() %>%
      filter(compartment == "I") %>% # Only compare Infected
      group_by(time, scenario) %>%
      summarise(mean_I = mean(count), .groups='drop')
    
    plot_ly(df, x = ~time, y = ~mean_I, color = ~scenario, 
            type = 'scatter', mode = 'lines', line = list(width=2)) %>%
      layout(
        title = "Comparison of Infection Curves (Mean)",
        xaxis = list(title = "Time (Days)"),
        yaxis = list(title = "Infected Count")
      )
  })
  
# Interpretation for step 3
  
  auto_interpretation <- reactive({
    req(stats_df())
    
    df <- stats_df() %>%
      group_by(scenario) %>%
      summarise(
        peak_prev = mean(peak_prev),
        time_peak = mean(time_peak),
        .groups = "drop"
      ) %>%
      arrange(peak_prev)
    
    best <- df$scenario[1]                   # lowest peak
    worst <- df$scenario[nrow(df)]          # highest peak
    
    slowest <- df$scenario[which.max(df$time_peak)] # latest peak (most delayed)
    fastest <- df$scenario[which.min(df$time_peak)] # earliest outbreak
    
    glue::glue("
    **Interpretation**
    
  • The intervention achieving the **lowest epidemic peak** is: **{best}**, 
    indicating the strongest reduction in transmission.
  
  • The intervention with the **highest peak** is: **{worst}**, 
    reflecting minimal control of β_eff.
  
  • The **most delayed epidemic peak** occurs in: **{slowest}**, 
    showing its effect in slowing transmission and flattening the curve.
  
  • The **earliest peak** occurs in: **{fastest}**, 
    indicating rapid epidemic spread under weaker interventions.
    
  • **Vertical Isolation Doesn't Work**: isolating only vulnerable groups (Scenario 4) 
    performs just as poorly as No Prevention (Scenario 1). 
    Both show rapid, uncontrollable outbreaks.
  
  • Overall, **Only strong interventions delay the peak significantly** (past Day 20).
    While Lockdown (Scenario 2) best flattens the curve, it is costly. 
    **Scenario 7 (Mask + Isolation) is a practical compromise**, 
    which combines strategies to significantly reduce spread without a full shutdown, 
    buying critical time for the healthcare system.
  ")
  })
  output$auto_interpretation_ui <- renderUI({
    HTML(markdown::markdownToHTML(text = auto_interpretation(), fragment.only = TRUE))
  })
  


# --- Step 4: Statistical Analysis ---
  
  # 1. Data Prep
  stats_df <- reactive({
    req(data_store())
    lst <- data_store()
    
    purrr::map_dfr(lst, function(scen) {
      purrr::map_dfr(scen$reps, function(r) {
        summ <- r$summary
        peak_I <- max(summ$count[summ$compartment == "I"])
        time_peak <- summ$time[which.max(summ$count[summ$compartment == "I"])][1]
        
        tibble(
          scenario = scen$name,
          sd = r$params$social_distancing,
          mask = r$params$mask_effectiveness,
          is_lockdown = r$params$is_lockdown,
          peak_prev = peak_I / input$n_agents * 100,
          time_peak = time_peak
        )
      })
    })
  })
  
  # 2. Fit Model
  lm_model <- reactive({
    df <- stats_df()
    req(df, nrow(df) > 0)
    
    req(input$stat_outcome) 
    preds <- input$stat_predictors
    
    if (length(preds) == 0) {
      showNotification("Select at least one predictor!", type = "error")
      return(NULL)
    }
    
    rhs <- paste(preds, collapse = " + ")
    if (input$use_interactions && length(preds) > 1) {
      rhs <- paste0("(", rhs, ")^2") 
    }
    
    f <- as.formula(paste(input$stat_outcome, "~", rhs))
    
    lm(f, data = df)
  })
  
  # =========================================================
  # PLOT 1: Violin Plot + Inner Box
  # =========================================================
  output$plot_boxplot_main <- renderPlotly({
    req(stats_df())
    df <- stats_df()
    y_label <- if(input$stat_outcome=="peak_prev") "Peak Prevalence (%)" else "Day of Peak"
    my_palette <- c(
      "#4E79A7",
      "#F28E2B",
      "#76B7B2",
      "#59A14F",
      "#EDC948",
      "#B07AA1",
      "#FF9DA7" 
    )
    
    plot_ly(df, x = ~scenario, y = ~get(input$stat_outcome), 
            type = 'violin',
            box = list(visible = TRUE, width = 0.2, fillcolor="#f9f9f9", line=list(color="#333", width=1)), 
            
            points = 'all',
            jitter = 0.4,
            pointpos = 0,
            marker = list(size = 4, opacity = 0.4, line=list(width=0)),
            
            color = ~scenario, 
            colors = my_palette,
            
            hoverinfo = "y+text",
            showlegend = FALSE) %>%
      layout(yaxis = list(title = y_label, titlefont=list(size=14), zeroline=FALSE, showgrid=TRUE, gridcolor="#eee"), 
             xaxis = list(title = "", showticklabels=TRUE, tickangle=45, zeroline=FALSE),
             margin = list(b=80),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)'
      )
  })
  
  # =========================================================
  # PLOT 2: Correlation Heatmap
  # =========================================================
  output$plot_heatmap <- renderPlot({
    req(stats_df())
    df <- stats_df()
    
    # 1. data processing
    # Select numeric columns relevant for correlation
    cor_data <- df %>% 
      select(sd, mask, is_lockdown, peak_prev, time_peak) %>%
      rename("SD" = sd, "Mask Eff." = mask, "Lockdown" = is_lockdown, 
             "Peak Prev(%)" = peak_prev, "Day of Peak" = time_peak)
    
    cor_mat <- cor(cor_data, use = "complete.obs")
    
    # Reshape for ggplot
    cor_melt <- as.data.frame(cor_mat) %>%
      tibble::rownames_to_column("Var1") %>%
      pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")
    
    var_order <- colnames(cor_data)
    cor_melt$Var1 <- factor(cor_melt$Var1, levels = var_order)
    cor_melt$Var2 <- factor(cor_melt$Var2, levels = rev(var_order))
    
    # 2. plot
    ggplot(cor_melt, aes(Var1, Var2, fill = Correlation)) +
      geom_tile(color = "white", size = 1.2) +
      geom_text(aes(label = sprintf("%.2f", Correlation),
                    color = abs(Correlation) > 0.6), 
                size = 5, fontface = "bold", show.legend = FALSE) +

      scale_fill_gradientn(colors = c("#91d8d2", "#a2dfda", "#b5e6e1", "#def3f2", "#fef2f2", "#fad1d3", "#f89a9a"),
                           limits = c(-1, 1), name="Corr.\nCoeff.") +
      scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black")) +
      coord_fixed() +
      labs(x=NULL, y=NULL) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face="bold", color="#2c3e50"),
        axis.text.y = element_text(face="bold", color="#2c3e50"),
        legend.position = "right",
        legend.title = element_text(size=10, face="bold"),
        panel.grid = element_blank(),
        plot.margin = margin(20, 20, 20, 20) 
      )
  })
  
  # =========================================================
  # PLOT 3: Interaction Grid
  # =========================================================
  output$interaction_plots_ui <- renderUI({
    req(lm_model())
    preds <- input$stat_predictors
    if(length(preds) < 2) return(div(class="alert alert-warning", "Select 2+ predictors to see interactions."))
    
    plot_list <- list()
    if("sd" %in% preds && "mask" %in% preds) plot_list[[length(plot_list)+1]] <- "plot_int_sd_mask"
    if("sd" %in% preds && "is_lockdown" %in% preds) plot_list[[length(plot_list)+1]] <- "plot_int_sd_lock"
    if("mask" %in% preds && "is_lockdown" %in% preds) plot_list[[length(plot_list)+1]] <- "plot_int_mask_lock"
    
    if(length(plot_list) == 0) return(NULL)
    do.call(fluidRow, lapply(plot_list, function(id) {
      column(4, plotOutput(id, height = "320px"))
    }))
  })
  
  # Helper Theme for Chunky Plots
  theme_chunky <- theme_bw(base_size = 14) + 
    theme(plot.title = element_text(face="bold"), 
          legend.position = "bottom",
          panel.border = element_rect(colour = "black", fill=NA, size=1.5))
  
  output$plot_int_sd_mask <- renderPlot({
    req(stats_df())
    ggplot(stats_df(), aes(x = sd, y = get(input$stat_outcome), color = factor(mask))) +
      geom_point(alpha=0.3, size=3) + 
      geom_smooth(method="lm", se=FALSE, size=2) + # Thicker lines
      labs(title = "SD × Mask", x = "Social Distancing", y = input$stat_outcome, color="Mask") +
      scale_color_viridis_d(option="plasma") + theme_chunky
  })
  
  output$plot_int_sd_lock <- renderPlot({
    req(stats_df())
    ggplot(stats_df(), aes(x = sd, y = get(input$stat_outcome), color = factor(is_lockdown))) +
      geom_point(alpha=0.3, size=3) + 
      geom_smooth(method="lm", se=FALSE, size=2) +
      labs(title = "SD × Lockdown", x = "Social Distancing", y = input$stat_outcome, color="Lockdown") +
      scale_color_manual(values = c("0"="#3498db", "1"="#e74c3c")) + theme_chunky
  })
  
  output$plot_int_mask_lock <- renderPlot({
    req(stats_df())
    ggplot(stats_df(), aes(x = mask, y = get(input$stat_outcome), color = factor(is_lockdown))) +
      geom_point(alpha=0.3, size=3) + 
      geom_smooth(method="lm", se=FALSE, size=2) +
      labs(title = "Mask × Lockdown", x = "Mask Eff.", y = input$stat_outcome, color="Lockdown") +
      scale_color_manual(values = c("0"="#3498db", "1"="#e74c3c")) + theme_chunky
  })
  
  
  # =========================================================
  #  Limitation Note
  # =========================================================
  output$model_limitation_alert <- renderUI({
    req(lm_model())
    coefs <- coef(lm_model())
    has_na <- any(is.na(coefs))
    
    if (has_na) {
      na_vars <- names(coefs)[is.na(coefs)]
      na_text <- paste(na_vars, collapse = ", ")
      
      # 1. Limitation case
      div(class="alert alert-warning", 
          style = "border-left: 5px solid #f39c12; background-color: #fdf5e6; color: #333333;", 
          
          
          h4("Analysis Limitation: Multicollinearity", style="font-weight:bold; margin-top:0; color: #8a6d3b;"), 
          
          
          p("The interaction term ", code(na_text), " is estimated as NA."),
          
          # Interpretation
          p(strong("Interpretation:"), "This is a limitation, a reflection of the study design. In simulation scenarios we based on the article, when 'Lockdown' = 1, 'Mask Usage' = 0 permanently."),
          
          # Future Studies
          p("In future studies, we can modify those different scenarios and limit this issue based on other articles or try our own parameters.")
      )
    } else {
      # 2. normal case
      return(NULL)
    }
  })
  
  # =========================================================
  # PLOT 4: Coefficients
  # =========================================================
  output$plot_coefs_fancy <- renderPlot({
    req(lm_model())
    
    # 1. data processing
    d <- broom::tidy(lm_model(), conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      filter(!is.na(estimate)) %>%  #out NA estimate
      mutate(term_label = gsub(":", " × ", term),
             color_grp = ifelse(estimate < 0, "Reduces Outcome", "Increases Outcome"))
    
    # 2. plot
    ggplot(d, aes(x = reorder(term_label, abs(estimate)), y = estimate, color = color_grp)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size=1) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1.5) +
      geom_point(size = 6) +
      geom_text(aes(label = round(estimate, 2)), vjust = -0.8, fontface = "bold", size=5, show.legend=FALSE) +
      coord_flip() +
      scale_color_manual(values = c("Reduces Outcome" = "#27ae60", 
                                    "Increases Outcome" = "#c0392b")) +
      labs(x = NULL, y = "Coefficient Estimate", color = "Effect Direction") +
      theme_bw(base_size = 14) +
      theme(legend.position = "top", 
            panel.grid.major.y = element_blank())
  })
  
  # =========================================================
  # PLOT 5: Diagnostics
  # =========================================================
  
  # 1. Residuals vs Fitted
  output$plot_resid_fancy <- renderPlot({
    req(lm_model())
    df_aug <- broom::augment(lm_model())
    
    ggplot(df_aug, aes(x = .fitted, y = .resid)) +
      geom_hline(yintercept = 0, color = "black", size=1) +
      geom_point(aes(color = abs(.resid)), size = 3, alpha = 0.7) + # Color by error magnitude
      geom_smooth(method = "loess", color = "#e74c3c", se = FALSE, linetype = "solid", size=1.5) +
      scale_color_viridis_c(option = "magma", direction = -1) +
      labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
      theme_bw(base_size = 14) + theme(legend.position = "none")
  })
  
  # 2. Q-Q Plot
  output$plot_qq_fancy <- renderPlot({
    req(lm_model())
    df_aug <- broom::augment(lm_model())
    
    ggplot(df_aug, aes(sample = .resid)) +
      stat_qq(size = 3, color = "#2c3e50", alpha=0.6) +
      stat_qq_line(color = "#e74c3c", size = 1.5, linetype = "dashed") +
      labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_bw(base_size = 14)
  })
  
  output$lm_summary_full <- renderPrint({ req(lm_model()); summary(lm_model()) })
  
  # Stats Box
  output$lm_metrics_box <- renderUI({
    req(lm_model())
    g <- broom::glance(lm_model())
    div(
      div(style="display:flex; justify-content:space-between; margin-bottom:5px;", strong("Adj. R²:"), span(round(g$adj.r.squared, 3), style="font-size:1.2em; color:#2980b9;")),
      div(style="display:flex; justify-content:space-between; margin-bottom:5px;", strong("AIC:"), span(round(g$AIC, 1))),
      div(style="display:flex; justify-content:space-between; margin-bottom:5px;", strong("Sigma:"), span(round(g$sigma, 2))),
      hr(style="margin:10px 0; border-top:1px solid #ccc;"),
      div(style="display:flex; justify-content:space-between;", strong("P-Value:"), 
          span(format.pval(g$p.value, digits=3), style = if(g$p.value<0.05) "color:#27ae60; font-weight:bold;" else "color:#c0392b;"))
    )
  })
  
  output$data_table_full <- renderDT({
    req(stats_df())
    datatable(stats_df(), options = list(pageLength = 5, scrollX = TRUE), 
              class = "cell-border stripe compact") %>%
      formatRound(columns = c(5,6), digits = 2)
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("simulation_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(stats_df())
      df <- stats_df()
      write.csv(df, file, row.names = FALSE)
    })
}










# ======================
# 3. Run app
# ======================
shinyApp(ui = ui, server = server)
