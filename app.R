library("shiny", quietly = TRUE, warn.conflicts = FALSE)
library("ggplot2", quietly = TRUE, warn.conflicts = FALSE)
library("dplyr", quietly = TRUE, warn.conflicts = FALSE)
library("markdown", quietly = TRUE, warn.conflicts = FALSE)
library("jsonlite", quietly = TRUE, warn.conflicts = FALSE)
library("shiny", quietly = TRUE, warn.conflicts = FALSE)
library("reticulate", quietly = TRUE, warn.conflicts = FALSE)

py_install(c("google-genai", "numpy", "python-dotenv"), pip = TRUE)    # installing python packages on deployment.
print(py_config())

bip = readRDS("data/bip.Rds")
b_lu = data.frame(readRDS("data/b-lu.Rds")) # why is this so much faster as a data frame
p_lu = data.frame(readRDS("data/p-lu.Rds")) # why is this so much faster as a data frame
batter_pool = readRDS("data/batter-pool.Rds")
pitcher_pool = readRDS("data/pitcher-pool.Rds")
mlb_teams = readRDS("data/mlb-teams.Rds")
stadiums = readRDS("data/stadiums.Rds")
stadium_paths = readRDS("data/stadium-paths.Rds")

shiny_seam_helper = function(b, p, br, pr) {
  do_full_seam_matchup(
    .batter = lu_b(b_lu, b),
    .pitcher = lu_p(p_lu, p),
    .bip = bip,
    .batter_pool = batter_pool,
    .pitcher_pool = pitcher_pool,
    .ratio_batter = br,
    .ratio_pitcher = pr
  )
}

shiny_plot_helper = function(seam, stadium = "generic") {
  p1 = plot_df(seam$seam_df, stadium = stadium, main = "Full SEAM")
  p2 = plot_df(seam$synth_pitcher_df, stadium = stadium, main = "Synthetic Pitcher")
  p3 = plot_df(seam$synth_batter_df, stadium = stadium, main = "Synthetic Batter")
  p4 = plot_df(seam$empirical_df, stadium = stadium, main = "Empirical Matchup")
  p5 = plot_df(seam$empirical_batter_df, stadium = stadium, main = "Empirical Batter")
  p6 = plot_df(seam$empirical_pitcher_df, stadium = stadium, main = "Empirical Pitcher")
  list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, p6 = p6)
}

ui = fluidPage(
  titlePanel("SEAM: Synthetic Estimated Average Matchup"),
  theme = bslib::bs_theme(bootswatch = "united", primary = "#31a354"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pitcher", label = "Pitcher", choices = unique(p_lu$pitcher_name), selected = "Justin Verlander"),
      sliderInput("p_ratio", "Ratio of Stuff to Release", min = 0, max = 1, value = .85, step = .01),
      hr(),
      selectInput("batter", label = "Batter", choices = unique(b_lu$batter_name), selected = "Mike Trout"),
      sliderInput("b_ratio", "Ratio of LA/EV to Batted Ball Location", min = 0, max = 1, value = .85, step = .01),
      hr(),
      selectInput("stadium", label = "Stadium", choices = stadiums, selected = "angels"),
      hr(),
      shiny::markdown('<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Google_Gemini_logo.svg/500px-Google_Gemini_logo.svg.png" width=55/><br>'),
      uiOutput("llm_analysis"),
      shiny::markdown('<small style="color: grey; text-align: center; display: block;">Gemini can make mistakes. For reference only.</small>')
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Full SEAM", plotOutput("plot_full", height = 600)),
                  tabPanel("SEAM Components", plotOutput("plot_comp")),
                  tabPanel("All Plots", plotOutput("plot_all"))
      )
    )
  )
)

server = function(input, output, session) {
  output$rendered_markdown <- renderUI({
    markdown::markdownToHTML(text = input$markdown_input, fragment.only = TRUE)
  })

  observeEvent(input$batter, {
    freezeReactiveValue(input, "stadium")
    updateSelectInput(inputId = "stadium",
                      selected = get_batter_stadium(.b_lu = b_lu,
                                                    teams = mlb_teams,
                                                    .batter = input$batter))
  })

  matchup = reactive({
    shiny_seam_helper(
      b = input$batter,
      p = input$pitcher,
      br = input$b_ratio,
      pr = input$p_ratio
    )
  })

  plots = reactive({
    shiny_plot_helper(
      seam = matchup(),
      stadium = input$stadium
    )
  })

  output$plot_full = renderPlot({
    plots()$p1
  })

  llm_response = reactiveVal("")    # Store LLM response
  llm_loading = reactiveVal(FALSE)

  observeEvent(list(input$batter, input$pitcher), {
    seam_matchup <- matchup()
    seam_json <- jsonlite::toJSON(seam_matchup$seam_df)
    write(seam_json, "LLM/seam_data.json")

    python_exe <- reticulate::py_exe()

    cmd <- sprintf(
      '%s LLM/llm.py "%s" "%s"',
      python_exe,
      input$pitcher,
      input$batter
    )

    print(paste("Running:", cmd))

    result <- system(cmd, intern = TRUE)
    llm_response(paste(result, collapse = "\n"))
    llm_loading(FALSE)
  }, ignoreInit = FALSE)

  output$llm_analysis = renderUI({
    shiny::markdown(llm_response())
  })

  output$plot_comp = renderPlot({
    gridExtra::grid.arrange(plots()$p4, plots()$p2, plots()$p3, ncol = 3)
  })

  output$plot_all = renderPlot({
    gridExtra::grid.arrange(
      plots()$p1,
      plots()$p2,
      plots()$p3,
      plots()$p4,
      plots()$p5,
      plots()$p6,
      ncol = 3
    )
  })

}

shinyApp(ui = ui, server = server)
