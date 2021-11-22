library(shiny)
library(ggplot2)
library(dplyr)

bip = data.table::fread("data-raw/bip.csv")
b_lu = data.table::fread("data-raw/b_lu.csv")
p_lu = data.table::fread("data-raw/p_lu.csv")
batter_pool = data.table::fread("data-raw/batter-pool.csv")
pitcher_pool = data.table::fread("data-raw/pitcher-pool.csv")

shiny_seam_helper = function(b, p, br, pr, stadium = "generic") {

  seam = do_full_seam_matchup(
    .batter = lu_b(b_lu, b),
    .pitcher = lu_p(p_lu, p),
    .pitches = pitches_for_ratios,
    .bip = bip,
    .batter_pool = batter_pool,
    .pitcher_pool = pitcher_pool,
    .ratio_batter = br,
    .ratio_pitcher = pr
  )

  p1 = plot_df(seam$seam_df, stadium = stadium, batter = b, pitcher = p, main = "Full SEAM")
  p2 = plot_df(seam$synth_pitcher_df, stadium = stadium, batter = b, pitcher = p, main = "Synthetic Pitcher")
  p3 = plot_df(seam$synth_batter_df, stadium = stadium, batter = b, pitcher = p, main = "Synthetic Batter")
  p4 = plot_df(seam$empirical_df, stadium = stadium, batter = b, pitcher = p, main = "Empirical Matchup")
  p5 = plot_df(seam$empirical_pitcher_df, stadium = stadium, batter = b, pitcher = p, main = "Empirical Pitcher")
  p6 = plot_df(seam$empirical_batter_df, stadium = stadium, batter = b, pitcher = p, main = "Empirical Batter")

  list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, p6 = p6)

}

ui = fluidPage(

  titlePanel("SEAM: Synthetic Estimated Average Matchup"),

  theme = bslib::bs_theme(bootswatch = "united", primary = "#31a354"),

  sidebarLayout(

    sidebarPanel(
      selectInput("pitcher", label = "Pitcher", choices = sort(unique(p_lu$pitcher_name)), selected = "Justin Verlander", selectize = TRUE),
      sliderInput("p_ratio", "Ratio of Stuff to Release", min = .50, max = 1, value = .85, step = .01),
      selectInput("batter", label = "Batter", choices = sort(unique(b_lu$batter_name)), selected = "Mike Trout", selectize = TRUE),
      sliderInput("b_ratio", "Ratio of LA/EV to Batted Ball Location", min = 0, max = 1, value = .85, step = .01),
      selectInput("stadium", label = "Stadium", choices = unique(GeomMLBStadiums::MLBStadiumsPathData$team), selected = "white_sox", selectize = TRUE)
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

server = function(input, output) {

  matchup = reactive({
    shiny_seam_helper(
      b = input$batter,
      p = input$pitcher,
      br = input$b_ratio,
      pr = input$p_ratio,
      stadium = input$stadium
    )
  })

  output$plot_full = renderPlot({
    matchup()$p1
  })

  output$plot_comp = renderPlot({
    gridExtra::grid.arrange(matchup()$p4, matchup()$p2, matchup()$p3, ncol = 3)
  })

  output$plot_all = renderPlot({
    gridExtra::grid.arrange(
      matchup()$p1,
      matchup()$p2,
      matchup()$p3,
      matchup()$p4,
      matchup()$p5,
      matchup()$p6,
      ncol = 3
    )
  })

}

shinyApp(ui = ui, server = server)
