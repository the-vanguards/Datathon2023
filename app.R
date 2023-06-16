library(tidyverse)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)



# theme_back function
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}


# load data 
load("mp_shiny_sfr.rda")
load("df_cond.rda")

df_cond <- df_cond |> 
  select(-platue,-delta_p)

# UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("ATE-Targeted Mechanical Power Threshold Simulation"),
  tabsetPanel(id = "main",
    tabPanel(title = "MP Threshold",
  sidebarLayout(
    sidebarPanel(
      width = 1,
      # helpText("hello",icon("question-circle")),
      numericInput(inputId = "hr", label = "Hour:" , value = 1, min = 1, max = 96, step = 1),
      # helpText(icon("question-circle")),
      numericInput(inputId = "mp_i", label = "Mechanical Power:", value = 50, min = 1, max = 50, step = 1),
      numericInput(inputId = "mp_sd", label = "Mechanical Power (+/-sd)", value = 3, min = 1, max = 5, step = 1),
      numericInput(inputId = "p_0", label = "Noise:", value = 0.8, min = 0, max = 1, step = 0.1),
      numericInput(inputId = "umin", label = "Min Noise:", value = 1),
      numericInput(inputId = "umax", label = "Max Noise:", value = 5),
      checkboxInput(inputId = "adj_ate", label = "Remove ATE with 95% CI crossing zero", value = F),
      actionButton("update", "Update")
    ),
    mainPanel(
      width = 11,
      fluidPage(
        fluidRow(
        column(6,
         plotOutput("ateplot", height = "800px", width = "700px")),
        column(6,
          plotOutput("ateplot2", height = "800px", width = "700px"))),
        fluidRow(
          htmlOutput(outputId = "description")
        )
        
      )
    )
  )),
  tabPanel(title = "MP Element Table",
           mainPanel(
             fluidPage(
               fluidRow(DT::dataTableOutput("ate_table", width = "100%", height = "100%"))),
             fluidRow( htmlOutput(outputId = "edescription"))
             )
           ))
  )


# Server
server <- function(input, output, session) {
  # Create reactive values for the plot data
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query)) {
      for (name in names(query)) {
        if (name == "mp_element_table") {
          try(updateTabsetPanel(session, "main", selected = "MP Element Table"))
        }
      }
    }
  })
  data <- reactiveValues(df = NULL, fig_is_medical = NULL, fig_is_surgical = NULL)
  
  observeEvent(input$update, {
    category <- df_ate %>%
      pull(category) %>%
      unique()
    
    if (input$adj_ate==T) {
      df_ate <- df_ate |>
        mutate(ate = case_when(
          lower_ci < 0 & upper_ci < 0 ~ ate,
          lower_ci > 0 & upper_ci > 0 ~ ate,
          TRUE ~ NA_integer_
        ))
    }
    
    for (cat in category) {
      hr <- input$hr
      mp_i <- input$mp_i
      mp_sd <- input$mp_sd
      t <- 0
      p_1 <- input$p_0
      p_0 <- 1 - p_1
      df <- tibble(hr = hr, threshold = mp_i, t = t)
      umin <- input$umin
      umax <- input$umax
      
      set.seed(1)
      
      for (i in hr:96) {
        if (mp_i==5) { break }
        t <- sample(c(0, 1), 1, prob = c(p_0, p_1))
        if (i >= 2 & t == 1) {
          mp_i <- mp_i + runif(1, umin, umax) 
          
        }
        mp_i <- round(mp_i)
        mpmax <- mp_i + mp_sd
        
    
        mpmin <- ifelse(mp_i >= 50, 49, mp_i - mp_sd)
  
        
        y <- df_ate %>%
          filter(category == !!cat) %>%
          filter(hr == !!hr + 1) %>%
          # drop_na(ate) |>
          filter(threshold <= mpmax) %>%
          filter(threshold >= mpmin)
        
        ymax <- y %>%
          arrange(desc(ate)) %>%
          head(input$mp_sd) |>
            pull(threshold) |>
            min()
      
        
        x <- y %>%
          filter(threshold == ymax)
        
        hr <- i + 1
        mp_i_i <- x %>%
          pull(threshold)
        ate <- x %>%
          pull(ate)
        
        if(is_empty(mp_i_i)) { mp_i } else { mp_i <- mp_i_i }
        if(mp_i<=5) { mp_i <- 5 } 
        
        df <- df %>%
          add_row(hr = hr, threshold = mp_i, t = t)
      }
      
      df_condition <- df_ate %>%
        filter(ate <= 500) %>%
        filter(ate >= -500) %>%
        filter(category == !!cat)
      
      fig <- ggplot() +
        geom_contour_filled(data = df_condition, mapping = aes(x = hr, y = threshold, z = ate)) +
        # geom_tile(data = df_condition, mapping = aes(x = hr, y = threshold, fill = ate)) +
        geom_point(data = df, aes(x = hr, y = threshold, color = as.factor(t)), shape = 5, size = 2.5) +
        scale_color_manual(values = c("white", "red"), name = "MP + Noise") +
        # scale_fill_manual(name = "ATE") +
        facet_wrap(. ~ category) +
        theme_black() +
        labs(fill = "ATE (SF Ratio)") +
        xlab("Hour") +
        ylab("MP Threshold") +
        theme(legend.key.size = unit(0.2,"cm")) 
      
      data[[paste0("fig_", cat)]] <- fig
    }
    
    # ateplot <- data$fig_is_medical
    # ate2plot <- data$
    # data$df <- ateplot
  })
  
  output$ateplot <- renderPlot({
    if (!is.null(data$fig_Medical)) {
      return(data$fig_Medical)
    } else { fig_Medical }
  })
  
  output$ateplot2 <- renderPlot({
    if (!is.null(data$fig_Surgical)) {
      return(data$fig_Surgical)
    } else { fig_Surgical }
  })
  
  output$ate_table <- renderDataTable({
    # df_cond <- df_cond |> select(-platue,-delta_p)
    table <- DT::datatable(df_cond, 
                  filter = list(position = 'top', clear = FALSE),
                   rownames = FALSE, options = list(
                    pageLength = 20,
                    scrollX = TRUE, 
                    scrollY = "500px",
                    # dom = "t",
                    search = list(regex = TRUE),
                    order = list(list(7, 'desc'))
                    ))
    return(table)
  })
  
  output$description <- renderText({
    x <- "<p><h5>Side Panel:</h5></p>
    <p>Hour: Which hour to begin simulation. If 10 is selected, the first diamond will show up at 10th hour</p>
    <p>Mechanical Power: At which MP a provider would like to choose or the patient is on</p>
    <p>Mechanical Power Standard Deviation (SD): Plus minus (+/-) how many increase/decrease units of MP. This will assign how many rows data before and after to filter. 
    <p>Noise: [0 to 1 representing 0% to 100%]. If set at 0.8, 80% of MPs will have an increase (or decrease if min is negative integer) of min to max units of random variable from a uniform distribution; White diamond shaped points represent MP without noise, red diamond shaped points represented MP with noise</p>
    <p>Min Noise: the minimum value of the uniform distribution</p>
    <p>Max Noise: the maximum value of the uniform distribution</p>
    <br>
    <p><h5>Plots:</h5></p>
    <p>Orientation: x-axis= 1-96 hour; y-axis=Mechanical Power Threshold; Colorscale=Average Treatment Effect of Spo2/Fio2 Ratio; Left plot: Medical cohort; Right plot: Surgical cohort</p>
    <p>How was the heatmap created? Iteration was used to train every hour and every MP threshold to estimate its ATE using doWhy.backdoor.linear.regression. Linear regression was used as a proof-of-concept to train over 8640 models to estimate each timepoint ATE. All of these ATE were then used to create a 2d contour for easier representation of ATE ranges. MP threshold were generated using a logic, for example, during iteration model training of MP of 10, all MPs of 10 and above will be conditioned to 1, whereas MP of 9 and below will be conditioned as 0. Model is trained on that particular hour only for the cohort selected. Mean ATE an its confidence interval were obtained.</p> 
    <p> How can this be helpful? If there is truly an MP threshold effect, how do we use this heatmap to help to target the next MP to lower the risk of outcome (SFR). With an algorithmic approach, the next hour will attempt to lower MP by 3 units (set by MP sd) or according to the best ATE of the 3 units decrase. And same goes for the hour after that. The approach is to set MP target for the next hour if possible.</p>
    <br>
    <p><h5>Simulation:</h5></p>
    <p>Each diamond shaped point is the MP for that hour, the next MP is selected by filtering out number of rows (MP sd) of the current MP. Then the algorithm will sort descending according to ATE and top 3 will be selected. The minimum of the threshold of these 3 rows will be used for the next target</p>
    <br>"
  })
  
  output$edescription <- renderText({
    "<p>Search: Please select category (is_medical or is_surgical); Other parameters can be search through range (beginning number...end number), for example I would like to filter out MP of 10, I will type out 10...10 on the search box above mp to select only mp with 10. If you want to search MP of 15 to 20, then type in 15...20</p>
    <p>What is this for? MP is made of 4 other variables, tidal, rr, peep, p_insp. Which also means there are multiple combinations to make an MP of 30. This is to provide a list of possible parameters for ventilators with a set MP</p>
    <p>Why do I need to knwo the elements of MP? For example, if your next MP target is 25, how do you know which parameters to set to achieve target of 25 optimized by ATE? Also, if you have a fixed tidal volume, say 350ml, what other parameters can you adjust to achieve your MP target guided by the best ATE? With this table, type in hr 2..2, tidal 300 to 350, type in 300...350, the list will give you p_insp, peep and rr for your adjustment</p>
    <p>MP Element Visualization <a href>https://datalore.jetbrains.com/view/report/C6Ket3vLcJfqForC3gGE1S</a></p>
    "
  })
}


shinyApp(ui, server)
