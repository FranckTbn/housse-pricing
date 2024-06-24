library(shiny)
# library(semantic.dashboard)
library(bs4Dash)
library(shinyWidgets)
library(htmltools)
library(shinyFeedback)
library(shiny.fluent)
library(shinyjs)
library(shiny.semantic)
library(shinycssloaders) # withspinner


library(fresh)
library(esquisse)
library(htmlwidgets)
library(waiter)
library(bslib)


library(dplyr)
library(ggplot2)
library(ggExtra)

library(spsComps)
library(mailtoR)

library(DT)


# Source the external R file
source("fonctions.R")

# search_vars_bs4dash()
mytheme <- fresh::create_theme(
  
  bs4dash_font(
    size_base = NULL,
    size_lg = NULL,
    size_sm = NULL,
    size_xs = NULL,
    size_xl = NULL,
    weight_light = NULL,
    weight_normal = NULL,
    weight_bold = NULL,
    family_sans_serif = NULL,
    family_monospace = NULL,
    family_base = NULL
  ),
  
  bs4dash_layout(main_bg = "#6DC5D1"),
  bs4dash_status(
    light = "red",
    primary = "#00755c",
    secondary = "#F1EF99"
  ),
  # search_vars_bs4dash("navbar")
  bs4dash_vars(
    navbar_light_color = "#00755c",
    
    navbar_nav_link_padding_x	= "1rem",
    navbar_toggler_padding_y	= ".25rem",
    navbar_toggler_padding_x	= ".75rem",
    
    main_header_bottom_border_color = "#00755c",
    main_header_light_form_control_focused_color = "red",
    main_header_light_placeholder_color = "red",
    
    sidebar_padding_x = "0.1rem",
    
    main_header_brand_padding_y = "0rem",
    
    navbar_light_hover_color	= "#00755c",
    navbar_light_active_color	 = "#00755c",
    navbar_light_disabled_color	 = "#00755c",
    navbar_light_toggler_icon_bg	=  "#00755c",
    navbar_light_toggler_border_color  = "#00755c"
    
    )
  
  
  ,
  bs4dash_yiq(
    contrasted_threshold = 10,
    text_light = "#FFF",
  ),
  bs4dash_sidebar_light(
    bg = "#76ABAE",
    color = "#FFF",
    hover_color = "#FFF",
    submenu_color = "#FFF",
    submenu_hover_color = "#FFF"
  ))

# simple_card
simple_card <- function(title, width, content) {
  bs4Dash::box(
    style = "max-height: 500px; overflow: auto; padding: 0px; ",
    title = title,
    closable = FALSE,
    status = "olive",
    width = width,
    icon = icon("redo"),
    content
    
  )
}


# all  toasts options

# use this list for all your toasts
myToastOptions <- list(
  positionClass = "toast-top-right",
  progressBar = FALSE,
  timeOut = 3000,
  closeButton = TRUE,
  
  # same as defaults
  newestOnTop = TRUE,
  preventDuplicates = FALSE,
  showDuration = 300,
  hideDuration = 1000,
  extendedTimeOut = 1000,
  showEasing = "linear",
  hideEasing = "linear",
  showMethod = "fadeIn",
  hideMethod = "fadeOut"
)





# header----
header = dashboardHeader(
  color = "lightblue",
  title = "Advanced Dashboard",
  inverted = TRUE,
  dropdownMenu(type = "notifications", taskItem("Project progress...", 50.777, color = "olive"))
)

# sidebar----
sidebar = bs4Dash::dashboardSidebar(
  size = "thin",
  color = "lightblue",
  
  disable = FALSE,
  width = NULL,
  skin = NULL,
  status = "primary",
  elevation = 4,
  collapsed = FALSE,
  minified = TRUE,
  expandOnHover = TRUE,
  fixed = TRUE,
  id = NULL,
  customArea = NULL,
  
  
  bs4Dash::sidebarMenu(
    bs4Dash::menuItem("Data", tabName = "data", icon = icon("th")),
    bs4Dash::menuItem(
      "Visualise",
      tabName = "charts",
      icon = icon("tachometer-alt")
    ),
    bs4Dash::menuItem("About", tabName = "about", icon = icon("address-card"))
  )
)


# controlbar ----

controlbar = dashboardControlbar()




# footer ----
footer = dashboardFooter(
  left = a(
    href = "https://www.linkedin.com/in/bi-nene-othniel-tra-263771200/",
    target = "_blank",
    span(h5("TRA BI NENE  2024")) %>% animateAppendNested("pulse"),
    
    actionButton("reload", "Reload")
  )
)



# body ----
body = dashboardBody(
  useShinyjs(),
  shinyFeedback::useShinyFeedback(feedback = FALSE),
  use_theme(mytheme), # <-- use the theme
  tags$style(".akzf {
        font-size: 20px;
        font-weight: bold;
      }
    "),
  tags$head(# Include our custom CSS
    includeCSS("styles.css"), includeScript("gomap.js")),
  
  tabItems(
    ## data ----
    tabItem(
      tabName = "data",
      
      
      simple_card("Data", 12, uiOutput("analysis")),
      simple_card("Data Description", 12, uiOutput("skim")),
      simple_card(
        "Missing heatmap",
        12,
        shinycssloaders::withSpinner(plotOutput("missing"))
      ),
      simple_card(
        "Missing heatmap",
        12,
        shinycssloaders::withSpinner(plotOutput("missing_loliplot"))
      ),
      simple_card("Types of variable", 6, highchartOutput(outputId = "hcplot1")),
      simple_card("Numeric variable correlation", 6, withSpinner(plotOutput(
        "corelation_heatmap"
      )))
      
      
    ),
    
    ## visualise ----
    
    tabItem(
      tabName = "charts",
      semantic.dashboard::box(
        h1("main"),
        title = "A b c",
        width = 12,
        color = "warning"
      ),
      
      fluidRow(
        column(
          width = 9,
          
          conditionalPanel(
            condition = "input.var_type != 'numeric'",
            
            box(
              title = "train-test composition",
              closable = FALSE,
              status = "info",
              width = 12,
              height = "500px",
              solidHeader = FALSE,
              collapsible = TRUE,
              
              
              withSpinner(plotOutput("plot_train_test"))
              
            )
          ),
          
          uiOutput("num_distribution"),
          uiOutput("cat_distribution"),
          
          uiOutput("num_link"),
          uiOutput("cat_link"),
          
          
          conditionalPanel(
            condition = "input.var_type != 'character'",
            
            box(
              title = "Chart Box 2",
              closable = FALSE,
              status = "danger",
              solidHeader = TRUE,
              width = 12,
              "Box content",
              useWaitress(),
              withSpinner(plotOutput("plot4"))
            )
            
          )
          
        ),
        
        
        
        column(
          width = 3,
          
          
          absolutePanel(
            id = "controls",
            class = "panel panel-default",
            fixed = TRUE,
            draggable = TRUE,
            top = 0,
            left = "auto",
            right = 0,
            bottom = "auto",
            width = 350,
            height = "auto",
            
            
            box(
              title = "Chart Box 2",
              closable = FALSE,
              solidHeader = FALSE,
              width = 12,
              
              
              fluidRow(
                reactOutput("teachingBubble"),
                TooltipHost(
                  content = "choisis la variable explicative (SalePrice)",
                  delay = 1,
                  textOutput("file_path")
                )
              ),
              
              br(),
              
              switchInput(
                inputId = "dataset_switch",
                label = "Switch Dataset",
                
                onLabel = "clean data",
                offLabel = "data",
                value = TRUE
              ),
              
              radioGroupButtons(
                inputId = "var_type",
                label = "Select Variable Type",
                choices = list("Character" = "character", "Numeric" = "numeric"),
                selected = "numeric",
                individual = FALSE
              ),
              
              uiOutput("variable_select_Y"),
              uiOutput("variable_select_X")
              
            )
            
          )
        )
      ),
      
    ),
    
    ## about ----
    tabItem(
      tabName = "about",
      
      box(
        title = "",
        status = "primary",
        width = 12,
        userPost(
          id = "1",
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
          author = "Jonathan Burke Jr.",
          description = "Shared publicly - 7:30 PM today",
          "Lorem ipsum represents a long-held tradition for designers,
       typographers and the like. Some people hate it and argue for
       its demise, but others ignore the hate as they create awesome
       tools to help create filler text for everyone from bacon
       lovers to Charlie Sheen fans.",
          
          userPostTagItems(
            userPostTagItem(dashboardBadge("item 1", color = "info")),
            userPostTagItem(dashboardBadge("item 2", color = "danger"), side = "right")
          )
        ),
        userPost(
          id = "2",
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user6-128x128.jpg",
          author = "Adam Jones",
          userPostMedia(image = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"),
          userPostTagItems(
            userPostTagItem(dashboardBadge("item 1", color = "success")),
            userPostTagItem(dashboardBadge("item 2", color = "danger"), side = "right")
          )
        )
      ),
      box(
        title = "Décourverte",
        id = "box1",
        closable = TRUE,
        width = 12,
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        label = boxLabel(text = 1, status = "danger"),
        dropdownMenu = boxDropdown(
          boxDropdownItem(actionLink("goButton", "reduire/agrandir")),
          dropdownDivider(),
          boxDropdownItem(
            "Link to google",
            href = "https://www.google.com",
            icon = icon("table-cells")
          )
        ),
        sidebar = boxSidebar(
          startOpen = FALSE,
          width = 35,
          id = "mycardsidebar_z",
          sliderInput(
            "obs",
            "Number of observations:",
            min = 0,
            max = 1000,
            value = 500
          )
        ),
        
        span(
          p(
            "Cette application vous permet de faire une analyse statisque de vos conversations whatsap"
          ) ,
          style = "color:blue"
        ),
        p("exporter vos données depuis whatsap par un format txt"),
        p(
          "puis depuis l'application, importer le fichier pour commencer votre analyse"
        ),
        p("les packages utilisés pour la création des cette aplication sont : "),
        a(href = "https://rinterface.github.io/bs4Dash/articles/bs4Dash.html", code("bs4Dash")),
        br(),
        a(href = "https://rstudio.github.io/cheatsheets/html/shiny.html", code("shiny")),
        br(),
        a(href = "https://mastering-shiny.org/", code("shiny")),
        br(),
        a(href = "https://rinterface.github.io/shinydashboardPlus/articles/more-skins.html", code("shinydashboardPlus")),
        br(),
        a(href = "https://rstudio.github.io/shinydashboard/structure.html#boxes", code("shinydashboard")),
        
        p("Certain site m'ont été utils :"),
        
        a(href = "https://educationshinyappteam.github.io/Style_Guide/", em("shinyappteam")),
        
        hr(),
        carousel(
          id = "mycarousel",
          carouselItem(caption = "Item 1", tags$img(src = "https://r4ds.hadley.nz/cover.jpg")),
          carouselItem(
            caption = "Item 2",
            tags$img(src = "https://d33wubrfki0l68.cloudfront.net/0c97eee3d8fc820f3a8d670c08b286e8a524257b/e426c/cover.png")
          ),
          
          carouselItem(
            caption = "Item 3",
            tags$img(src = "https://r.geocompx.org/images/cover.png")
          )
          
          
        )
      )
      
    )
  )
)


# ui ----
ui = dashboardPage(
  title = "DataExplor",
  #autoWaiter(),
  
  freshTheme = NULL,
  options = NULL,
  
  skin = "blue-light",
  scrollToTop = TRUE,
  preloader = list(html = tagList(spin_flower(), h4("App loading...")), color = "#3c8dbc"),
  header = header,
  sidebar = sidebar,
  body = body,
  
  footer = footer
  
)

server <- function(input, output, session) {
  # data display as list
  details_list_columns <- reactive({
    tibble(
      fieldName = names(data()),
      name = names(data()),
      key = fieldName
    )
  })
  
  
  ## analysis
  
  output$analysis <- renderUI({
    items_list <- if (nrow(data()) > 0) {
      shiny.fluent::DetailsList(items = data(), columns = details_list_columns())
    } else {
      p("No matching transactions.")
    }
  })
  
  
  
  
  
  
  output$plot_train_test <- renderPlot({
    viz_train_test(data = vartt , var = !!sym(input$variable))
  })
  
  
  
  
  
  
  
  
  
  # skim data description
  output$skim <- renderUI({
    a <- skimr::skim(data())
    
    skim_data <- data.frame(
      type = a$skim_type,
      variable = a$skim_variable,
      missing = a$n_missing,
      `complete rate` = round(a$complete_rate, 2),
      `character min` = a$character.min,
      `character max` = a$character.max,
      `character empty` = a$character.empty,
      `a$character n_unique` = a$character.n_unique,
      `character whitespace` = a$character.whitespace,
      `numeric mean` = a$numeric.mean,
      `numeric sd` = a$numeric.sd,
      `numeric p0` = a$numeric.p0,
      `numeric p25` = a$numeric.p25,
      `numeric p50` = a$numeric.p50,
      `numeric p75` = a$numeric.p75,
      `numeric p100` = a$numeric.p100,
      `numeric hist` = a$numeric.hist
    )
    
    skim_list_columns <- tibble(
      fieldName = names(skim_data),
      name = names(skim_data),
      key = fieldName
    )
    
    
    shiny.fluent::DetailsList(items = skim_data, columns = skim_list_columns)
    
    
    
  })
  
  
  
  # heatmap ----
  
  output$missing <- renderPlot({
    base_data = data()
    missing_heatmap(base_data)
    
  })
  
  output$missing_loliplot <- renderPlot({
    base_data = data()
    missing_loliplot(base_data)
    
  })
  
  
  
  
  
  
  
  # # teaching Bubble
  # showBubble <- reactiveVal(FALSE)
  # observeEvent(input$uploadFileButton, showBubble(TRUE))
  # observeEvent(input$hideBubble, showBubble(FALSE))
  # output$teachingBubble <- renderReact({
  #   if (showBubble()) {
  #     TeachingBubble(
  #       target = "#target",
  #       headline = "Very useful!",
  #       ActionButton.shinyInput(
  #         "hideBubble",
  #         styles = list("background: green"),
  #         iconProps = list("iconName" = "AddFriend"),
  #         text = "Action Button"
  #       )
  #     )
  #   }
  # })
  
  
  # upload file ----
  observeEvent(input$uploadFileButton, {
    # click("uploadFile")
    runjs('document.getElementById("uploadFile").click();') # the same
  })
  
  output$file_path <- renderText({
    if (is.null(input$uploadFile)) {
      "Aucun fichier"
    } else {
      paste("Fichier : ", input$uploadFile$name)
    }
  })
  
  
  
  # data loading ----
  data <- reactive({
    req(input$uploadFile)
    
    file <- input$uploadFile
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath, header = TRUE, sep = ",")
  })
  
  
  
  #Create a reactive valcharacter.n_unique# 
  #Create a reactive value to store the selected dataset
  selected_dataset <- reactive({
    if (input$dataset_switch) {
      data()
    } else {
      data()
    }
  })
  
  # choisis la variiable ----
  variable_choices <- reactive({
    dataset <- data()
    if (input$var_type == "character") {
      names(dataset)[sapply(dataset, is.character)]
    } else if (input$var_type == "numeric") {
      names(dataset)[sapply(dataset, is.numeric)]
    }
  })
  
  
  
  
  
  
  # select X ----
  output$variable_select_X <- renderUI({
    shiny::selectInput(
      inputId = "variable",
      label = "Select Variable",
      choices = variable_choices(),
      selected = NULL
    )
  })
  
  
  # select Y ----
  output$variable_select_Y <- renderUI({
    dataset <- data()
    shiny::selectInput(
      inputId = "variable_Y",
      label = "variable explicative",
      choices = names(dataset)[sapply(dataset, is.numeric)],
      selected = NULL
    )
  })
  
  
  
  
  
  
  
  # #missing
  # miss_data <- reactive({
  #   selected_dataset() %>%
  #     questionr::freq.na() %>%
  #     as.data.frame() %>%
  #     rownames_to_column("variables") %>%
  #     filter( .data[[missing]] > .env$input$miss ) %>%
  #     arrange( desc(missing))
  # })
  #
  # output$missing <- renderTable({
  #
  #   miss_data() %>%
  #     gt_trabi()
  # })
  #
  #>>
  
  #<< plot
  
  # plot 1 2 3
  output$plot1 <- renderPlot({
    req(input$variable)  # Ensure that input$variable is available
    dataset <- selected_dataset()
    variable <- dataset[[input$variable]]
    
    if (is.character(variable)) {
      var_cible_quali_1(
        data = selected_dataset() ,
        var = !!sym(input$variable),
        label =  !!sym(input$variable_Y)
      )
    }
    
    
    
  })
  
  output$plot2 <- renderPlot({
    req(input$variable)  # Ensure that input$variable is available
    dataset <- selected_dataset()
    variable <- dataset[[input$variable]]
    
    if (is.character(variable)) {
      var_cible_quali_2(data = selected_dataset() , var = !!sym(input$variable))
    }
    
    
    
  })
  
  
  output$plot3 <- renderPlot({
    req(input$variable)  # Ensure that input$variable is available
    dataset <- selected_dataset()
    variable <- dataset[[input$variable]]
    
    if (is.character(variable)) {
      var_cible_quali_3(data = selected_dataset() , var = !!sym(input$variable))
    }
    
  })
  
  
  # distribution ui ----
  
  observeEvent(input$variable, {
    train <- data()
    var <- input$variable
    
    if (is.character(train[[var]]) | is.factor(train[[var]])) {
      output$cat_distribution <- renderUI({
        bs4Dash::tabBox(
          id = "tabs",
          title = "DISTRIBUTION",
          selected = NULL,
          width = 12,
          maximizable = TRUE,
          status = "warning",
          solidHeader = TRUE,
          height = "500px",
          background = "gray",
          gradient = TRUE,
          type = "tabs",
          tabPanel("Graph a", withSpinner(plotOutput("plot_a"))),
          tabPanel("Graph b", withSpinner(plotOutput("plot_b"))),
          tabPanel("Graph c", withSpinner(plotOutput("plot_c")))
        )
      })
    }else{
      output$cat_distribution <- renderUI({NULL})
    }
    
    
  })
  
  
  observeEvent(input$variable, {
    train <- data()
    var <- input$variable
    
    if (is.numeric(train[[var]]) | is.integer(train[[var]])) {
      output$num_distribution <- renderUI({
        bs4Dash::tabBox(
          id = "tabs",
          title = "DISTRIBUTION",
          selected = NULL,
          width = 12,
          maximizable = TRUE,
          status = "warning",
          solidHeader = TRUE,
          height = "500px",
          background = "gray",
          gradient = TRUE,
          type = "tabs",
          tabPanel("Graph 1", withSpinner(plotOutput("plot_1"))),
          tabPanel("Graph 2", withSpinner(plotOutput("plot_2"))),
          tabPanel("Graph 3", withSpinner(plotOutput("plot_3")))
        )
      })
    }else{
      output$num_distribution <- renderUI({NULL})
    }
    
    
  })
  
  
  # plot a b c ----
  output$plot_a <- renderPlot({
    req(input$variable) # s'assurer que la variable est sélectionnée
    train <- train
    var <- input$variable
    
    if (is.character(train[[var]]) | is.factor(train[[var]])) {
      var_quali_viz_1(data = train, var = !!sym(input$variable))
    }
  })
  
  
  # plot b
  output$plot_b <- renderPlot({
    req(input$variable)
    train <- train
    var <- input$variable
    
    if (is.character(train[[var]]) | is.factor(train[[var]])) {
      var_quali_viz_2(data = train, var = !!sym(input$variable))
    }
  })
  
  
  # plot c
  output$plot_c <- renderPlot({
    req(input$variable)
    train <- train
    var <- input$variable
    
    if (is.character(train[[var]]) | is.factor(train[[var]])) {
      p1 <- var_quali_viz_3(data = train, class = !!sym(input$variable))
      p2 <- var_quali_viz_4(data = train, class = !!sym(input$variable))
      p1 + p2
    }
    
    
  })
  
  
  
  # plot 1 2 3 ----
  output$plot_1 <- renderPlot({
    req(input$variable)
    train <- train
    var <- input$variable
    
    if (is.numeric(train[[var]]) | is.integer(train[[var]])) {
      var_quanti_viz_1(data = train, !!sym(input$variable))
    }
    
  })
  
  # plot 2
  output$plot_2 <- renderPlot({
    req(input$variable)
    train <- train
    var <- input$variable
    
    if (is.numeric(train[[var]]) | is.integer(train[[var]])) {
      var_quanti_viz_2(data = train, !!sym(input$variable))
    }
    
  })
  
  # plot 3
  output$plot_3 <- renderPlot({
    req(input$variable)
    train <- train
    var <- input$variable
    
    if (is.numeric(train[[var]]) |
        is.integer(train[[var]])) {
      var_quanti_viz_3(data = train, !!sym(input$variable))
    }
    
  })
  
  
  observeEvent(input$reload, {
    session$reload()
  })
  
  # link ui ----
  
  observeEvent(input$variable, {
    train <- data()
    var <- input$variable
    
    if (is.character(train[[var]]) | is.factor(train[[var]])) {
      output$cat_link <- renderUI({
        bs4Dash::tabBox(
          id = "tabs",
          title = "DISTRIBUTION",
          selected = NULL,
          width = 12,
          maximizable = TRUE,
          status = "warning",
          solidHeader = TRUE,
          height = "500px",
          background = "gray",
          gradient = TRUE,
          type = "tabs",
          tabPanel("Graph a", withSpinner(plotOutput("plot1"))),
          tabPanel("Graph b", withSpinner(plotOutput("plot2"))),
          tabPanel("Graph a", withSpinner(plotOutput("plot3")))
        )
      })
    }else{
      output$cat_link <- renderUI({NULL })
    }
    
    
  })
  
  
  
  
  output$corelation_heatmap <- renderPlot({
    corelation_heatmap(base_data)
  })
  
  
  
  output$hcplot1 <- renderHighchart({
    var_type_viz_1(base_data)
  })
  
  
  #<< missing ----
  
  # that contains the names of variables who have missing value
  missing_names <- reactive({
    data.frame(n = apply(is.na(selected_dataset()), 2, sum)) %>%
      rownames_to_column("variables") %>%
      filter(n > 0) %>%
      distinct(variables)
    
  })
  
  
  observeEvent(input$variable, {
    if (any(input$variable == missing_names())) {
      showModal(modalDialog(
        title = div(tags$b("Missing value", style = "color: red;")),
        p("this variable have missing value ."),
        br(),
        
        
        withSpinner(plotOutput("missing_donut")),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    
  })
  
  output$missing_donut <- renderPlot({
    missing_viz_1(data = selected_dataset(), class = !!sym(input$variable))
    
  })
  
  
  waitress <- Waitress$new("#plot3") # call the waitress
  
  
  output$plotazr3 <- renderPlot({
    for (i in 1:10) {
      waitress$inc(10) # increase by 10%
      Sys.sleep(.01)
    }
    
    
    train %>%
      group_by(.data[[input$xvar]]) %>%
      summarise(moyenne = mean(.data[[input$yvar]])) %>%
      ggplot(mapping = aes(x = .data[[input$xvar]], y = .data[[moyenne]], )) +
      geom_col(fill = "dodgerblue1", colour = "black", ) +
      labs(y = paste0(input$yvar, "Mean"))
    
    waitress$close() # hide when done
  })
  #>>
  
  
  #<< cible quati ----
  waitress_1 <- Waitress$new("#plot4") # call the waitress
  
  output$plot4 <- renderPlot({
    for (i in 1:10) {
      waitress_1$inc(10) # increase by 10%
      Sys.sleep(.1)
    }
    
    req(input$variable)  # Ensure that input$variable is available
    dataset <- selected_dataset()
    variable <- dataset[[input$variable]]
    
    if (is.numeric(variable)) {
      var_cible_quanti(data = selected_dataset(), var = !!sym(input$variable))
      
    }
    
    waitress_1$close() # hide when done
    
  })
  
  
  # modale ----

  dataModal <- function(failed = FALSE) {
    shiny::modalDialog(
      title = div(tags$b("Alert", style = "color: red;")),
      easyClose = FALSE,
      size = "xl",
      p("vous pouvez maintenant visualiser vos graphiques."),
      br(),
      
      span("Cette application est née de la nécessité de réduire la redondance de
      code lors de l'analyse des données dans les projets Kaggle. En participant
      à divers défis, j'ai constaté que les mêmes visualisations et analyses étaient
      souvent répétées, ce qui pouvait rendre le processus inefficace et chronophage.

Afin de remédier à ce problème, j'ai développé cette application pour regrouper 
toutes les visualisations couramment utilisées en un seul endroit. Elle offre une
compréhension rapide et globale des données d'entraînement, facilitant ainsi l'analyse
et l'exploration initiale des datasets.

L'application présente une variété de graphiques et de visualisations prédéfinis
qui vous aideront à mieux comprendre vos données sans avoir à écrire du code répétitif.
Cependant, je crois fermement en la collaboration et l'innovation communautaire.
Si vous avez des idées de graphiques supplémentaires ou des méthodes particulières 
pour visualiser vos données, je vous encourage vivement à les partager, accompagnées 
de votre code.

Ensemble, nous pouvons améliorer cette application pour qu'elle devienne un outil 
           encore plus puissant et adaptable à différents besoins analytiques. Profitez
           de cette plateforme pour explorer vos données de manière plus efficace et collaborative."),
      textInput(
        "name",
        "Veuillez entrer votre nom",
        width = NULL,
        placeholder = "your name"
      ),
      
      DefaultButton.shinyInput(
        inputId = "uploadFileButton",
        text = "Upload File",
        iconProps = list(iconName = "Upload")
      ),
      div(
        style = "
                               visibility: hidden;
                               height: 0;
                               width: 0;
                               ",
        shiny::fileInput(
          inputId = "uploadFile",
          accept = ".csv",
          label = NULL
        )
      ),
      
      footer = tagList(shiny::actionButton("ok", "OK"))
      
    )
  }
  
  
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observe({
      shiny::showModal(dataModal(failed = FALSE))
   })
  
  
  # reset the loadingButton to its active state after 2 seconds
  observeEvent(input$myFirstButton, {
    Sys.sleep(2)
    resetLoadingButton("myFirstButton")
    
  })
  
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    if (is.null(input$uploadFile)) {
      showModal(dataModal(failed = TRUE))
      showToast(
        "error", 
        "importez une base de donnée afin utiliser pleinement l'aplication" ,
        .options = myToastOptions
      )
    }
  })
  
  
  observeEvent(input$uploadFile, {
    if (!is.null(input$uploadFile)) {
     
      showToast(
        "success", 
        paste0("importation de", textOutput("file_path", inline = TRUE), " reussi"), 
        .options = myToastOptions
      )
      
      shiny::removeModal()
    }
    
  })
  #>>
  
}

shinyApp(ui, server)
