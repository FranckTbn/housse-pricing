library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(htmltools)


library(fresh)

library(esquisse)

library(htmlwidgets)

library(waiter)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)

library(shinycssloaders) # withspinner
library(spsComps)
library(mailtoR)

library(DT)


# Source the external R file
source("fonctions.R")



library(fresh)

# Create the theme
layout_vars <- bs4dash_layout(main_bg = "#6DC5D1")

navbar_vars <- list(
  # navbar background
  bs4dash_status(light = "red", primary = "#00755c", secondary = "#F1EF99"), 
  # put toggler in white
  bs4dash_vars(navbar_light_color = "#00755c" ) 
)

inverted_colors <- bs4dash_color(
  white = "#F1F1F1"
)


sidebar_vars <- list(
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
  )
)


mytheme <- create_theme(
  layout_vars,
  navbar_vars,
  inverted_colors,
  sidebar_vars
)




# header----
header = dashboardHeader(title = "Advanced Dashboard")

# sidebar----
sidebar = dashboardSidebar(
  
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
  
  
  sidebarMenu(
    menuItem("Data", tabName = "data", icon = icon("th")),
    menuItem("Visualise", tabName = "charts", icon = icon("tachometer-alt") ),
    menuItem("About", tabName = "about", icon = icon("address-card") )
  )
)


# controlbar ----

controlbar = dashboardControlbar( )




# footer ----
footer = dashboardFooter(
  left = a(
    href = "https://www.linkedin.com/in/bi-nene-othniel-tra-263771200/",
    target = "_blank", span(h5("TRA BI NENE  2024")) %>% animateAppendNested("pulse"),
    
    actionButton("reload", "Reload")
  )
)



# body ----
body = dashboardBody(
  
# use_theme(mytheme), # <-- use the theme
  tags$style(
    ".akzf {
        font-size: 20px;
        font-weight: bold;
      }
    "
  ),
tags$head(
  # Include our custom CSS
  includeCSS("styles.css"),
  includeScript("gomap.js")
),
  
  tabItems(
    
    ## data ----
    tabItem(
      tabName = "data",
    
        
        box(
          title = "simple penguins",
          closable = FALSE,
          status = "warning",
          width = 12,
          solidHeader = FALSE,
          collapsible = TRUE,
          
          fluidRow(
            column(
              width = 3,
              numericInput(
                inputId =  "nrows",
                label = "Number of rows:",
                min = 0,
                step = 2,
                max = 100,
                value = 4
              )
            ),
            
            column(
              width = 6,
              selectInput(
                inputId = "var_select",
                label = "Choose Options",
                selected = c(names(train)[1:6], "SalePrice"),
                choices = names(train),
                multiple = TRUE
              )
            ),
            
            
            DT::dataTableOutput("table")
          )
          
         
        ), 
      
      fluidRow(
        box(
          title = "simple penguins",
          closable = FALSE,
          status = "warning",
          width = 6,
          solidHeader = FALSE,
          collapsible = TRUE,
          
          withSpinner(plotOutput("missing_heatmap"))
        ),
        
        box(
          title = "corelation betwen numeric variable",
          closable = FALSE,
          status = "warning",
          width = 6,
          solidHeader = FALSE,
          collapsible = TRUE,
          
          withSpinner(plotOutput("corelation_heatmap"))
        )
      ),
      
      
      fluidRow(
        box(
          title = "Types of variable",
          closable = FALSE,
          status = "warning",
          width = 6,
          solidHeader = FALSE,
          collapsible = TRUE,
          
          highchartOutput(outputId = "hcplot1" )
        ),
        
        box(
          title = "corelation betwen numeric variable",
          closable = FALSE,
          status = "warning",
          width = 6,
          solidHeader = FALSE,
          collapsible = TRUE,
          
          
          withSpinner(plotOutput("corelation_heatmap"))
        )
      )
      
      
      
      ),
    
    ## visualise ----
    
    tabItem(
      tabName = "charts",
      
      fluidRow(
        
        column(width = 9,
               
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
            
            
            withSpinner( plotOutput("plot_train_test") )
            
          )
        ),
        
        box(
          title = "DISTRIBUTION",
          closable = FALSE,
          status = "warning",
          width = 12,
          height = "500px",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow( actionButton("update", "parametres", class="btn btn-info") ),
          sidebar = boxSidebar(
            id = "mycardsidebar",
            width = 35,
            background = "#333a40",
            startOpen = TRUE,
            
            "Choisis les bons paramètres qui correspondent à la nature de la variable correspondante ",
            code('install.packages("shiny")'),
            radioButtons("main_viz", "Select Waveform:",
                      choices = list("Viz_1" = "viz_1", 
                                     "Viz_2" = "viz_2",
                                     "Viz_3" = "viz_3"))
            
            
          ),
          
          fluidRow( "Shiny is a package from Posit that makes it incredibly easy to build interactive web applications with R.
    For an introduction and live examples, visit the Shiny homepage (https://shiny.posit.co).") ,
          
          withSpinner(plotOutput("plota"))
            
        ),
      
      
      conditionalPanel(
        condition = "input.var_type != 'numeric'",
        box(
          id = "box2",
          title = "lien avec la variable cible",
          closable = FALSE,
          status = "primary",
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          
          
          fluidRow(
            column(6, withSpinner( plotOutput("plotb1") )),
            column(6, withSpinner( plotOutput("plotb2") ))
            ),
          
          fluidRow(
            column(6, withSpinner( plotOutput("plotb3") ), offset = 3) 
            
            )
          )
        ),
        
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
    
    
    
    column(width = 3,
           
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         
                         h2("ZIP explorer"),
                         
                         
                         box(
                           title = "Chart Box 2",
                           closable = FALSE,
                           solidHeader = FALSE,
                           width = 12,
                           
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
                           
                           uiOutput("variable_select")
                           
                         )
                         
                         )
           
           
           
           
           
           
           )),
    
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
        label = boxLabel(text = 1,
                         status = "danger"),
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
          carouselItem(
            caption = "Item 1",
            tags$img(src = "https://r4ds.hadley.nz/cover.jpg")
          ),
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
  
  skin ="blue-light",
  scrollToTop = TRUE,
  preloader = list(html = tagList(spin_flower(), h4("App loading...")), color = "#3c8dbc"),
  header = header,
  sidebar = sidebar,
  body = body,
  
  footer = footer
  
)

server <- function(input, output, session) {
  
  output$plot_train_test <- renderPlot({
    
    viz_train_test(data = vartt , var = !!sym(input$variable) )
  })
  
  
  
  # Create a reactive value to store the selected dataset
  selected_dataset <- reactive({
    if (input$dataset_switch) {
      clean_data
    } else {
      base_data
    }
  })
  
  
  
  # Reactive expression to get the variable choices based on the selected dataset and variable type
  variable_choices <- reactive({
    dataset <- selected_dataset()
    if (input$var_type == "character") {
      names(dataset)[sapply(dataset, is.character)]
    } else if (input$var_type == "numeric") {
      names(dataset)[sapply(dataset, is.numeric)]
    }
  })
  
  
  
  # Update the selectInput choices based on the selected dataset
  output$variable_select <- renderUI({
    selectInput(
      inputId = "variable",
      label = "Select Variable",
      choices = variable_choices(),
      selected = "SalePrice"
    )
  })
  
  
  
  
  
  
  output$table <- DT::renderDataTable({
    
    df <- base_data %>%
      slice_head(n = input$nrows) %>%
      select(input$var_select)
    
    action <- DT::dataTableAjax(session, df, outputId = "table")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })

  
  
  
  
  
  #missing
  miss_data <- reactive({
    selected_dataset() %>%
      questionr::freq.na() %>%
      as.data.frame() %>%
      rownames_to_column("variables") %>%
      filter( .data[[missing]] > .env$input$miss ) %>%
      arrange( desc(missing))
  })  
  
  output$missing <- renderTable({
    
    miss_data() %>%
      gt_trabi()
  })
  
  #>>
  
  #<< plot

     output$plotb1 <- renderPlot({
       
       req(input$variable)  # Ensure that input$variable is available
       dataset <- selected_dataset()
       variable <- dataset[[input$variable]]
       
       if (is.character(variable)) {
       
       var_cible_quali_1(data = selected_dataset() , var = !!sym(input$variable) ) }
       
       
       
       })
     
     output$plotb2 <- renderPlot({
       
       req(input$variable)  # Ensure that input$variable is available
       dataset <- selected_dataset()
       variable <- dataset[[input$variable]]
       
       if (is.character(variable)) {
         
         var_cible_quali_2(data = selected_dataset() , var = !!sym(input$variable) ) }
       
       
       
     })
     
     
     output$plotb3 <- renderPlot({
       
       req(input$variable)  # Ensure that input$variable is available
       dataset <- selected_dataset()
       variable <- dataset[[input$variable]]
       
       if (is.character(variable)) {
         
         var_cible_quali_3(data = selected_dataset() , var = !!sym(input$variable) ) }

     })
     
     
     

     
   
   
   
   

  
  
  #<< plota ----

  
  output$plota<- renderPlot({
    
    req(input$variable) # s'assurer que la variable est sélectionnée
    
    train <- selected_dataset()
    Sys.sleep(.3)
    
    var <- input$variable
    
    if ( is.character(train[[var]]) ) {
      
      
      if (input$main_viz == "viz_1") {
        
        var_quali_viz_1(data = train, var = !!sym(input$variable))
        
      } else if( input$main_viz == "viz_2" ) {
        
        var_quali_viz_2(data = train, var = !!sym(input$variable))
        
      } else if ( input$main_viz == "viz_3" ) {
        
        p1 <- var_quali_viz_3(data = train, class = !!sym(input$variable))
        
        p2 <- var_quali_viz_4(data = train, class = !!sym(input$variable))
        
        p1 + p2
      }
      
      
    } else if (is.numeric(train[[var]])) {
      
      
      if (input$main_viz == "viz_1") {
        
        var_quanti_viz_1(data = train,  !!sym(input$variable))
        
      } else if( input$main_viz == "viz_2" ) {
        
        var_quanti_viz_2(data = train, var = !!sym(input$variable))
        
      } else if ( input$main_viz == "viz_3" ) {
        
        var_quanti_viz_3(data = train, !!sym(input$variable))
        
      } 
      
    }
    

    
  }, res = 100)
  
  
  
  
  observeEvent(input$update, {
    updateBoxSidebar("mycardsidebar")
  })
  #>>
  
  
  
  
 
  
  observeEvent(input$reload, {
    session$reload()
  })
  
 
  
  
  #<< heatmap ----
  
  output$missing_heatmap <- renderPlot({
    
    missing_heatmap(base_data)
    
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
    
    data.frame(n = apply( is.na( selected_dataset() ), 2, sum)) %>%
      rownames_to_column("variables") %>%
      filter(n > 0) %>%
      distinct(variables)
    
  })  
  
  
  observeEvent(input$variable, {
    
    if ( any( input$variable == missing_names()) ) {
      showModal(modalDialog(
        
        title = div(tags$b("Missing value", style = "color: red;")),
        p("this variable have missing value ."),br(),
        
        
        withSpinner( plotOutput("missing_donut") ),
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    
  })
  
  output$missing_donut <- renderPlot({
    
    missing_viz_1(data = selected_dataset(), class = !!sym(input$variable) )
    
  })
  
    


  output$plot1 <- renderPlot({
    hist(rnorm(100), main = "Histogram 1")
  })
  
  
  
  
  output$plot2 <- renderPlot({
    plot(rnorm(100), main = "Scatter Plot 1")
  })
  
  
  

  waitress <- Waitress$new("#plot3") # call the waitress
   
  
  output$plot3 <- renderPlot({
    
    for(i in 1:10){
      waitress$inc(10) # increase by 10%
      Sys.sleep(.01)
    }
    
    
    train %>% 
      group_by(.data[[input$xvar]]) %>%
      summarise( moyenne = mean(.data[[input$yvar]])) %>%
      ggplot( mapping = aes( x = .data[[input$xvar]], y = .data[[moyenne]],  ))+
      geom_col(fill = "dodgerblue1",
               colour = "black",) +
      labs(y = paste0(input$yvar,"Mean"))
    
    waitress$close() # hide when done
  })
  #>>
  

  #<< cible quati ----
  waitress_1 <- Waitress$new("#plot4") # call the waitress
  
  output$plot4 <- renderPlot({
    
    for(i in 1:10){
      waitress_1$inc(10) # increase by 10%
      Sys.sleep(.1)
    }
    
    req(input$variable)  # Ensure that input$variable is available
    dataset <- selected_dataset()
    variable <- dataset[[input$variable]]
    
    if (is.numeric(variable)) {
      
      
      var_cible_quanti(data = selected_dataset(), var = !!sym(input$variable) ) 
      
    }
    
    waitress_1$close() # hide when done
    
  })
  
  
  
  

  
  output$missing <- renderPlot({
    
    missing_heatmap(base_data)
  })
  
  
  
  observe({
    if (input$obs > 500) {
      addPopover(
        id = "distPlot",
        options = list(
          content = "Vivamus sagittis lacus vel augue laoreet rutrum faucibus.",
          title = "Server popover",
          placement = "bottom",
          trigger = "hover"
        )
      )
    } else {
      removePopover(id = "distPlot")
    }
  })
  
  
  #<< pre modale ----

  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      title = div(tags$b("Alert", style = "color: red;")),
      p("vous pouvez maintenant visualiser vos graphiques."),br(),
      
      textInput("name", "Veuillez entrer votre nom", width = NULL, placeholder = "your name"),
      
      
      if (failed)
        div(tags$b("entrez un nom invalide", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
      
    )
  }
  
  # Show modal.
  observe({
    showModal(dataModal())
  })
  
  
  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    if (is.null(input$name)) {
      showModal(dataModal(failed = TRUE))
      
    } else {
      removeModal()
    }
  })
  
  #>>
  
}

shinyApp(ui, server)
