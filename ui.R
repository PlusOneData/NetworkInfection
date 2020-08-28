# Define UI for application that modifies network graphs
shinyUI(
    fluidPage(
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "legend.css")
        ),
        
        # Application title
        titlePanel("COVID-19 Network Model"),
        
        # Sidebar with a slider input for various model parameters
        sidebarLayout(
            sidebarPanel(
                shinyjs::useShinyjs(),
                id = "side-panel",
                tags$style(type = 'text/css', ".btn {width:100%;}"),
                dropdownButton(
                    inputId = "model",
                    label = "Model Controls",
                    #icon = icon("sliders"),
                    status = "primary",
                    size = "default",
                    circle = FALSE,
                    width = "100%",
                    sliderInput("day",
                                "Days Since Infection:",
                                min = 1,
                                max = 60,
                                value = 30),
                    sliderInput("n",
                                "Number of People:",
                                min = 10,
                                max = 500,
                                value = 50),
                    sliderInput("init_num",
                                "Initial Number Infected:",
                                min = 1,
                                max = 100,
                                value = 1)
                    # prettyToggle(
                    #     inputId = "na",
                    #     label_on = "NAs keeped",
                    #     label_off = "NAs removed",
                    #     icon_on = icon("check"),
                    #     icon_off = icon("remove")
                    # )
                ),
                br(),
                dropdownButton(
                    inputId = "testing",
                    label = "Testing Controls",
                    #icon = icon("sliders"),
                    status = "primary",
                    size = "default",
                    circle = FALSE,
                    width = "100%",
                    sliderInput("testDelay",
                                "Number of Days for Results:",
                                min = 1,
                                max = 14,
                                value = 4),
                    sliderInput("testFrequency",
                                "Number of Days Between Testing:",
                                min = 1,
                                max = 10,
                                value = 3),
                    sliderInput("propTested",
                                "Percent of Population Tested:",
                                min = 0.0,
                                max = 1.0,
                                value = 0.8)
                ),
                bsPopover(id = "testing", title = "Testing",
                          content = "Controls to adjust testing parameters.",
                          placement = "right",  
                          options = NULL),
                br(),
                dropdownButton(
                    inputId = "comply",
                    label = "PPE Compliance",
                    #icon = icon("sliders"),
                    status = "primary",
                    circle = FALSE,
                    width = "100%",
                    tooltip = TRUE,
                    # tooltipOptions(placement = "right", title = "Something"),
                    sliderInput("compliance",
                                "Percent Complying with Policies:",
                                min = 0.0,
                                max = 1.0,
                                value = 0.8),
                    checkboxGroupInput("npi", "Select Policy Measures:",
                                       choiceNames =
                                           list(
                                               #paste0("This is a test", shiny::icon("head-side-mask")),
                                               HTML('Face Coverings Required <i class="fas fa-head-side-mask" style = "color:#D55E00;"></i>'),
                                               HTML('Eye Protection Required <i class="fas fa-head-side-mask" style = "color:#D55E00;"></i>'),
                                               #tags$div(HTML('<i class="fa fa-font" style = "color:#D55E00;"></i> This is a text input')),
                                               #HTML('<i class="fas fa-head-side-mask"></i>'),
                                               HTML('Social Distancing Required <i class="fas fa-head-side-mask"></i>')),
                                       choiceValues =
                                           list("faceCovering", "eyeProtection", "distancing"),
                                       selected = c("faceCovering", "eyeProtection", "distancing")
                    )
                ),
                br(),
                dropdownButton(
                    inputId = "recovery",
                    label = "Recovery Controls",
                    status = "primary",
                    circle = FALSE,
                    width = "100%",
                    tooltip = TRUE,
                    sliderInput("max_recovery_time",
                                "Maximum Time to Recover:",
                                min = 10,
                                max = 30,
                                value = 20),
                    sliderInput("leaveDuration",
                                "Maximum Time on Leave:",
                                min = 5,
                                max = 30,
                                value = 10)
                ),
                br(),
                dropdownButton(
                    inputId = "costs",
                    label = "Cost Drivers",
                    status = "primary",
                    circle = FALSE,
                    width = "100%",
                    tooltip = TRUE,
                    sliderInput("avgWage", 
                                "Estimated Average Hourly Wage:",
                                min = 7.75, max = 200, value = 25, 
                                # step = 2500,
                                # animate = TRUE,
                                pre = "$", sep = ","),
                    radioButtons("testTypes", "Select Tests Used:",
                                 choiceNames =
                                     list("PCR", "Antigen", "LAMP"
                                     ),
                                 choiceValues =
                                     list("pcr", "antigen", "lamp"),
                                 selected = c("pcr")
                    ),
                    checkboxGroupInput("ppeBought", "Select PPE Purchased:",
                                 choiceNames =
                                     list("Face Masks", "Face Shields", "Hand Sanitizer", "Gloves"
                                     ),
                                 choiceValues =
                                     list("masks", "shields", "handSan", "gloves"),
                                 selected = c("masks", "handSan")
                    )
                ),
                # tags$div(style = "height: 140px;"), # spacing
                br(),
                actionButton("update", "Update Bottom SIR Plot", width = "100%"),
                br(),
                br(),
                actionButton("reset_input", "Reset Model Inputs", width = "100%"),
                br(),
                #br(),
                
                # Should fix at some point to combine with below conditionalPanel
                conditionalPanel(
                    #condition = "input.tabs != 'SIR Distribution' && input.tabs != 'Summary'",
                    condition = "input.tabs != 'Summary'",
                    tags$div(
                        HTML("<h3 style='color:#204d74; text-align:center; vertical-align: middle; font-size: 20px; text-decoration: underline; margin-bottom: 1.5em;'>Summary Statistics</h3>")
                    ),
                    verbatimTextOutput("summary") %>% withSpinner(color="#0dc5c1", proxy.height = "50px", hide.ui = FALSE, size = 0.5),
                    tags$head(tags$style("#summary{background-color: white; font-size: 14px}")),
                    
                    
                    # Check if conditionalPanels can be nested?
                    conditionalPanel(
                        condition = "input.tabs == 'SIR Distribution'",
                        tags$div(
                            HTML("<h3 style='color:#204d74; text-align:center; vertical-align: middle; font-size: 20px; text-decoration: underline; margin-bottom: 1.5em;'>Cumulative Leave Costs</h3>")
                        )
                    ),
                    
                    tableOutput("table")
                )
            ),
            
            
            
            # Show a plot of the generated distribution
            mainPanel(
                tags$style(HTML(".tooltip {opacity: 1}")),
                tabsetPanel(id = "tabs", type = "tabs",
                            tabPanel("SIR Distribution", 
                                     br(),
                                     fluidRow(plotOutput("curvePlot", height="300px") %>% withSpinner(color="#0dc5c1")),
                                     br(),
                                     fluidRow(plotOutput("curvePlot2", height="300px") %>% withSpinner(color="#0dc5c1"))),
                            tabPanel("Random Force Network",
                                     tags$div(class = "center",
                                              HTML('
                                          <br><strong><center>Random Force Network<center></strong><br>
                                          <ul class="legend">
                                             <li><span class="susceptible"></span> Susceptible</li>
                                             <li><span class="infected"></span> Infected</li>
                                             <li><span class="recovered"></span> Recovered</li>
                                             <li><span class="leave"></span> On Leave</li>
                                         </ul>
                                         <br>
                                         ')
                                     ),
                                     forceNetworkOutput("randomForce")  %>% withSpinner(color="#0dc5c1")),
                            tabPanel("Scale Free Force Network", 
                                     tags$div(class = "center",
                                              HTML('
                                          <br><strong><center>Scale Free Force Network<center></strong><br>
                                          <ul class="legend">
                                             <li><span class="susceptible"></span> Susceptible</li>
                                             <li><span class="infected"></span> Infected</li>
                                             <li><span class="recovered"></span> Recovered</li>
                                             <li><span class="leave"></span> On Leave</li>
                                         </ul>
                                         <br>
                                         ')
                                     ),
                                     forceNetworkOutput("scaleForce") %>% withSpinner(color="#0dc5c1")),
                            tabPanel("Small World Force Network", 
                                     tags$div(class = "center",
                                              HTML('
                                          <br><strong><center>Small World Force Network<center></strong><br>
                                          <ul class="legend">
                                             <li><span class="susceptible"></span> Susceptible</li>
                                             <li><span class="infected"></span> Infected</li>
                                             <li><span class="recovered"></span> Recovered</li>
                                             <li><span class="leave"></span> On Leave</li>
                                         </ul>
                                         <br>
                                         ')
                                     ),
                                     forceNetworkOutput("smallForce") %>% withSpinner(color="#0dc5c1")),
                            tabPanel("Summary",
                                     tags$iframe(src = './NetworkInfection.html', # put myMarkdown.html to /www
                                                 width = '100%', height = '800px', 
                                                 frameborder = 0, scrolling = 'auto'))
                            #htmlOutput("inc"))
                            #includeMarkdown("NetworkInfection.Rmd"))
                            #tabPanel("Scale Free", plotOutput("test1Plot")),
                            #tabPanel("Small World", plotOutput("test2Plot"))
                )
            )
        )
    )
)
