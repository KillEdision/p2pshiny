shinyUI(bootstrapPage(
  fluidPage(
    title = "The credit risk assessments of borrower in P2P lending based on machine learning",
    tags$head(includeCSS(file.path(
      'www', 'style.css'
    ))),
    useShinyjs(),
    fluidRow(id = "title-row",column(
      12,h1(
        "The credit risk assessments of borrower in P2P lending based on machine learning"
      )
    )),
    ###########################################################################################################################
    sidebarLayout(sidebarPanel(
      fluidRow(
        id = "app-content",wellPanel(
          div(fileInput(
            inputId = 'dataset', label = tags$div('data set *',style = 'color:blue'),accept =
              c(
                'text/csv','text/comma-separated-values,text/plain','.csv'
              )
          )),div(
            checkboxInput("file_param","Show FileReader parameters",FALSE)
          ),
          conditionalPanel(
            condition = "input.file_param==true",checkboxInput(
              inputId = 'header',label = 'header',value = TRUE
            ),radioButtons(
              inputId = 'separator',label = 'sep',choices = c(
                Comma = ',',Semicolon = ';',Tab = '\t'
              ),','
            )
          ),numericInput(
            inputId = 'trainset_size',label = tags$div('train set size',style = 'color:blue'),value = 0.7,min = 0,max = 1,step = 0.1
          )
        ),

        wellPanel(
          div(checkboxInput("varibledisplay","Varibles",FALSE)),

          conditionalPanel(condition = 'input.varibledisplay==true',selectInput(
            inputId = "loan_dim_variable",label = "varibles of loan_dim",choices = c(names(loan_dim)),selected = c(names(loan_dim)[1:5]),multiple = TRUE,selectize = TRUE
          ),
          selectInput(
            inputId = "user_dim_variable",label = "varibles of user_dim",choices = c(names(user_dim)),selected = c(names(user_dim)[1:5]),multiple = TRUE,selectize = TRUE
          ),
          selectInput(
            inputId = "fact_variable",label = "varibles of fact",choices = c(names(fact)),selected = c(names(fact)[1:3]),multiple = TRUE,selectize = TRUE
          ),
          selectInput(
            inputId = "melt_variable",label = "varibles of melted data",choices = c(names(melteddata)),selected = c(names(melteddata)[1:5]),multiple = TRUE,selectize = TRUE
          ))

        ),

        wellPanel(
          div(
            checkboxInput("randomForest_param","Show RandomForest parameters",FALSE)
          ),
          conditionalPanel(
            condition = "input.randomForest_param==true",
            div(selectInput(
              inputId = 'type',label = tags$div('type',style = 'color:blue'),choices = c('Classification','Regression')
            )),
            div(numericInput(
              inputId = "ntree", label = tags$div('ntree',style = 'color:blue'), NA
            )),
            div(numericInput(
              inputId = "mtry", label = tags$div('mtry',style = 'color:blue') , NA
            )),
            div(
              selectInput(
                inputId = 'replace',label = tags$div('replace',style = 'color:blue'),choices = c('YES','NO'),selected = 'YES'
              )
            ),div(numericInput(
              inputId = "sampsize", label = tags$div('sampsize',style = 'color:blue') , NA
            )),
            div(
              selectInput(
                inputId = 'importance',label = tags$div('importance',style = 'color:blue'),choices = c('YES','NO'),selected = 'YES'
              )
            ),
            div(
              selectInput(
                inputId = 'proximity',label = tags$div('proximity',style = 'color:blue') ,choices = c('YES','NO'),selected = 'YES'
              )
            ),
            div(
              selectInput(
                inputId = 'oob.prox',label = tags$div('oob.prox',style = 'color:blue'),choices = c('YES','NO'),selected = 'YES'
              )
            ),
            div(
              selectInput(
                inputId = 'keep.forest',label = tags$div('keep.forest',style = 'color:blue'),choices = c('YES','NO'),selected = 'YES'
              )
            )
          )
        ),
        wellPanel(div(
          align = "center",actionButton(
            inputId = "quit", label = div(align = "center",'Quit the app',style = 'color:blue')
          )
        ))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "data",wellPanel(dataTableOutput('loan_dim')),br(),br(),wellPanel(dataTableOutput('user_dim')),br(),br(),wellPanel(dataTableOutput('fact')), br(),br(),wellPanel(dataTableOutput('data'))
        ),
        tabPanel(
          'summary',
          wellPanel(plotOutput('melteddata')),
          verbatimTextOutput('describe'),
          absolutePanel(wellPanel(
            checkboxInput("mainplot_param","Show mainplot parameters",FALSE),
            conditionalPanel(
              condition =
                "input.mainplot_param==true",wellPanel(
                  class = "settings",
                  h3(class = "settings-title", "Main plot"),
                  uiOutput("dataset_select"),
                  uiOutput("x_var_select"),
                  uiOutput("y_var_select"),
                  sliderInput("font_size", "Font size", 0, 50, 15, 1)
                )
            )
          ),draggable = TRUE)
          ,

          absolutePanel(wellPanel(
            checkboxInput("marginplot_param","Show marginplot parameters",FALSE),
            conditionalPanel(
              condition =
                "input.marginplot_param==true",wellPanel(
                  class = "settings",
                  h3(class = "settings-title", "Marginal plots"),
                  checkboxInput("show_marginal", "Show marginal plots", TRUE),

                  div(
                    id = "marginal-settings",
                    selectInput("extratype", NULL, c("density", "histogram", "boxplot")),
                    selectInput("margins", "Which margins?", c("both", "x", "y")),
                    conditionalPanel(
                      condition = "input.margins != 'y'",
                      selectInput("xtrans", "X axis transformation", c("none", "log","reverse"))
                    ),
                    conditionalPanel(
                      condition = "input.margins != 'x'",
                      selectInput("ytrans", "Y axis transformation", c("none", "log","reverse"))
                    ),
                    sliderInput("size",
                                "Size ratio of main plot:marginal plots",
                                1, 5, 5, 0.5),
                    shinyjs::colourInput("col", "Marginal plot colour", "red", showColour = "background"),
                    conditionalPanel(
                      condition = "input.type != 'density'",
                      shinyjs::colourInput("fill", "Marginal plot fill colour", "orange", showColour = "background")
                    )
                  )
                )
            )
          ),draggable = TRUE),

          wellPanel(plotOutput('extraplot')),

          absolutePanel(
            wellPanel(
              selectInput(
                inputId = 'ggallytype',label = 'GGallytype',choices = c(
                  'box','density','denstrip','dot','point','ggpairs','ggscatmat'
                ),selected = 'box'
              ),
              uiOutput("gg_x_var_select"),
              uiOutput("gg_y_var_select"),
              uiOutput('gg_col_var_select')
            ),draggable = TRUE
          ),
          wellPanel(plotOutput('ggally_plot')),

          absolutePanel(
            selectInput(
              inputId = 'leafletitem',label = 'item',choices = c('address','origin'),selected = 'address'
            ),draggable = TRUE
          ) ,

          wellPanel(leafletOutput('leaflet'))
        ),
        tabPanel(
          'model',
          wellPanel(actionButton(inputId = 'calrand',label = 'Train and Test Model'),
                    verbatimTextOutput('randsummary'),br(),br(),
                    actionButton(inputId = 'getTree',label = 'getTree'),
                    verbatimTextOutput("getTree"),

              actionButton(inputId = 'calpr',label = 'Precission/Recall Curve'),
            plotOutput('gg_pr_Curve'),
            br(),br(),

            actionButton(inputId = 'calc',label = 'Cumulative Curve'),
            plotOutput('gg_Cumulative_curve'),
            br(),br(),
              actionButton(inputId = 'calroc',label = 'ROC Curve'),
            plotOutput('gg_roc_curve'),
            br(),br(),
              actionButton(inputId = 'callift',label = 'Lift Curve'),
            plotOutput('gg_lift_curve'),
            br(),br()

          )
        ),
        tabPanel(
          "About",
          wellPanel(
            strong(
              'The credit risk assessments of borrower in P2P lending based on machine learning'
            ),

            p(
              'This application is developed with',
              a("Shiny.", href = "http://www.rstudio.com/shiny/", target =
                  "_blank"),
              ''
            ),
            p(
              'The code for this application is available at this',
              a('GitHub.', href = 'https://github.com/guanlongtianzi/shinyOfRandomForest', target =
                  "_blank")
            ),
            br(),

            strong('List of Packages Used'), br(),

            aceEditor(
              "myEditor1", value = '#R Scripts \nrequire(shiny)\nrequire(randomForest)\nrequire(ggplot2)\nrequire(shinyAce)\nrequire(bit)\nrequire(ff)\nrequire(shinyjs)\nrequire(ffbase)\nrequire(GGally)\nrequire(ROCR)\nrequire(grid)\nrequire(tabplot)', mode =
                "r",height = '200px',fontSize = 15,theme = "ambiance"
            ),
            br(),
            br(),
            wellPanel(
              div(
                align = "center",radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),inline = TRUE)
              ),
              div(
                align = "center",downloadButton(outputId = 'download',label = 'Download RandomForest')
              ),
              div(
                align = "center",radioButtons('format2', 'Document format', c('PDF', 'HTML', 'Word'),inline = TRUE)
              ),
              div(
                align = "center",downloadButton(outputId = 'downloadmeasure',label = 'Download Performance measure')
              )
            )
          )
        )
      )
    ))
  )
))