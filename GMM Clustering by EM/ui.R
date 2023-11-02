library(mvtnorm)
library(fields)
library(plotly)
library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)

# Define UI for application
ui = navbarPage("111024509 Statistical Computing Final Project", ## for the main menu
                # tab1
                tabPanel("GMM Clustering",fluidPage(theme = shinytheme("cosmo")),  ## set shiny theme
                         tags$head(
                           tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                         pageWithSidebar(
                           headerPanel('GMM Clustering by EM Algorithm'),
                           sidebarPanel(
                             # input file
                             fileInput("file", "Input data.", ## for input file
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")
                             ),
                             tags$small(paste0(
                               "Note : Check if the dataset is numeric."
                             )),
                             # select data
                             # uiOutput("ui_var"),
                             checkboxGroupInput("var", "Select variables.", choices = " "),
                             tags$small(paste0(
                               "Note : Please select at least 2 variables."
                             )),
                             # select k
                             textInput(inputId = "k_select", label = "Enter the number of components k.", value = " "),
                             actionButton("GMMrun","Submit"),
                             br(),
                             tags$small(paste0(
                               "Note : An inappropriate large k may cause an error, please try again or choose a smaller k."
                             )),
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Data Selected",
                                        downloadButton("download_data_tab", "Download Data"),
                                        dataTableOutput("data_tab")
                               ),
                               tabPanel("Histogram and Contour Plot by GMM",
                                        # downloadButton('download_plot', 'Download Plot'),
                                        plotOutput("plot", width = 800, height = 800)
                               ),
                               tabPanel("Process of Estimating Parameters",
                                        downloadButton("download_pro_par", "Download Data"),
                                        dataTableOutput("pro_par")
                               ),
                               tabPanel("Convergence of Parameters",
                                        # downloadButton('download_tsplot', 'Download Plot'),
                                        plotOutput("tsplot", width = 500, height = 500)
                               ),
                               tabPanel("Value of Estimated Parameters",
                                        downloadButton("download_est_par", "Download Data"),
                                        dataTableOutput("est_par")
                               ),
                               tabPanel("Clustering Index",
                                        downloadButton("download_cluster_par", "Download Data"),
                                        dataTableOutput("cluster_par")
                               )
                             )
                           )
                         )
                ),
                # tab2
                tabPanel("Best k Selecting",
                         tags$head(
                           tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                         pageWithSidebar(
                           headerPanel('Selecting k by BIC'),
                           sidebarPanel(
                             # input file
                             fileInput("fileBIC", "Input data.", ## for input file
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")
                             ),
                             tags$small(paste0(
                               "Note : Check if the dataset is numeric."
                             )),
                             # select data
                             checkboxGroupInput("varBIC", "Select variables.", choices = " "),
                             tags$small(paste0(
                               "Note : Please select at least 2 variables."
                             )),
                             # select k
                             textInput(inputId = "k_min_select", label = "Enter the starting number of components k.", value = " "),
                             textInput(inputId = "k_max_select", label = "Enter the ending number of components k.", value = " "),
                             actionButton("BICrun","Submit"),
                             br(),
                             tags$small(paste0(
                               "Note : An inappropriate large k may cause an error, please try again or choose a smaller k."
                             )),
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Data Selected",
                                        downloadButton("download_BICdata_tab", "Download Data"),
                                        dataTableOutput("BICdata_tab")
                               ),
                               tabPanel("BIC Comparing",
                                        # downloadButton('download_BICplot', 'Download Plot'),
                                        plotOutput("BICplot", width = 500, height = 500)
                               )
                             )
                           )
                         )
                ),
                
                tabPanel("About",## for the main menu
                         titlePanel("GMM Clustering by EM Algorithm Shiny App Descriptions"),

                         hr(),
                         strong("GMM Clustering",style = "font-size:25px"),
                         br(),
                         helpText("Data Selected : Print the original dataframe according to the user selection.",style = "font-size:20px"),
                         helpText("Histogram and Contour Plot by GMM : Plot the histograms with the estimated curves of selected variables and the 2D contour plots with clustering of selected variables.",style = "font-size:20px"),
                         helpText("Process of Estimating Parameters : Show all works from initial value to the final estimated value.",style = "font-size:20px"),
                         helpText("Convergence of Parameters : Show time series plot of parameters to check the parameters convegence.",style = "font-size:20px"),
                         helpText("Value of Estimated Parameters : Show the final estimated value.",style = "font-size:20px"),
                         helpText("Clustering Index : Show the clustering group of all data.",style = "font-size:20px"),
                         br(),
                         helpText("Note : Since different initials may result to different process, some highly closed initial values may cause a reduced of parameters, which mostly appear at large k, and turn out an error, if such consequence occurs, you may try again or choose a smaller k.",style = "font-size:20px"),
                         br(),
                         strong("Best k Selecting",style = "font-size:25px"),
                         br(),
                         helpText("Data Selected : Print the original dataframe according to the user selection.",style = "font-size:20px"),
                         helpText("BIC Comparing : Calculate BIC with respected k and compare in a line chart.",style = "font-size:20px"),
                         br(),
                         helpText("Note : BIC comparing requires several runs of EM, the same error may show up more frequently, please choose the k precisely.",style = "font-size:20px"),
                         br(),
                         strong("EM Algorithm",style = "font-size:25px"),
                         br(),
                         withMathJax(),
                         helpText("Set tolerence rate at \\(10^{-7}\\) and maximum iterations at 500 runs.",style = "font-size:20px"),
                         
                ),
                
                tabPanel("Developer and Related Websites", ## for the main menu
                         column(6,
                                titlePanel("Developer"),
                                hr(),
                                img(class="img-polaroid",src="KuanLinChen.jpg", width = 100, height = 138),
                                br(),
                                p(a("Kuan Lin Chen", href="https://www.facebook.com/klchen1999", target="_blank"),style = "font-size:25px"),
                                p("Institute of Statistics, National Tsing Hua University",style = "font-size:20px"),
                                p("Email : stat111024509@gapp.nthu.edu.tw",style = "font-size:20px")
                         )
                )
)