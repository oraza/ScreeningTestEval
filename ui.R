library(shiny)
shinyUI(fluidPage(
  titlePanel("Screening Test Evaluation Web App", windowTitle = "Screening Test Evaluation"),
  fluidRow(
    column(width = 2,
      tags$a(href = "http://www.facebook.com/sharer.php?u=https://ow-raza.shinyapps.io/ScreeningTestEval/", 
             target = "_blank", 
             tags$img(height = "20px", 
                      src = "fb2.png")
      ),
      tags$a(href = "http://www.linkedin.com/shareArticle?mini=true&url=https://ow-raza.shinyapps.io/ScreeningTestEval/", 
             target = "_blank", 
             tags$img(height = "20px", 
                      src = "linkedin.png")
      ),
      tags$a(href = "https://github.com/oraza",
             target = "_blank",
             tags$img(height = "20px", 
                      src = "github.png")
      ),
      tags$a(href = "mailto:?ow.raza@hotmail.com?subject=Feedback on Screening Test Evaluation Web App",
             target = "_blank",
             tags$img(height = "20px", 
                      src = "mail.png")
      )
    )
    ),
  br(),
  sidebarLayout(
    sidebarPanel(width = 3,
                 strong("Insert your data below:", style = "font-family: 'times'; color:blue"),
                 numericInput("TP",
                              label = h5("True Positive (a)", style = "font-family: 'times'; color:blue"),
                              value = 00),
                 numericInput("FP",
                              label = h5("False Positive (b)", style = "font-family: 'times'; color:blue"),
                              value = 00),
                 numericInput("FN", 
                              label = h5("False Negative (c)", style = "font-family: 'times'; color:blue"),
                              value = 00),
                 numericInput("TN",
                              label = h5("True Negative (d)", style = "font-family: 'times'; color:blue"),
                              value = 00)
                 ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Important note",
                 br(),
                 br(),
                 br(),
                 h4("Introduction",  align = "left", style = "font-family: 'times'; color:blue"),
                 p("This web-based app helps you to evaluate screening tool by calculating various diagnostic effeciency statistics.", style = "font-family: 'times'"),
                 p("The four cells (a, b, c, d) capture the four possible relationships between the results of a screening test
and a gold standard diagnosis.",
                   style = "font-family: 'times'"),
                 tags$ol(
                   tags$li("True positive (cell a): when the person scores positive on the screening test and does in fact have the condition.",
                           style = "font-family: 'times'"), 
                   tags$li("False positive (cell b): when the person scores positive on the screening test but does not in fact have the condition.",
                           style = "font-family: 'times'"), 
                   tags$li("False negative (cell c): when the person scores negative on the screening test but does in fact have the condition.",
                           style = "font-family: 'times'"),
                   tags$li("True negative (cell d): when the person scores negative on the screening test and does not in fact have the condition.",
                           style = "font-family: 'times'")
                 )),
        tabPanel("Sensitivity",
                 verbatimTextOutput("sensi"),
                 br(),
                 br(),
                 br(),
                 p("Sensitivity of the test reflects the probability that the screening test will be positive among those who are diseased.",
                   style = "font-family: 'times'")
                 ),
        tabPanel("Specificity", 
                 verbatimTextOutput("speci"),
                 br(),
                 br(),
                 br(),
                 p("Specificity of the test reflects the probability that the screening test will be negative among those who do not have the disease.",
                   style = "font-family: 'times'")),
        tabPanel("LR +",
                 verbatimTextOutput("LRpos"),
                 br(),
                 br(),
                 br(),
                 p("Positive likelihood ratio (LR+) is the ratio of the sensitivity of a test to the false positive rate. In general,",
                   strong("the higher the ratio, the better the test is."),"The LR+ is another index of the accuracy of the test and tells
                  what the odds are that a positive test result has come from a person who has the attribute. When the LR+ is 1, the test is useless.",
                   style = "font-family: 'times'")),
        tabPanel("LR -",
                 verbatimTextOutput("LRneg"),
                 br(),
                 br(),
                 br(),
                 p("Negative likelihood ratio (LR-) is the ratio of the false negative rate to the specificity of the test.",
                   strong("the lower the ratio, the better the test is."),
                   style = "font-family: 'times'")),
        tabPanel("PPV",
                 verbatimTextOutput("ppv"),
                 br(),
                 br(),
                 br(),
                 p("Positive predictive value is the probability that subjects with a positive screening test truly have the disease.",
                   style = "font-family: 'times'")),
        tabPanel("NPV",
                 verbatimTextOutput("npv"),
                 br(),
                 br(),
                 br(),
                 p("Negative predictive value is the probability that subjects with a negative screening test truly don't have the disease.",
                   style = "font-family: 'times'")),
        tabPanel("Kappa",
                 verbatimTextOutput("kap"),
                 br(),
                 br(),
                 br(),
                 p("A measure of the degree of nonrandom agreement between observers or measurements of the same categorical variable.",
                   strong("Complete agreement corresponds to K = 1, and lack of agreement corresponds to K = 0."),
                   style = "font-family: 'times'"),
                 p("The value of kappa can be interpreted as follows:",
                   style = "font-family: 'times'"),
                 img(src="kapinter2.jpg")),
        tabPanel("Accuracy",
                 verbatimTextOutput("acu"),
                 br(),
                 br(),
                 br(),
                 p("Accuracy tells us what proportion of all tests have given the correct result?",
                   style = "font-family: 'times'")),
        tabPanel("References",
                 br(),
                 br(),
                 br(),
                 tags$ol(
                   tags$li("Streiner DL. Diagnosing tests: Using and misusing diagnostic and screening tests. 
                   Journal of personality assessment. 2003 Dec 1;81(3):209-19.",
                           style = "font-family: 'times'"), 
                   tags$li("McHugh ML. Interrater reliability: the kappa statistic. Biochemia medica. 2012 Oct 15;22(3):276-82.",
                           style = "font-family: 'times'"), 
                   tags$li('Richardson R, Trepel D, Perry A, et al. Screening for psychological and mental health
                            difficulties in young people who offend: a systematic review and decision model. 
                            Southampton (UK): NIHR Journals Library; 2015 Jan. (Health Technology Assessment, 
                            No. 19.1.) Available from:',
                           a("http://www.ncbi.nlm.nih.gov/books/NBK269083/", href = "http://www.ncbi.nlm.nih.gov/books/NBK269083/"), 
                            'doi: 10.3310/hta19010',
                           style = "font-family: 'times'")
                 )
              )
      )
    )
    )
))