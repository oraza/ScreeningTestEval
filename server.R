# server.R


server <- function(input, output) {

  
  sensitivity.val <- function(xx){(input$TP/(input$TP+input$FN))}
  specifivity.val <- function(xy){input$TN/(input$FP+input$TN)}
  lr.pos <- function(lrp){(input$TP/(input$TP+input$FN))/(1 - (input$TN/(input$FP+input$TN)))}
  lr.neg <- function(lrn){(1-(input$TP/(input$TP+input$FN)))/(input$TN/(input$FP+input$TN))}
  ppv.val <- function(aa){input$TP/(input$TP+input$FP)}
  npv.val <- function(ab){input$TN/(input$TN+input$FN)}

  
  output$sensi <- renderPrint(
    {sensitivity.val(input$xx)}
  )
  output$speci <- renderPrint(
    {specifivity.val(input$xy)}
  )
  output$LRpos <- renderPrint(
    {lr.pos(input$lrp)}
    )
  output$LRneg <- renderPrint(
    {lr.neg(input$lrn)}
  )
  output$ppv <- renderPrint(
    {ppv.val(input$aa)}
  )
  output$npv <- renderPrint(
    {npv.val(input$ab)}
  )
  output$kap <- renderPrint(
    {
      ((input$TP+input$TN)-((((input$TP+input$FP)*(input$TP+input$FN))+((input$FN+input$TN)*(input$FP+input$TN)))/(input$TP + input$FP + input$FN + input$TN)))/((input$TP + input$FP + input$FN + input$TN)-((((input$TP+input$FP)*(input$TP+input$FN))+((input$FN+input$TN)*(input$FP+input$TN)))/(input$TP + input$FP + input$FN + input$TN)))
    }
  )
  output$acu <- renderPrint(
    {
      (input$TP + input$TN)/(input$TP + input$FP + input$FN + input$TN)
    }
  )
  }

