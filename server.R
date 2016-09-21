# server.R

server <- function(input, output) {
  ### functions to be used:
  ## for indices:
  sensitivity.val <- function(xx){input$TP/(input$TP+input$FN)}
  specifivity.val <- function(xy){input$TN/(input$FP+input$TN)}
  lr.pos <- function(lrp){(input$TP/(input$TP+input$FN))/(1 - (input$TN/(input$FP+input$TN)))}
  lr.neg <- function(lrn){(1-(input$TP/(input$TP+input$FN)))/(input$TN/(input$FP+input$TN))}
  ppv.val <- function(aa){input$TP/(input$TP+input$FP)}
  npv.val <- function(ab){input$TN/(input$TN+input$FN)}
  
  ## for 95% CIs
  sd.err.sen <- function(senci){sqrt((input$TP/(input$TP+input$FN))*(1-(input$TP/(input$TP+input$FN))))/(input$TP+input$FN)}
  
  # ci for sensitivity
  sensi.ciLB <- function(senLB){((input$TP/(input$TP+input$FN)) - 1.96 * (sqrt(((input$TP/(input$TP+input$FN))*(1-(input$TP/(input$TP+input$FN))))/(input$TP+input$FN))))}
  sensi.ciUB <- function(senUB){((input$TP/(input$TP+input$FN)) + 1.96 * (sqrt(((input$TP/(input$TP+input$FN))*(1-(input$TP/(input$TP+input$FN))))/(input$TP+input$FN))))}
  
  # ci for sensitivity
  speci.ciLB <- function(specLB){((input$TN/(input$FP+input$TN)) - 1.96 * (sqrt(((input$TN/(input$FP+input$TN))*(1-(input$TN/(input$FP+input$TN))))/(input$FP+input$TN))))}
  speci.ciUB <- function(specUB){((input$TN/(input$FP+input$TN)) + 1.96 * (sqrt(((input$TN/(input$FP+input$TN))*(1-(input$TN/(input$FP+input$TN))))/(input$FP+input$TN))))}
  
  # ci for LR+
  lrpos.ciLB <- function(lrpLB){
    #SE:
    lrp.se <- sqrt((1/input$TP)-(1/(input$TP+input$FN))+(1/input$FP)-(1/(input$FP+input$TN)))
    #95% ci:
    (input$TP/(input$TP+input$FN))/(1 - (input$TN/(input$FP+input$TN))) / exp(1.96 * lrp.se)
  }
  lrpos.ciUB <- function(lrpUB){
    #SE:
    lrp.se <- sqrt((1/input$TP)-(1/(input$TP+input$FN))+(1/input$FP) - (1/(input$FP+input$TN)))
    #95% ci:
    (input$TP/(input$TP+input$FN))/(1 - (input$TN/(input$FP+input$TN))) * exp(1.96 * lrp.se)
  }
  # ci for LR-
  lrneg.ciLB <- function(lrnLB){
    #SE:
    lrn.se <- sqrt((1/input$FN)-(1/(input$TP+input$FN))+(1/input$TN) - (1/(input$FP+input$TN)))
    #95% ci:
    (1-(input$TP/(input$TP+input$FN)))/(input$TN/(input$FP+input$TN)) / exp(1.96 * lrn.se)
  }
  lrneg.ciUB <- function(lrnUB){
    #SE:
    lrn.se <- sqrt((1/input$FN)-(1/(input$TP+input$FN))+(1/input$TN)-(1/(input$FP+input$TN)))
    #95% ci:
    (1-(input$TP/(input$TP+input$FN)))/(input$TN/(input$FP+input$TN)) * exp(1.96 * lrn.se)
  }
  # ci for ppv
  ppv.ciLB <- function(ppvLB){
    #SE:
    ppv.se <- sqrt(((input$TP/(input$TP+input$FP)) * (1 - (input$TP/(input$TP+input$FP))))/(input$TP+input$FP))
    #95% ci:
    (input$TP/(input$TP+input$FP)) - 1.96 * ppv.se
  }
  ppv.ciUB <- function(ppvUB){
    #SE:
    ppv.se <- sqrt(((input$TP/(input$TP+input$FP)) * (1 - (input$TP/(input$TP+input$FP))))/(input$TP+input$FP))
    #95% ci:
    (input$TP/(input$TP+input$FP)) + 1.96 * ppv.se
  }
  # ci for npv
  npv.ciLB <- function(npvLB){
    #SE:
    npv.se <- sqrt(((input$TN/(input$TN+input$FN)) * (1 - (input$TN/(input$TN+input$FN))))/(input$FN+input$TN))
    #95% ci:
    (input$TN/(input$TN+input$FN)) - 1.96 * npv.se
  }
  npv.ciUB <- function(npvUB){
    #SE:
    npv.se <- sqrt(((input$TN/(input$TN+input$FN)) * (1 - (input$TN/(input$TN+input$FN))))/(input$FN+input$TN))
    #95% ci:
    (input$TN/(input$TN+input$FN)) + 1.96 * npv.se
  }
  # ci for kappa
  kap.ciLB <- function(kLB){
    #SE:
    n = input$TP + input$FP + input$FN + input$TN
    p = (input$TP + input$TN)/n
    pe = ((input$TP+input$FN)*(input$TP+input$FP) + (input$TN+input$FP)*(input$TN+input$FN))/ (n)^2
    kap.se <- sqrt((p*(1-p))/((n)*(1-pe)^2))
    #95% ci:
    kap <- ((input$TP+input$TN)-((((input$TP+input$FP)*(input$TP+input$FN))+((input$FN+input$TN)*(input$FP+input$TN)))/(input$TP + input$FP + input$FN + input$TN)))/((input$TP + input$FP + input$FN + input$TN)-((((input$TP+input$FP)*(input$TP+input$FN))+((input$FN+input$TN)*(input$FP+input$TN)))/(input$TP + input$FP + input$FN + input$TN)))
    kap - (1.96 * kap.se)
  }
  kap.ciUB <- function(kUB){
    #SE:
    n = input$TP + input$FP + input$FN + input$TN
    p = (input$TP + input$TN)/n
    pe = ((input$TP+input$FN)*(input$TP+input$FP) + (input$TN+input$FP)*(input$TN+input$FN))/ (n)^2
    kap.se <- sqrt((p*(1-p))/((n)*(1-pe)^2))
    #95% ci:
    kap <- ((input$TP+input$TN)-((((input$TP+input$FP)*(input$TP+input$FN))+((input$FN+input$TN)*(input$FP+input$TN)))/(input$TP + input$FP + input$FN + input$TN)))/((input$TP + input$FP + input$FN + input$TN)-((((input$TP+input$FP)*(input$TP+input$FN))+((input$FN+input$TN)*(input$FP+input$TN)))/(input$TP + input$FP + input$FN + input$TN)))
    kap + (1.96 * kap.se)
  }
  # ci for accuracy
  acc.ciLB <- function(accLB){
    #SE:
    n <- input$TP + input$FP + input$FN + input$TN
    p <- (input$TP + input$TN)/n
    acc.se <- sqrt((p * (1 - p))/n) 
    #95% ci:
    p - (1.96 * acc.se)
  }
  acc.ciUB <- function(accUB){
    #SE:
    n <- input$TP + input$FP + input$FN + input$TN
    p <- (input$TP + input$TN)/n
    acc.se <- sqrt((p * (1 - p))/n)
    #95% ci:
    p + (1.96 * acc.se)
  }
  
  ## Indices:
  output$sensi <- renderText(
    round({sensitivity.val(input$xx)}, 3)
  )
  output$speci <- renderText(
    round({specifivity.val(input$xy)}, 3)
  )
  output$LRpos <- renderText(
    round({lr.pos(input$lrp)}, 3)
    )
  output$LRneg <- renderText(
    round({lr.neg(input$lrn)}, 3)
  )
  output$ppv <- renderText(
    round({ppv.val(input$aa)}, 3)
  )
  output$npv <- renderText(
    round({npv.val(input$ab)}, 3)
  )
  output$kap <- renderText(
    {
      round(((input$TP+input$TN)-((((input$TP+input$FP)*(input$TP+input$FN))+((input$FN+input$TN)*(input$FP+input$TN)))/(input$TP + input$FP + input$FN + input$TN)))/((input$TP + input$FP + input$FN + input$TN)-((((input$TP+input$FP)*(input$TP+input$FN))+((input$FN+input$TN)*(input$FP+input$TN)))/(input$TP + input$FP + input$FN + input$TN))), 3)
    }
  )
  output$acu <- renderText(
    {
      round((input$TP + input$TN)/(input$TP + input$FP + input$FN + input$TN), digits = 3)
    }
  )
  # 95% CIs
  output$sensi.95ci <- renderText(
    round(c({sensi.ciLB(input$senLB)},
            {sensi.ciUB(input$senUB)}), digits = 3 )
  )
  output$speci.95ci <- renderText(
    round(c({speci.ciLB(input$specLB)},
            {speci.ciUB(input$specUB)}), digits = 3 )
  )
  output$lrpos.95ci <- renderText(
    round(c({lrpos.ciLB(input$lrpLB)},
            {lrpos.ciUB(input$lrpUB)}), digits = 3 )
  )
  output$lrneg.95ci <- renderText(
    round(c({lrneg.ciLB(input$lrnLB)},
            {lrneg.ciUB(input$lrnUB)}), digits = 3 )
  )
  output$ppv.95ci <- renderText(
    round(c({ppv.ciLB(input$ppvLB)},
            {ppv.ciUB(input$ppvUB)}), digits = 3 )
  )
  output$npv.95ci <- renderText(
    round(c({npv.ciLB(input$npvLB)},
            {npv.ciUB(input$npvUB)}), digits = 3 )
  )
  output$kap.95ci <- renderText(
    round(c({kap.ciLB(input$kLB)},
            {kap.ciUB(input$kUB)}), digits = 3 )
  )
  output$acc.95ci <- renderText(
    round(c({acc.ciLB(input$accLB)},
            {acc.ciUB(input$accUB)}), digits = 3 )
  )
}

