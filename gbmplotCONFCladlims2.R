gbm.plotCONF <- function (gbm.object, variable.no = 0, smooth = FALSE, rug = TRUE, 
          n.plots = length(pred.names), common.scale = TRUE, write.title = TRUE, 
          y.label = "fitted function", x.label = labels, show.contrib = TRUE, 
          plot.layout = c(3, 4), ...) 
{
  if (!requireNamespace("gbm")) {
    stop("you need to install the gbm package to run this function")
  }
  requireNamespace("splines")
  gbm.call <- gbm.object$gbm.call
  gbm.x <- gbm.call$gbm.x
  pred.names <- gbm.call$predictor.names
  response.name <- gbm.call$response.name
  data <- gbm.call$dataframe
  
  max.plots <- plot.layout[1] * plot.layout[2]
  plot.count <- 0
  n.pages <- 1
  if (length(variable.no) > 1) {
    stop("only one response variable can be plotted at a time")
  }
  if (variable.no > 0) {
    n.plots <- 1
  }
  max.vars <- length(gbm.object$contributions$var)
  if (n.plots > max.vars) {
    n.plots <- max.vars
    warning("reducing no of plotted predictors to maximum available (", 
            max.vars, ")")
  }
  predictors <- list(rep(NA, n.plots))
  responses <- list(rep(NA, n.plots))
  for (j in c(1:n.plots)) {
    if (n.plots == 1) {
      k <- variable.no
    }
    else {
      k <- match(gbm.object$contributions$var[j], pred.names)
    }
    if (is.null(x.label)) {
      var.name <-  labels[j]
    }
    else {
      var.name <-labels[j]
    }
    pred.data <- data[, gbm.call$gbm.x[k]]
    response.matrix <- gbm::plot.gbm(gbm.object, k, return.grid = TRUE)
    predictors[[j]] <- response.matrix[, 1]
    if (is.factor(data[, gbm.call$gbm.x[k]])) {
      predictors[[j]] <- factor(predictors[[j]], levels = levels(data[, 
                                                                      gbm.call$gbm.x[k]]))
    }
    responses[[j]] <- response.matrix[, 2] - mean(response.matrix[, 
                                                                  2])
    if(j == 1) {
      ymin = min(origunlist$adj.lo)
      ymax = max(origunlist$adj.hi)
    } else {
      ymin = min(ymin,min(origunlist$adj.lo))
      ymax = max(ymax,max(origunlist$adj.hi))
      #ymin = -1
      #ymax = 1
    }
  }
  op <- par(no.readonly = TRUE)
  par(mfrow = plot.layout)
  for (j in c(1:n.plots)) {
    if (plot.count == max.plots) {
      plot.count = 0
      n.pages <- n.pages + 1
    }
    plot.count <- plot.count + 1
    if (n.plots == 1) {
      k <- match(pred.names[variable.no], gbm.object$contributions$var)
      if (show.contrib) {
        x.label <- paste(var.name, "  (RI: ", round(gbm.object$contributions[k, 2], 1), "%)", sep = "")
      }
    }
    else {
      k <- match(gbm.object$contributions$var[j], pred.names)
      var.name <- labels[j]
      if (show.contrib) {
        x.label <- paste(var.name, "  (RI: ", round(gbm.object$contributions[j, 2], 1), "%)", sep = "")
      }
      else x.label <- var.name
    }
    if (common.scale) {
      
      CIplot <- orig2[[k]]
      
      if (is.factor(data[,gbm.call$gbm.x[k]])) {
        plot(CIplot[[1]],CIplot$y - mean(CIplot$y),ylim=c(-1,1), type='l', xlab = x.label, ylab = y.label, ...) 
        plot(CIplot[[1]], CIplot$CIhi - mean(CIplot$y),ylim=c(-1,1), type='l', lwd = .5, add=TRUE, border="gray49",axes = FALSE, xlab = "", ylab = "")
        plot(CIplot[[1]], CIplot$CIlo - mean(CIplot$y),ylim=c(-1,1), type='l', lwd = .5, add=TRUE, border="gray49",axes = FALSE, xlab = "", ylab = "")
        legend("topleft",paste(round(gbm.object$contributions[j, 2], 1),"%", sep = ""), bty="n", adj=.5) 
        
        } else {
          plot(CIplot[[1]], CIplot$y - mean(CIplot$y), type="l", ylim=c(-1,1), xlab = x.label, ylab = y.label, ...)
          lines(CIplot[[1]], CIplot$CIhi - mean(CIplot$y),col="gray49",lty=3, ylim=c(-1,1))
          lines(CIplot[[1]], CIplot$CIlo - mean(CIplot$y),col="gray49", lty=3, ylim=c(-1,1))
          legend("topleft", paste(round(gbm.object$contributions[j, 2], 1), "%", sep = ""), bty="n", adj=.5) 
        }
      }
    if (smooth & is.vector(predictors[[j]])) {
      temp.lo <- loess(responses[[j]] ~ predictors[[j]], 
                       span = 0.3)
      lines(predictors[[j]], fitted(temp.lo), lty = 2, 
            col = 2)
    }
    if (plot.count == 1) {
      if (write.title) {
        title(paste(response.name, " - page ", n.pages, 
                    sep = ""))
      }
      if (rug & is.vector(data[, gbm.call$gbm.x[variable.no]])) {
        rug(quantile(data[, gbm.call$gbm.x[variable.no]], 
                     probs = seq(0, 1, 0.1), na.rm = TRUE))
      }
    }
    else {
      if (write.title & j == 1) {
        title(response.name)
      }
      if (rug & is.vector(data[, gbm.call$gbm.x[k]])) {
        rug(quantile(data[, gbm.call$gbm.x[k]], probs = seq(0, 
                                                            1, 0.1), na.rm = TRUE))
      }
    }
  }
  par(op)
}