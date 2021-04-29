Bland.Altman.re <-
  function(x,y,alpha = .05,rep.meas = FALSE,subject,xname = 'x',yname = 'y',addDensity =FALSE,...) {

    ## require library(ggplot2)
	## require library(gridExtra)
	
    #*** 1. Set a few constants
    z <- qnorm(1 - alpha / 2)  ## value of z corresponding to alpha
    d <- x - y               ## pair-wise differences
    m <- (x + y) / 2           ## pair-wise means
    
    #*** 2. Calculate mean difference
    d.mn <- mean(d,na.rm = TRUE)
    
    #*** 3. Calculate difference standard deviation
    if (rep.meas == FALSE) {
      d.sd = sqrt(var(d,na.rm = TRUE))
    }
    else{
      #*** 3a. Ensure subject is a factor variable
      if (!is.factor(subject))
        subject <- as.factor(subject)
      
      #*** 3b. Extract model information
      n <- length(levels(subject))      # Number of subjects
      model <- aov(d ~ subject)           # One way analysis of variance
      MSB <- anova(model)[[3]][1]       # Degrees of Freedom
      MSW <- anova(model)[[3]][2]       # Sums of Squares
      
      #*** 3c. Calculate number of complete pairs for each subject
      pairs <- NULL
      for (i in 1:length(levels(as.factor(subject)))) {
        pairs[i] <- sum(is.na(d[subject == levels(subject)[i]]) == FALSE)
      }
      Sig.dl <-
        (MSB - MSW) / ((sum(pairs) ^ 2 - sum(pairs ^ 2)) / ((n - 1) * sum(pairs)))
      d.sd <- sqrt(Sig.dl + MSW)
    }
    
    #*** 4. Calculate lower and upper confidence limits
    ucl <- d.mn + z * d.sd
    lcl <- d.mn - z * d.sd
    print(d.mn)
    print(ucl)
    print(lcl)
    
    #*** 5. Make Plot
    xlabstr = paste(c('Mean(',xname,', ',yname,')'), sep = "",collapse = "")
    ylabstr = paste(c(xname, ' - ', yname), sep = "",collapse = "")
    id = as.factor(rep(1,length(m)))
    xy <- data.frame(m,d,id)
    maxm <- max(m)
    if (addDensity == FALSE) {
      plot(
        m, d,abline(
          h = c(d.mn,ucl,lcl),col = '#ff7f27',lwd = 2
        ), col = '#357EC7',pch = 16,xlab = xlabstr,ylab = ylabstr,...
      )
    }
    else{
      empty <- ggplot() + geom_point(aes(1,1), colour = "white") +
        theme(
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
        )
      #scatterplot of x and y variables
      scatter <- ggplot(xy,aes(m, d)) +
        geom_point(aes(color = id,size=1.5)) +
        scale_color_manual(values = c("#2B547E", "purple")) +
        geom_abline(
          intercept = d.mn,slope = 0, colour = "#168EF7",size = 1
        ) +
        geom_abline(
          intercept = ucl,slope = 0,colour = "#F88017",size = 1,linetype = "dashed"
        ) +
        geom_abline(
          intercept = lcl,slope = 0,colour = "#F88017",size = 1,linetype = "dashed"
        ) +
        theme(
          legend.position = "none",axis.text = element_text(size = 12),axis.title = element_text(size = rel(1.5))
        ) +
        xlab(xlabstr) +
        ylab(ylabstr) +
        geom_text(x=maxm-2.4,y=d.mn+1.5,label='Mean',colour='#168EF7',size=6)+
        geom_text(x=maxm-2.4,y=ucl+1.5,label='+1.96SD',colour='#F88017',size=6)+
        geom_text(x=maxm-2.4,y=lcl+1.5,label='-1.96SD',colour='#F88017',size=6)
      
      #marginal density of x - plot on top
      plot_top <- ggplot(xy, aes(m,fill = id)) +
        geom_density(alpha = .5) +
        scale_fill_manual(values = c("#2B547E", "purple")) +
        theme(legend.position = "none") +
        xlab("") # set it as blank
      
      #marginal density of y - plot on the right
      plot_right <- ggplot(xy, aes(d,fill = id)) +
        geom_density(alpha = .5) +
        coord_flip() +
        scale_fill_manual(values = c("#2B547E", "purple")) +
        theme(legend.position = "none") +
        xlab("") # set it as blank
      grid.arrange(
        plot_top, empty, scatter, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights =
          c(1, 4)
      )
    }
    values <- round(cbind(lcl,d.mn,ucl),4)
    colnames(values) <- c("LCL","Mean","UCL")
    if (rep.meas == FALSE)
      Output <- list(limits = values,Var = d.sd ^ 2)
    else
      Output <- list(
        limits = values,Var = Sig.dl,MSB = MSB,MSW = MSW
      )
    return(Output)
  }