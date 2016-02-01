# Load ECP change point package
library(ecp)
library(dplyr)

# Create fake data -----------
fake <- data.frame() # create an empty data frame
for (i in 1:10) {
  base_num <- rnorm(1, mean = 5, sd = 2) 
  period1 <- rnorm(100)
  period2 <- rnorm(100, 0, base_num)
  period3 <- rnorm(100, base_num, 3)
  value <- matrix(c(period1, period2, period3), ncol = 1)
  temp <- data.frame(ID = i, value)
  fake <- rbind(fake, temp)
}

# Run change point model ---------------------------------
# Not data is assumed to be incorrect time series order
# use dplyr::arrange(GROUPING_VAR, TIME_VAR)

fake_with_change_points <- data.frame()
for (i in 1:length(unique(fake$ID))) {
  temp <- fake %>% filter(ID == i)
  temp_out <- e.divisive(X = as.matrix(temp['value']))
  # Extract position of the estimated change points
  estimated_cp <- temp_out$estimates[
    seq(from = 2, to = length(temp_out$estimates) - 1)]
  # Put change points in original data frame for indiv.
  est_changes <- rep(0, nrow(temp)) 
  est_changes[estimated_cp] <- 1
  
  # Combine into one data frame with all units
  temp$est_changes <- est_changes
  fake_with_change_points <- rbind(fake_with_change_points,
                                   temp)
}

## fake_with_change_points includes the original grouped time 
# series with a new dummy variable (est_changes) that is 1
# at the estimated change point and 0 otherwise.



###### outakes for making correct axis lines for graphs
# http://www.statmethods.net/advgraphs/axes.html 
# https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/grid.html

par(mfrow=c(2,1), #multiple graphs 
    mex = 0.8, #margin size
    ps = 9, #font size
    pty = "s", #or m
    bg = "white",
    tcl= -0.8)
axis(1, 
     at=seq(1, 
            445, 
            by=12), 
     labels=F, 
     lwd=1, 
     lwd.ticks=1)
abline(v=(12*(seq(2,32,by=5)))+1, col="lightgray",
       lty="dotted")
abline(h=(seq(0,150,25)), col="lightgray",
       lty="dotted")


par(tcl= -0.2)
axis(1, at=seq(1, 445, by=12), labels=F, lwd=1, lwd.ticks=1)

par(tcl= -0.5)
axis(1, at=seq(1 + 12*2, 450, by=60), labels=seq(1975,2010,5), lwd=0, lwd.ticks=2)

par(tcl= -0.5)
axis(2)

abline(v=(12*(seq(2,32,by=5)))+1, col="lightgray", lty="dotted")
abline(h=(seq(0,150,25)), col="lightgray", lty="dotted")