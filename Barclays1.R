
Barclays1 <- subset(combined, Code=="BARC_LN") #limit the unit to 



par(mfrow=c(1,1), #multiple graphs 
    mex = 0.8, #margin size
    ps = 9, #font size
    pty = "m", #or m
    bg = "white",
    tcl= -0.8)
plot(Barclays1$Days, Barclays1$SRISK)
