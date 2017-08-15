## NONMEM Model Plots
## Samuel Callisto
## 20 April 2017
#####################

## ONLY CHANGE THESE TWO VARIABLES ##
path = 'C:/Users/Sam/NONMEM_Runs/TPM-PK-Transit/'
model = "0065"
######################################

library(ggplot2)
library(reshape)

tab <- data.frame(read.table (file=paste0(path,model,'.tab'),
                              skip=1, header=T))
tab.long <- melt(tab, id=c("ID", "TIME"), measure=c("DV", "IPRED"))

ggplot(tab.long, aes(x = TIME, y = value, colour=variable)) +
  geom_line(size=1) +     # Set linetype
  geom_point(size=1, fill="white") +         # Use larger points, fill with white
  expand_limits(y=0) +                       # Set y range to include 0
  scale_linetype_discrete(name="Subject ID") +
  xlab("Time After Dose (hr)") + ylab("log(10) TPM Plasma Conc (ug/mL)") + # Set axis labels
  ggtitle(paste0("Model ", model, " DV & IPRED v TIME_Individual")) +     # Set title
#  ggtitle("Transit Model Structural Only") +
  theme_bw() +
  facet_wrap(~ID) +
  geom_hline(yintercept=-1)
