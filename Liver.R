#change names 
library(plyr)
liver_enzyme_data_complete_renamed <- rename(liver_enzyme_data_complete, 
                                             c("@_ALK00"="ALK00", "@_ALK01"="ALK01", "@_ALK02"="ALK02", "@_ALK04"="ALK04", "@_ALK08"="ALK08", "@_ALK52"="ALK52",
                                               "@_SGO00"="AST00", "@_SGO01"="AST01", "@_SGO02"="AST02", "@_SGO04"="AST04", "@_SGO08"="AST08", "@_SGO52"="AST52",
                                               "@_SGP00"="ALT00", "@_SGP01"="ALT01", "@_SGP02"="ALT02", "@_SGP04"="ALT04", "@_SGP08"="ALT08", "@_SGP52"="ALT52"))

#ALK Concentration
ALKConOverTimeComplete <- reshape(liver_enzyme_data_complete_renamed, varying = c("ALK00", "ALK01", "ALK02", "ALK04", "ALK08", "ALK52"),
                                  v.names = "ALKConcentration", timevar = "Time", times = c("Week 00", "Week 01", "Week 02", "Week 04", "Week 08", "Week 52"), direction = "long")
ALKConOverTimeComplete <- ALKConOverTimeComplete[, c("ALKConcentration", "Time", "SPLVL", "ASIMPC01_A", "PTID")]

library(car)
ALKConOverTimeComplete$DividedSPLVL <- recode(ALKConOverTimeComplete$SPLVL, "'C01':'C04' = 'C High';
                                              'C05':'C08' = 'C Low';
                                              'T01':'T06' = 'T High';
                                              'T07':'T12' = 'T Low'")

ALKConOverTimeComplete$Severity <- recode(ALKConOverTimeComplete$ASIMPC01_A, "'A':'B' = 'Complete'; 'C':'D' = 'Incomplete'")

library(ggplot2)

ggplot(data=subset(ALKConOverTimeComplete, !is.na(Severity)), aes(x= Time, y=ALKConcentration))+ 
  geom_boxplot(aes(fill = DividedSPLVL), width = 2) +
  scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod", "paleturquoise", "paleturquoise4")) +
  ggtitle("ALK Concentration in Complete and Incomplete Across T and C")+
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12))+
  facet_grid(Severity ~ Time, scales = "free_x") +
  geom_hline(yintercept = range(38, 126), colour = "black", linetype = "dashed", size = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 42, ymax = 130, fill = "blue", alpha = .1) +
  labs(caption="Shaded = Normal Concentration")

#AST Concentration
ASTConOverTimeComplete <- reshape(liver_enzyme_data_complete_renamed, varying = c("AST00", "AST01", "AST02", "AST04", "AST08", "AST52"),
                                  v.names = "ASTConcentration", timevar = "Time", times = c("Week 00", "Week 01", "Week 02", "Week 04", "Week 08", "Week 52"), direction = "long")
ASTConOverTimeComplete <- ASTConOverTimeComplete[, c("ASTConcentration", "Time", "SPLVL", "ASIMPC01_A", "PTID")]

library(car)
ASTConOverTimeComplete$DividedSPLVL <- recode(ASTConOverTimeComplete$SPLVL, "'C01':'C04' = 'C High';
                                              'C05':'C08' = 'C Low';
                                              'T01':'T06' = 'T High';
                                              'T07':'T12' = 'T Low'")

ASTConOverTimeComplete$Severity <- recode(ASTConOverTimeComplete$ASIMPC01_A, "'A':'B' = 'Complete'; 'C':'D' = 'Incomplete'")

library(ggplot2)

ggplot(data=subset(ASTConOverTimeComplete, !is.na(Severity)), aes(x= Time, y=ASTConcentration))+ 
  geom_boxplot(aes(fill = DividedSPLVL), width = 2, outlier.shape=NA) +
  coord_cartesian(ylim=c(0,200)) +
  scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod", "paleturquoise", "paleturquoise4")) +
  ggtitle("AST Concentration in Complete and Incomplete Across T and C")+
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12))+
  facet_grid(Severity ~ Time, scales = "free_x") +
  geom_hline(yintercept = range(18, 40), colour = "black", linetype = "dashed", size = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 18, ymax = 40, fill = "blue", alpha = .1) +
  labs(caption="Shaded = Normal Concentration")
