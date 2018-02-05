if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}

library(ggplot2)

ab_data <- read.csv("./antibiotics_data.csv", stringsAsFactors = FALSE)

library(reshape2)

ab_data$Penicilin <- log10(ab_data$Penicilin) 
ab_data$Streptomycin <- log10(ab_data$Streptomycin)
ab_data$Neomycin <- log10(ab_data$Neomycin)

ab_data2 <- t(ab_data)

breaks = 10**(-1:10)

colnames(ab_data2) <- ab_data$Bacteria
ab_data2 <- ab_data2[c(2:4),]
ab_data3 <- melt(ab_data2, id.vars = "row")
colnames(ab_data3) <- c("Treatment", "Bacteria", "Effectiveness")
# ab_data3$Effectiveness <- as.factor(1.0 / as.numeric(ab_data3$Effectiveness))
ggplot(ab_data3, aes(x = ab_data3$Bacteria, y = as.numeric(ab_data3$Effectiveness))) + 
  geom_bar(stat="identity", aes(fill=Treatment),width = 0.5, position = position_dodge(width=0.5)) +
  xlab("Bacteria") +
  ylab("Minimum Effective Dosage\nin mL per mL of infection") +
  ggtitle("Effective Dosage of Antibiotics for 1mL of Bacteria") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(breaks =  c(0.1, 3, 6, 9, 12, 15, 18, 24, 30), labels = c(0.1, 1000, 1000000, 1000000000, 1000000000000, 10 ** 15, 10**18, 10**24, 10**30)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

