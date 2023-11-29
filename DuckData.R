library(ggplot2)
library(dplyr)
DuckData <- read.csv(file="/Users/kevinbuck/Desktop/BehavioralEcologyData.csv")
colnames(DuckData) <- c("Trial","Time","Ducks_Larger","Ducks_Smaller","Ducks_Away")
DuckData <- as.data.frame(DuckData)
DuckData <- DuckData %>% 
  mutate(Ratio=Ducks_Larger/Ducks_Smaller)
DuckData$Trial <- as.character(DuckData$Trial)
DuckData %>% 
  ggplot(aes(x= Trial, y = Ratio, fill = Trial)) +
  geom_boxplot() + ggtitle("Duck Free Distribution (by Trial)")
Ratio.Data <- DuckData$Ratio
Ratio.Data <- Ratio.Data[is.finite(Ratio.Data)]
summary(Ratio.Data)
sd(Ratio.Data)
mean()
mean_val <- mean(Ratio.Data)

print(t.test(Ratio.Data,mu=2))

ggplot(DuckData, aes(x = Ratio, fill = ..count..)) +
  geom_histogram(binwidth = 0.5, color = "white", alpha = 0.7) +
  geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Duck Free Distribution",
       x = "Ratio",
       y = "Frequency") +
  theme_minimal()