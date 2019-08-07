library(dplyr)
library(magrittr)
library(xlsx)
library(lattice)
library(ggplot2)
library(car)

student_por <- studentpor

por <- select(student_por, "sex", "higher", "G1", "G3")
por$sex <- factor(por$sex, levels = c("M", "F"), labels = c("female", "male"))  

# We need to filter test = 0 data
por1 <- por %>% filter(G3 > 0 & G1 > 0)
# Delete the 0 records.
por1 %>% head()
por1$G1 %>% hist(main = "Histogram Of First Grade", breaks = 20, col = "skyblue", xlab = "Grade")
por1$G3 %>% hist(main = "Histogram of Third Grade", breaks = 20, col = "red", xlab = "Grade")
summary(por1$G1)
summary(por1$G3)
por1 %>% group_by(sex)%>% summarise(Min = signif(min(G1,na.rm = TRUE),3),
                                     Q1 = signif(quantile(G1,probs = .25,na.rm = TRUE),3),
                                     Median = signif(median(G1, na.rm = TRUE),3),
                                     Q3 = signif(quantile(G1,probs = .75,na.rm = TRUE),3),
                                     Max = signif(max(G1,na.rm = TRUE),3),
                                     Mean =signif( mean(G1, na.rm = TRUE),4),
                                     SD = signif(sd(G1, na.rm = TRUE),4),
                                     IQR = signif(IQR(G1, na.rm = TRUE),3),
                                     n = n(),
                                     Missing = sum(is.na(G1)))

# Try to find if the higher education demand is same in both male and female 
tb1 <- table(por1$higher, por1$sex) 
knitr::kable(tb1)
tb2 <- table(por1$higher, por1$sex) %>% prop.table(margin = 2)
knitr::kable(tb2)
# Provide a crosstabulation about gender and demand of higher education.
tb2 %>% barplot(main = "The Demand of Higher Education In Different Gender",
                ylim = c(0, 1),
                legend = rownames(tb1), 
                beside = TRUE,
                args.legend = c(x = "top", horiz = TRUE, title = "Demand of Higher Education"),
                xlab="Gender",col = c("red3", "gray80"))
# The proportion of higher education demand in different gender.

chi2 <- chisq.test(table(por1$higher,por1$sex))
chi2
# summary of data
por1 %>% group_by(higher) %>% summarise(Mean = mean(G3,na.rm = TRUE),
                                        Median = median(G3, na.rm = TRUE),
                                        SD = sd(G3, na.rm = TRUE),
                                        Q1 = quantile(G3, probs = .25, na.rm = TRUE),
                                        Q3 = quantile(G3, probs = .75, na.rm = TRUE),
                                        Min = min(G3, na.rm = TRUE),
                                        Max = max(G3, na.rm = TRUE),
                                        IQR = IQR(G3, na.rm=TRUE),
                                        n = n()) -> table1 
knitr::kable(table1)
# Provide hisrogram of final grade divided by the desire of higher education.
por1 %>% histogram(~ G3|higher, col = "dodgerblue3",
                   layout = c(1, 2), data = ., xlab = "Grade of Final")
# boxplot
boxplot(
  por1$G3 ~ por1$higher,
  ylab = "Grade of G3",
  xlab = "want to take higher education")

# qqplot
# Because the sample size greater than 30, it is acceptable if we ignore this step.(not necessary)
por1$G3 %>% qqPlot(dist="norm")

leveneTest(G3 ~ higher, data = por1)
# Reject variances are equal. Hence use Welch test.
t.test(
  G3 ~ higher,
  data = por1,
  var.equal = FALSE,
  alternative = "less")

plot(G1 ~ G3, data = por1, xlab = "Grade of Final", ylab = "Grade of First")
R1 <- lm(G1 ~ G3, data = por1)
abline(R1, col = "red")

R1 %>% summary()
plot(R1)
<return>
  
)>
  