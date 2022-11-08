# Các gói thư viện cần thiết
install.packages("MASS")
install.packages("magrittr")
install.packages("dplyr")
install.packages("corrr")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("DT")
install.packages("leaps")
install.packages("glmnet")
install.packages("PerformanceAnalytics")
library(corrr)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DT)
library(MASS)
library(leaps)
library(glmnet)
library(PerformanceAnalytics)

# Tải dữ liệu
data("Boston")
Boston

attach(Boston)



# Kiểm tra cấu trúc dữ liệu và giá trị trống của dữ liệu

# Kiểm tra dữ liệu trống
colSums(is.na(Boston))

# Xem dữ liệu tổng quan
summary(Boston)



# Giải quyết bài toán bằng mô hình hồi quy với R

# Tương quan giữa các biến
Boston %>% correlate(Boston) %>% focus(medv)

# Biểu đồ tương quan giữa các biến dự đoán và lưu biểu đồ
png("D:\\Study\\R\\r_boston\\source\\images\\f04_Biểu đồ tương quan giữa các biến [result].png")
chart.Correlation(Boston[,-14], histogram=TRUE, pch=19)
dev.off()

# Biểu đồ tán xạ giữa biến mục tiêu và các biến dự báo khác + Lưu biểu đồ
png("D:\\Study\\R\\r_boston\\source\\images\\f05_Biểu đồ tán xạ giữa biến mục tiêu và các biến dự báo khác [result].png")
Boston %>%
  gather(-medv, key = "var", value = "value") %>%
  filter(var != "chas") %>%
  ggplot(aes(x = value, y = medv)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()
dev.off()

# Biểu đồ hộp cho các biến dự đoán + Lưu biểu đồ
png("D:\\Study\\R\\r_boston\\source\\images\\f06_Biểu đồ hộp cho các biến dự báo [result].png")
Boston %>%
  gather(-medv, key = "var", value = "value") %>%
  filter(var != "chas") %>%
  ggplot(aes(x = '',y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()
dev.off()

# Biểu đồ tần suất cho các biến dự đoán + Lưu biểu đồ
png("D:\\Study\\R\\r_boston\\source\\images\\f07_Biểu đồ tần suất cho các biến dự báo [result].png")
Boston %>%
  gather(-medv, key = "var", value = "value") %>%
  filter(var != "chas") %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()
dev.off()

# Chia dữ liệu
set.seed(12420246)
index <- sample(nrow(Boston),nrow(Boston)*0.80)
Boston.train <- Boston[index,]
Boston.test <- Boston[-index,]



# Mô hình hồi quy tuyến tính sử dụng tất cả các biến dự đoán (Full Model)
model1 <- lm(medv~ ., data = Boston.train)
sum.model1 <- summary(model1)
sum.model1

# Thống kê về mô hình
model1.mse <- (sum.model1$sigma)^2
model1.rsq <- sum.model1$r.squared
model1.arsq <- sum.model1$adj.r.squared
test.pred.model1 <- predict(model1, newdata=Boston.test) 
model1.mpse <- mean((Boston.test$medv-test.pred.model1)^2)
model1.aic <- AIC(model1)
model1.bic <- BIC(model1)

stats.model1 <- c("full", model1.mse, model1.rsq, model1.arsq, model1.mpse, 
                  model1.aic, model1.bic)

comparison_table <- c("model type", "MSE", "R-Squared", "Adjusted R-Squared", 
                      "Test MSPE", "AIC", "BIC")
data.frame(cbind(comparison_table, stats.model1))




# Mô hình trích chọn đặc trưng (Subset selection)
# 3 phương pháp

# Phương pháp chọn biến Forward
model2 <- regsubsets(medv~ ., data = Boston.train, nvmax = 13, method="forward")
sum.model2 <- summary(model2)

model2.subsets <- cbind(sum.model2$which, sum.model2$bic, sum.model2$rsq, 
                        sum.model2$adjr2,sum.model2$cp)
model2.subsets <- as.data.frame(model2.subsets) 
colnames(model2.subsets)[15:18] <- c("BIC","rsq","adjr2","cp")
model2.subsets

# Biểu đồ của 13 mô hình theo các chỉ số + Lưu biểu đồ
png("D:\\Study\\R\\r_boston\\source\\images\\f10_ Biểu đồ của 13 mô hình theo các chỉ số [result].png")
rsq <- data.frame(round(sum.model2$rsq,5))
model2.rsq.plot <- ggplot(data = rsq, aes(y = rsq, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label = rsq), size = 3, vjust = -0.5) +
  scale_x_continuous(breaks=1:13)
model2.rsq.plot

adjr2 <- data.frame(round(sum.model2$adjr2,4))
model2.adjrsq.plot <- ggplot(data = adjr2, aes(y = adjr2, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=adjr2), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

bic <- data.frame(round(sum.model2$bic,4))
model2.bic.plot <- ggplot(data = bic, aes(y = bic, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=bic), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

cp <- data.frame(round(sum.model2$cp,4))
model2.cp.plot <- ggplot(data = cp, aes(y = cp, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=cp), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

grid.arrange(model2.rsq.plot,model2.adjrsq.plot,model2.bic.plot,model2.cp.plot, ncol=2)

dev.off()

