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
png("D:\\Study\\R\\r_boston\\source\\images\\
    f10_zBiểu đồ của 13 mô hình theo các chỉ số [result].png")
rsq <- data.frame(round(sum.model2$rsq,5))
model2.rsq.plot <- ggplot(data = rsq, aes(y = rsq, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=rsq), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

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

grid.arrange(model2.rsq.plot,model2.adjrsq.plot,
             model2.bic.plot,model2.cp.plot, ncol=2)

dev.off()

# Mô hình có chỉ số R-squared lớn nhất
which.max(sum.model2$rsq)

# Mô hình có chỉ số adjusted R-squared lớn nhất
which.max(sum.model2$adjr2)

# Mô hình có chỉ số cp thấp nhất
which.min(sum.model2$cp)

# Mô hình có chỉ số BIC thấp nhất
which.min(sum.model2$bic)

# Tổng quan về mô hình thứ 11
coef(model2,11)





# Phương pháp chọn biến Backward
model3 <- regsubsets(medv~ ., data = Boston.train, nvmax = 13, method="backward")
sum.model3 <- summary(model3)

model3.subsets <- cbind(sum.model3$which, sum.model3$bic, sum.model3$rsq, 
                        sum.model3$adjr2,sum.model3$cp)
model3.subsets <- as.data.frame(model3.subsets) 
colnames(model3.subsets)[15:18] <- c("BIC","rsq","adjr2","cp")
model3.subsets

# Biểu đồ của 13 mô hình theo các chỉ số + Lưu biểu đồ
png("D:\\Study\\R\\r_boston\\source\\images\\
    f11_zBiểu đồ của 13 mô hình theo các chỉ số [result].png")
rsq <- data.frame(round(sum.model3$rsq,5))
model3.rsq.plot <- ggplot(data = rsq, aes(y = rsq, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=rsq), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

adjr2 <- data.frame(round(sum.model3$adjr2,4))
model3.adjrsq.plot <- ggplot(data = adjr2, aes(y = adjr2, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=adjr2), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

bic <- data.frame(round(sum.model3$bic,4))
model3.bic.plot <- ggplot(data = bic, aes(y = bic, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=bic), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

cp <- data.frame(round(sum.model3$cp,4))
model3.cp.plot <- ggplot(data = cp, aes(y = cp, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=cp), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

grid.arrange(model3.rsq.plot,model3.adjrsq.plot,model3.bic.plot,model3.cp.plot, ncol=2)
dev.off()

# Mô hình có chỉ số R-squared lớn nhất
which.max(sum.model3$rsq)

# Mô hình có chỉ số adjusted R-squared lớn nhất
which.max(sum.model3$adjr2)

# Mô hình có chỉ số cp thấp nhất
which.min(sum.model3$cp)

# Mô hình có chỉ số BIC thấp nhất
which.min(sum.model3$bic)

# Tổng quan về mô hình thứ 11
coef(model3,11)





# Phương pháp chọn biến Exhaustive
model4 <- regsubsets(medv~ ., data = Boston.train, nvmax = 13)
sum.model4 <- summary(model4)

model4.subsets <- cbind(sum.model4$which, sum.model4$bic, sum.model4$rsq, sum.model4$adjr2,sum.model4$cp)
model4.subsets <- as.data.frame(model4.subsets) 
colnames(model4.subsets)[15:18] <- c("BIC","rsq","adjr2","cp")
model4.subsets

# Biểu đồ của 13 mô hình theo các chỉ số + Lưu biểu đồ
png("D:\\Study\\R\\r_boston\\source\\images\\
    f12_zBiểu đồ của 13 mô hình theo các chỉ số [result].png")
rsq <- data.frame(round(sum.model4$rsq,5))
model4.rsq.plot <- ggplot(data = rsq, aes(y = rsq, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=rsq), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

adjr2 <- data.frame(round(sum.model4$adjr2,4))
model4.adjrsq.plot <- ggplot(data = adjr2, aes(y = adjr2, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=adjr2), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

bic <- data.frame(round(sum.model4$bic,4))
model4.bic.plot <- ggplot(data = bic, aes(y = bic, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=bic), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

cp <- data.frame(round(sum.model4$cp,4))
model4.cp.plot <- ggplot(data = cp, aes(y = cp, x = 1:13)) + 
  geom_point() + geom_line() + 
  geom_text(aes(label=cp), size=3, vjust=-0.5) +
  scale_x_continuous(breaks=1:13)

grid.arrange(model4.rsq.plot,model4.adjrsq.plot,model4.bic.plot,model4.cp.plot, ncol=2)
dev.off()

# Mô hình có chỉ số R-squared lớn nhất
which.max(sum.model4$rsq)

# Mô hình có chỉ số adjusted R-squared lớn nhất
which.max(sum.model4$adjr2)

# Mô hình có chỉ số cp thấp nhất
which.min(sum.model4$cp)

# Mô hình có chỉ số BIC thấp nhất
which.min(sum.model4$bic)

# Tổng quan về mô hình thứ 11
coef(model4,11)




# Tổng quan về mô hình được chọn
model.ss <- lm(medv ~ . -indus -age, data=Boston.train)
sum.model.ss <- summary(model.ss)
sum.model.ss

# Các chỉ số của mô hình
model.ss.mse <- (sum.model.ss$sigma)^2
model.ss.rsq <- sum.model.ss$r.squared
model.ss.arsq <- sum.model.ss$adj.r.squared
test.pred.model.ss <- predict(model.ss, newdata=Boston.test) 
model.ss.mpse <- mean((Boston.test$medv-test.pred.model.ss)^2)
modelss.aic <- AIC(model.ss)
modelss.bic <- BIC(model.ss)

stats.model.ss <- c("model.SS", model.ss.mse, model.ss.rsq, model.ss.arsq, 
                    model.ss.mpse, modelss.aic, modelss.bic)

data.frame(cbind(comparison_table, stats.model.ss))







# Mô hình hồi quy LASSO

# Chuẩn hóa biến
Boston.X.std <- scale(dplyr::select(Boston, -medv))
X.train<- as.matrix(Boston.X.std)[index,]
X.test<- as.matrix(Boston.X.std)[-index,]
Y.train<- Boston[index, "medv"]
Y.test<- Boston[-index, "medv"]

# Xây dựng mô hình + Lưu biểu đồ
png("D:\\Study\\R\\r_boston\\source\\images\\
    f14_zXây dựng mô hình [result].png")
lasso.fit<- glmnet(x=X.train, y=Y.train, alpha = 1)
plot(lasso.fit, xvar = "lambda", label=TRUE)
dev.off()

# Tìm kiếm hệ số Lambda tối ưu thông qua kiểm chứng chéo + Lưu kết quả
png("D:\\Study\\R\\r_boston\\source\\images\\
    f14_zzTìm kiếm hệ số Lambda tối ưu thông qua kiểm chứng chéo [result].png")
cv.lasso<- cv.glmnet(x=X.train, y=Y.train, alpha = 1, nfolds = 10)
plot(cv.lasso)
dev.off()

# Hệ số λmin
cv.lasso$lambda.min

# Hệ số λ1se
cv.lasso$lambda.1se

# Các hệ số của mô hình ứng với λmin
coef(lasso.fit, s=cv.lasso$lambda.min)

# Các hệ số của mô hình ứng với giá trị λ1se
coef(lasso.fit, s=cv.lasso$lambda.1se)

# Các mô hình thống kê
pred.lasso.train.min <- predict(lasso.fit, newx = X.train, s=cv.lasso$lambda.min)
pred.lasso.train.1se <- predict(lasso.fit, newx = X.train, s=cv.lasso$lambda.1se)
pred.lasso.test.min<- predict(lasso.fit, newx = X.test, s=cv.lasso$lambda.min)
pred.lasso.test.1se<- predict(lasso.fit, newx = X.test, s=cv.lasso$lambda.1se)
lasso.min.mse <- sum((Y.train-pred.lasso.train.min)^2)/(404-14)
lasso.1se.mse <- sum((Y.train-pred.lasso.train.1se)^2)/(404-11)
lasso.min.mpse <- mean((Y.test-pred.lasso.test.min)^2)
lasso.1se.mpse <- mean((Y.test-pred.lasso.test.1se)^2)
sst <- sum((Y.train - mean(Y.train))^2)
sse_min <- sum((Y.train-pred.lasso.train.min)^2)
sse_1se <- sum((Y.train-pred.lasso.train.1se)^2)
rsq_min <- 1 - sse_min / sst
rsq_1se <- 1 - sse_1se / sst
adj_rsq_min <- 1 - (dim(X.train)[1]-1)*(1-rsq_min)/(dim(X.train)[1]-12-1)
adj_rsq_1se <- 1 - (dim(X.train)[1]-1)*(1-rsq_1se)/(dim(X.train)[1]-10-1)
stats.model.lasso.min <- c("model.lasso.min", lasso.min.mse, rsq_min, adj_rsq_min, lasso.min.mpse)
stats.model.lasso.1se <- c("model.lasso.1se", lasso.1se.mse, rsq_1se, adj_rsq_1se, lasso.1se.mpse)
comparison_table <- c("model type", "MSE", "R-Squared", "Adjusted R-Squared", "Test MSPE")
data.frame(cbind(comparison_table, stats.model.lasso.min, stats.model.lasso.1se))




# So sánh giữa các mô hình
data.frame(cbind(comparison_table, 
                 c("full", model1.mse, model1.rsq, 
                   model1.arsq, model1.mpse), 
                 c("model.SS", model.ss.mse, 
                   model.ss.rsq, model.ss.arsq, 
                   model.ss.mpse), 
                 stats.model.lasso.min, 
                 stats.model.lasso.1se)
           )



# Phát triển mô hình
new_model = update(model.ss,~. +I(rm^2) + I(lstat^2))
summary(new_model)



