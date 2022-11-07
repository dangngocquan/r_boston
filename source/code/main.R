# Tải dữ liệu
install.packages("MASS")
library(MASS)
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
install.packages("magrittr")
install.packages("dplyr")
install.packages("corrr")
library(magrittr)
library(dplyr)
library(corrr)
Boston %>% correlate(Boston) %>% focus(medv)

# Biểu đồ tương quan giữa các biến dự đoán
install.packages("GGally")
library(GGally)
png("D:\\Study\\R\\r_boston\\source\\images\\f04_Biểu đồ tương quan giữa các biến [result].png")
ggpairs(Boston[,-14], histogram=TRUE, pch=19)
dev.off()

# Biểu đồ tán xạ giữa biến mục tiêu và các biến dự báo khác
png("D:\\Study\\R\\r_boston\\source\\images\\f05_Biểu đồ tán xạ giữa biến mục tiêu và các biến dự báo khác [result].png")
library(tidyr)
library(dplyr )
Boston %>%
gather(-medv, key = "var", value = "value") %>%
filter(var != "chas") %>%
ggplot(aes(x = value, y = medv)) +
geom_point() +
stat_smooth() +
facet_wrap( var, scales = "free") +
theme_bw()
rlang::last_error()
dev.off()




