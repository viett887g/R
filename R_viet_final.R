install.packages("ggplot2", lib="C:\\Program Files\\R\\R-4.1.0")
library(ggplot2)
library("readxl")
install.packages('plotrix')
library(plotrix)
library(scales)
library(plotrix)
install.packages('readxl')
setwd("C:/Users/ADMIN/Downloads")
my_data= read_excel("Hotel.xlsx")
df1= na.omit(my_data)
df= data.frame(df1)

head(df,5)

fert=df$Reviews
hh=df$VeryGood
average=df$Average
poor= df$Poor

#Tinh cac gia tri
table(fert)

#Do dai
dodai<-length(fert)
print(dodai)

#Trung binh  
trung_binh<-mean(fert)
print(trung_binh)

#Yeu vi
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
yeu_vi<-getmode(fert)
print(yeu_vi)

#Trung vi
trung_vi<- median(fert)
print(trung_vi)

#Phuong sai
phuong_sai<-var(fert)
print(phuong_sai)

#Do lech chuan
do_lech_chuan<-sd(fert)
print(do_lech_chuan)

#Sai so chuan
sai_so_chuan = sd(fert)/sqrt(length(fert))
print(sai_so_chuan)

#gia tri nho nhat
gt_nho_nhat=min(fert)
print(gt_nho_nhat)

#gia tri lon nhat
gt_lon_nhat=max(fert)
print(gt_lon_nhat)
#tứ phân vị
duration = fert
quantile(duration) 

#gan gia tri de ve bieu do
data <- df1[,13:17]
new_data <- c()
names <- colnames(data)
for (i in data){
  
  new_data<-append(new_data,sum(i))
}
new_data
data <- c(average,poor,hh,fert)
df <- data.frame(names,new_data)

#Ve bieu do
plot(fert,type="l",col='red',main = "dữ liệu khách sạn biểu đồ Lines",ylab = "count")
lines(hh,type="l",col='blue')

#box
boxplot(fert,hh,
        main = "dữ liệu khách sạn tại Reviews và Verrygood",
        xlab = "count",
        ylab = "Reviews và Verygood",
        col = "lightblue",
        border = "brown",horizontal = TRUE)
# biêu đồ bar
ggplot(data=df, aes(x=names, y=new_data)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=new_data), vjust=-0.3, size=3.5)+
  theme_minimal()
#bieu do tron
ggplot(df, aes(x = "", y = new_data, fill = names)) +
  geom_col(color = "black") +
  geom_text(aes(label = new_data),
            position = position_stack(vjust = 0.5)) +coord_polar(theta = "y")
