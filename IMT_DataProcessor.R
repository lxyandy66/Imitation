# 处理LXM模型中动态线性回归系数
# 用于以箱形图范围处理异常值
data.imt.para<-read.xlsx(file = "ParameterTest.xlsx",sheetIndex = 1)%>%as.data.table()

data.imt.para$a3%>%{ 
  mean(.[.>boxplot.stats(.)$stats[2] & .<boxplot.stats(.)$stats[4]],na.rm=TRUE)
}
