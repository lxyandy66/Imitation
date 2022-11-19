# 处理LXM模型中动态线性回归系数
# 用于以箱形图范围处理异常值
data.imt.para<-read.xlsx(file = "ParameterTest.xlsx",sheetIndex = 1)%>%as.data.table()

data.imt.para$a3%>%{ 
  mean(.[.>boxplot.stats(.)$stats[2] & .<boxplot.stats(.)$stats[4]],na.rm=TRUE)
}


#### 检测模型的准确性 ####

data.imt.valid<-read.xlsx(file="Modelling_Valid_Valve2Flowrate.xlsx",sheetIndex = 1)%>%as.data.table()

getRSquare(ref = data.imt.valid$Real,pred = data.imt.valid$Sim)
getMAPE(yPred = data.imt.valid[Real!=0]$Sim,yLook = data.imt.valid[Real!=0]$Real)
RMSE(pred = data.imt.valid$Sim,obs = data.imt.valid$Real,na.rm = TRUE)/
  mean(data.imt.valid$Real)

####对模拟的网络数据交换情况进行分析####
data.imt.net<-read.xlsx(file = "1020_Pre_NetworkExchangeImport.xlsx",sheetIndex = 1)%>%as.data.table()
data.imt.net.raw<-fread(file="1020_Pre_NetworkExchangeImport.csv")

# 按照MsgIdRcv（time-trigger）统计整体，统计所得丢包率不对
# 一个发送间隔内，接收端可能收到多个包，因此统计的丢包率不对，且偏大

data.imt.net.rcv<-data.imt.net.raw[,.(rcvTime=min(time,na.rm = TRUE)),by=MsgIdRcv]
data.imt.net.snd<-data.imt.net.raw[,.(sndTime=min(time,na.rm = TRUE)),by=MsgIdSnd]

data.imt.net.exchange<-merge(x=data.imt.net.snd,y=data.imt.net.rcv,
                             all.x = TRUE,by.x="MsgIdSnd",by.y="MsgIdRcv")
data.imt.net.exchange$isNewest<-apply()

