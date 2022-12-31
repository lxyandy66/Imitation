#### 本脚本用于生成丢包的马尔可夫链 ####
# 配合实际测试的网络情况
# file="网络情况统计_IoT_1230_Weak.xlsx"

####整体思路####
# 定义数个网络状态，如“忙”/“闲”--------（如何具体定义？
# 每个状态中对应不同的丢包概率和延迟分布



# 顺序 当前0 当前1
info.prob.packetLoss<-data.table(next0=c(0.903389831,0.04029304),
                                 next1=c(0.096610169,0.95970696)) #每行行号为当前的状态


# 生成马尔可夫链的长度
TimeCount=1000


s<-1 #第一个包认为是成功发送
for(t in 2:TimeCount){
  s<-c(s,generatorFunction(s[t-1]))
}

plot(s, pch = 20,cex = 0.5)

data.network.schedule<-data.table(time=c(1:TimeCount),isRcv=s,delay=abs(rnorm(TimeCount,mean = 0.1963753,sd = 0.1248156)))

ggplot(data = data.network.schedule,aes(x=time,y=delay))+geom_line()
ggplot(data = data.network.schedule,aes(x=delay))+geom_density()

write.xlsx(x=data.network.schedule,file="Model_NetworkSchedule.xlsx")

#随机抽样函数
generatorFunction<-function(currentState){ 
  sample(0:1,1,prob = info.prob.packetLoss[currentState+1,]) 
}


