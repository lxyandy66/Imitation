#### 本脚本用于生成丢包的马尔可夫链 ####
# 配合实际测试的网络情况
# file="网络情况统计_IoT_1230_Weak.xlsx"

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

write.xlsx(x=data.table(time=c(1:length(s)),isRcv=s),file="Model_MarkovPacketLoss.xlsx")

#随机抽样函数
generatorFunction<-function(currentState){ 
  sample(0:1,1,prob = info.prob.packetLoss[currentState+1,]) 
}


