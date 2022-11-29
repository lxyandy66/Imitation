#### ���ű��������ɶ����������ɷ��� ####
# ���ʵ�ʲ��Ե��������
# file="�������ͳ��_IoT_1230_Weak.xlsx"

# ˳�� ��ǰ0 ��ǰ1
info.prob.packetLoss<-data.table(next0=c(0.903389831,0.04029304),
                                 next1=c(0.096610169,0.95970696)) #ÿ���к�Ϊ��ǰ��״̬

# ���������ɷ����ĳ���
TimeCount=1000




s<-1 #��һ������Ϊ�ǳɹ�����
for(t in 2:TimeCount){
  s<-c(s,generatorFunction(s[t-1]))
}

plot(s, pch = 20,cex = 0.5)

write.xlsx(x=data.table(time=c(1:length(s)),isRcv=s),file="Model_MarkovPacketLoss.xlsx")

#�����������
generatorFunction<-function(currentState){ 
  sample(0:1,1,prob = info.prob.packetLoss[currentState+1,]) 
}

