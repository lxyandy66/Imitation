# ����LXMģ���ж�̬���Իع�ϵ��
# ����������ͼ��Χ�����쳣ֵ
data.imt.para<-read.xlsx(file = "ParameterTest.xlsx",sheetIndex = 1)%>%as.data.table()

data.imt.para$a3%>%{ 
  mean(.[.>boxplot.stats(.)$stats[2] & .<boxplot.stats(.)$stats[4]],na.rm=TRUE)
}