#Pract9( Kruskal-Walis tests)
s1=sample.int(100,10)
s1
s2=sample.int(100,10)
s2
s=data.frame(s1,s2)
summary(s)

stacked=stack(s)
head(stacked)

kruskal.test(values~ind,data = stacked)
