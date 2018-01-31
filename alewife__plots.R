alew=read.csv("~/RM11/alewife_data/alewife_data.csv")

ggplot(data=alew)+
  + geom_point(mapping=aes(x=fl,y=tl))

ggplot(data=alew)+
  + geom_point(mapping=aes(x=fl,y=tl,colour=m))+
  + ggtitle("Total length vs. Forklength")+
  + xlab("Fork length")+
  + ylab("Total Length")

ggplot(data=alew)+
  + geom_point(mapping=aes(x=fl,y=tl,color=sx))+
  + ggtitle("total length vs. forklength")+
  + xlab("Fork length")+
  + ylab("Total length")

ggplot(data=alew)+
  + geom_histogram(mapping=aes(x=m),binwidth=0.5)

ggplot(data=alew,mapping=aes(x=tl))+
  + geom_freqpoly(mapping=aes(color=sx),binwidth=10)

Boxplot: Sx vs. Tail length
ggplot(data=alew,mapping=aes(x=sx,y=tl))+
  + geom_boxplot()

To visualize the amount of male and female Alewife present in June and July 

ggplot(data=alew)+
  + geom_count(mapping=aes(x=sx,y=m))

Visualize the covariation between sx and month using geom_tile()
> alew %>%
  + count(sx,m) %>%
  + ggplot(mapping=aes(x=sx,y=m))+
  + geom_tile(mapping=aes(fill=n))







