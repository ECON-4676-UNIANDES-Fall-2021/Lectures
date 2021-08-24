#######################################################################
#  Author: Ignacio Sarmiento-Barbieri (i.sarmiento at uniandes.edu.co)
# please do not cite or circulate without permission
#######################################################################

rm(list=ls())
cat("\014")

require("ggplot2")
set.seed(10101)

N<-20
x = runif(N,2,10)
u<- rchisq(N,25)
f<- 2 + 5*x
y = f +u 

db<-data.frame(x=x,y=y)

p<-ggplot(db,aes(x=x,y=y)) +
  geom_point(shape=1,alpha=1,size=2) + 
  theme_bw()  +
  #ylim(8,15) +
  theme(legend.position = "none",
        #axis.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

p
ggsave("fig_1.pdf",height = 7, width = 12)

p + 
  geom_smooth(method = lm, se = FALSE, col = "black",size=.5) +
  geom_segment(aes(y = predict(lm(y ~ x)), yend = y, x = x, xend = x), col = "red")
ggsave("fig_1b.pdf",height = 7, width = 12)

p + 
  geom_line(data=data.frame(spline(db, n=N*10)))
ggsave("fig_1c.pdf",height = 7, width = 12)




# # -----------------------------------------------------------------------
# Risk Example ------------------------------------------------------------
# # -----------------------------------------------------------------------
require("dplyr")
require("tidyr")
p_star<-seq(0,1,0.01)

p1<-function(p,n){
  p*(1-p)/n
}

p2<-function(p,n){
  n/(4*(n+sqrt(n))^2)
} 

db_risk1<-tibble(p=p_star,n=4)%>% 
            mutate(p1=p1(p,n),p2=p2(p,n)) %>% 
            pivot_longer(c(p1,p2),names_to="Estimators",values_to= "Risk")


ggplot(db_risk1) +
  geom_line(aes(x=p,y=Risk,group=Estimators,col=Estimators)) +
  theme_classic() 
ggsave("fig_2a.pdf",height = 7, width = 12)
  
db_risk2<-tibble(p=p_star,n=400)%>% 
  mutate(p1=p1(p,n),p2=p2(p,n)) %>% 
  pivot_longer(c(p1,p2),names_to="Estimators",values_to= "Risk")


ggplot(db_risk2) +
  geom_line(aes(x=p,y=Risk,group=Estimators,col=Estimators)) +
  theme_classic() 
ggsave("fig_2b.pdf",height = 7, width = 12)
