#######################################################################
#  Author: Ignacio Sarmiento-Barbieri (i.sarmiento at uniandes.edu.co)
# please do not cite or circulate without permission
#######################################################################


# Clean the workspace -----------------------------------------------------
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



# Load Packages -----------------------------------------------------------
pkg<-list("dplyr","ggplot2")
lapply(pkg, require, character.only=T)
rm(pkg)

setwd("~/Dropbox/Teaching/2021/BDL/Lectures/Lecture07")

N<-20
x  <- 0:19



db<-data.frame(k=rep(x,3),
               theta=c(rep(0.05,N),rep(0.10,N),rep(0.20,N)))
db<- db %>% mutate(Prob=dbinom(x,N,prob=theta),
                   theta=as.factor(theta))               
               


ggplot(db %>% filter(theta=="0.05")) +
  geom_bar(aes(x = k, y = Prob,group=theta ),stat = "identity",width=.8, position = "dodge") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  xlab("x") + 
  ylab("Density") + 
  theme_classic()  +
  theme(legend.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title=element_text(size=20)
  )
ggsave("figures/fig_1a.pdf",height = 7, width = 12)

ggplot(db %>% filter(theta=="0.1")) +
  geom_bar(aes(x = k, y = Prob,group=theta ),stat = "identity",width=.8, position = "dodge") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  xlab("X") + 
  ylab("Density") + 
  theme_classic()  +
  theme(legend.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title=element_text(size=20)
  )
ggsave("figures/fig_1b.pdf",height = 7, width = 12)


ggplot(db %>% filter(theta=="0.2")) +
  geom_bar(aes(x = k, y = Prob,group=theta ),stat = "identity",width=.8, position = "dodge") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  xlab("x") + 
  ylab("Density") + 
  theme_classic()  +
  theme(legend.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title=element_text(size=20)
  )
ggsave("figures/fig_1c.pdf",height = 7, width = 12)


ggplot(db) +
  geom_bar(aes(x = k, y = Prob,fill=theta,group=theta ),stat = "identity",width=.8, position = "dodge") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  xlab("x") + 
  ylab("Density") + 
  theme_classic()  +
  theme(legend.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title=element_text(size=20)
  )
ggsave("figures/fig_1.pdf",height = 7, width = 12)


choose(20,0)*.95^20

choose(20,0)*.8^20

choose(20,1)*0.05*.95^19
  

x <- seq(0,1,length=100)
beta_dist <- data.frame(theta=x, prob=dbeta(x,2,40))

ggplot(beta_dist, aes(theta,prob)) +
  geom_line() +
  xlab("% Infected Population") + 
  ylab("Density") + 
  scale_x_continuous(breaks = round(seq(min(beta_dist$theta), max(beta_dist$theta), by = 0.01),1)) +
  theme_bw()  +
  theme(legend.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title=element_text(size=20)
  )
ggsave("figures/fig_2.pdf",height = 7, width = 12)




beta_dist <- data.frame(theta=x, prob=dbeta(x,2,20))

ggplot(beta_dist, aes(theta,prob)) +
  geom_line() +
  xlab("% Infected Population") + 
  ylab("Density") + 
  scale_x_continuous(breaks = round(seq(min(beta_dist$theta), max(beta_dist$theta), by = 0.01),1)) +
  theme_bw()  +
  theme(legend.title =element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title=element_text(size=20)
  )
ggsave("figures/fig_beta.pdf",height = 7, width = 12)

