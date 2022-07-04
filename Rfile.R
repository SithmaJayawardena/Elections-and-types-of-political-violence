# Do elections attract violent or non-violent or both types of engagements?
# Logistic regression models

library(ggplot2)
library(sjPlot)
library(webshot)
library(readxl)

# Importing the dataset

options(warn = -2)
options(warn = -1)
full = read_xlsx('/Users/sithmasineka/Desktop/Research_3/R project/Project1/full.xlsx')
df1 = as.data.frame(full)
head(df1)

# Political violence event types

ggplot(data = full, aes(x=as.factor(event_type), fill=as.factor(event_type))) + scale_fill_hue(c = 60) +
  geom_bar() + theme(legend.position="none") + ggtitle('Political violence event types') + xlab('Count') + 
  ylab('Event type') + theme(axis.text=element_text(size=12), axis.title=element_text(size=14),title = element_text(size = 14),axis.text.x = element_text(angle = 45,vjust = 0.6,hjust = 0.5))

# data pre-processing

dff1 = dummy.data.frame(df1, names = 'event_type' , sep = ".")
colnames(dff1)=c('country','event_date','year','Battles','Explosions','Protests','Riots','Strategic_developments','Violence_against_civilians','inter1','Until','Since','kill','torture','polity','fair','corrupt', 'war','GDP','population','tech')
dff1$std.logGDP = scale(log(dff1$GDP))
dff1$std.logpop = scale(log(dff1$population))
dff1$std.logtech = scale(log(dff1$tech))
dff1$Until = as.integer(dff1$Until)
dff1$Since = as.integer(dff1$Since)
dff1$polity = dff1$polity+10
dff1$Since = ifelse(dff1$Since == '.', 1000000,dff1$Since)
dff1$Until[is.na(dff1$Until)] = -1000000
dff1$Since = as.integer(dff1$Since)
dff1 = dff1 %>% mutate(pre = ifelse((abs(Until) <= Since), 1,0))
dff1$post = ifelse(dff1$pre == 0, 1,0)

# Logistic regression models

# Pre-election violence

dff1$pre1 = ifelse((dff1$pre==1) & (-15 <= dff1$Until) & (dff1$Until <= 0), 1, 0)

preMod1 = glm(pre1 ~ Battles+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP +std.logpop+std.logtech, data = dff1,family = 'binomial')
preMod2=glm(pre1 ~ Explosions+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+std.logpop+std.logtech, data = dff1,family = 'binomial')
preMod3=glm(pre1 ~ Protests+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+std.logpop+std.logtech, data = dff1,family = 'binomial')
preMod4=glm(pre1 ~ Riots+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+std.logpop+std.logtech, data = dff1,family = 'binomial')
preMod5=glm(pre1 ~ Violence_against_civilians+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+std.logpop+std.logtech, data = dff1,family = 'binomial')

pl = c(Battles='Battles',Explosions='Explosions/Remote violence',Protests='Protests',Riots='Riots',
       Violence_against_civilians='Violence against civilians',kill='Freedom from political killings',
       torture='Freedom from torture',polity='Polity combined score',fair='Elections free and fair',
       corrupt='Political corruption index','factor(war)1' = 'War',std.logGDP='GDP (log)',std.logpop='Population (log)', std.logtech='Technical cooperation grants (log)','factor(Strategic_developments)1'='Strategic developments')
tab_model(preMod1,preMod2,preMod3,preMod4,preMod5,collapse.ci = TRUE,dv.labels = c('Model 1', 'Model 2','Model 3','Model 4','Model 5'), p.style = "stars",show.reflvl = FALSE,show.aic = TRUE,pred.labels = pl,order.terms = c(1,2,13:16,3:12),digits = 4, title='Logistic models (odds ratios) for types of pre-election violence (election month)',df.method = 'wald',file='pre_eleOR.html')
webshot("pre_eleOR.html", "pre_eleOR.png")

# Post-election violence

dff1$post1 = ifelse((dff1$post==1) & (dff1$Since <= 15) & (dff1$Since > 0), 1, 0)
postMod1 = glm(post1 ~ Battles+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP +std.logpop+std.logtech, data = dff1,family = 'binomial')
postMod2 = glm(post1 ~ Explosions+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+std.logpop+std.logtech, data = dff1,family = 'binomial')
postMod3 = glm(post1 ~ Protests+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+std.logpop+std.logtech, data = dff1,family = 'binomial')
postMod4 = glm(post1 ~ Riots+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+std.logpop+std.logtech, data = dff1,family = 'binomial')
postMod5 = glm(post1 ~ Violence_against_civilians+factor(Strategic_developments)+kill+torture+polity+fair+corrupt+factor(war)+std.logGDP+std.logpop+std.logtech, data = dff1,family = 'binomial')

tab_model(postMod1,postMod2,postMod3,postMod4,postMod5,collapse.ci = TRUE,dv.labels = c('Model 1', 'Model 2','Model 3','Model 4','Model 5'),p.style = "stars",show.reflvl = FALSE,show.aic = TRUE,pred.labels = pl,order.terms = c(1,2,13:16,3:12),digits = 4,title='Logistic models (odds ratios) for types of post-election violence (election month)',df.method = 'wald',file = "post_ele_OR.html")
webshot("post_ele_OR.html", "post_ele_OR.png")






