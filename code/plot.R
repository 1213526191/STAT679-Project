library(mlmRev)
library(tidyverse)
library(lme4)
# read data ---------------------------------------------------------------

data(star)
star2 <- na.omit(star)
star_sub <- star2 %>%
  group_by(id, cltype, sch) %>%
  summarise(count = n()) %>%
  filter(count == 4)
id_index <- star_sub$id
dat <- star %>%
  filter(id %in% id_index) %>%
  group_by(id) %>%
  mutate(mean_math = mean(math), mean_read = mean(read)) %>%
  group_by(id) %>%
  top_n(1, yrs) %>%
  select(-gr, -read, -math, -yrs)

# plot --------------------------------------------------------------------

# school size
df = dat %>%
  group_by(sch) %>%
  summarize(n=n())
p1 = ggplot(df,aes(x=n)) +
  geom_histogram(center=0,binwidth=1,color="black",fill="lightblue") +
  xlab('School size') +
  ylab('') +
  theme_bw()

# plot to indicate random slope
p2 = ggplot(dat, aes(x=cltype,y=mean_math,group=sch)) +
  geom_point(position=position_jitter(width=0.1,height=0), color = "lightcoral") +
  geom_smooth(se=FALSE,method="lm",color="mediumaquamarine",size=0.5,linetype=1) +
  xlab("class type") + ylab("mean of math score") 


# mean_read vs mean_score
p3 = ggplot(dat, aes(x = mean_math, y = mean_read)) + geom_point(position=position_jitter(width=0.1,height=0), color = "lightcoral") +
  xlab("mean of math score") + ylab("mean of read score") 

# mean_math
p4 = ggplot(dat, mapping = aes(x=mean_math))+ 
  geom_histogram(aes(y = ..density..), binwidth = 5, color="black", fill="white") +
  geom_density() + labs(title="Density plot of mean of math score", x = "mean of math score")+
  theme(plot.title = element_text(hjust = 0.5))

# log of mean_math
p5 = ggplot(dat, mapping = aes(x=log(mean_math)))+ 
  geom_histogram(aes(y = ..density..), binwidth = 0.01, color="black", fill="white") +
  geom_density() + labs(title="Density plot of mean of math score", x = "log function of mean of math score")+
  theme(plot.title = element_text(hjust = 0.5))



# cltype vs mean_math
p6 = ggplot(dat, aes(x = cltype, y = mean_math)) + geom_boxplot() +
  ggtitle("cltype vs math") +
  xlab("class type") + ylab("mean of math score")

ggplot(dat, aes(x = cltype, y = mean_math)) + geom_point(position=position_jitter(width=0.1,height=0)) +
  ggtitle("The relation between class type and mean of math score") +
  xlab("class type") + ylab("mean of math score")
# ses vs mean_math
p7 = ggplot(dat, aes(x = ses, y = mean_math)) + geom_boxplot() +
  ggtitle("ses vs math") +
  xlab("socioeconomic status") + ylab("mean of math score")
ggplot(dat, aes(x = ses, y = mean_math)) + geom_point(position=position_jitter(width=0.1,height=0)) +
  ggtitle("The relation between socioeconomic status and mean of math score") +
  xlab("socioeconomic status") + ylab("mean of math score")
# eth vs mean_math
p8 = ggplot(dat, aes(x = eth, y = mean_math)) + geom_point(position=position_jitter(width=0.1,height=0)) +
  ggtitle("eth vs math") +
  xlab("student's ethnicity") + ylab("mean of math score")
# sch vs mean_math
p9 = ggplot(dat, aes(x = mean(as.numeric(sch)), y = mean_math)) + geom_point(position=position_jitter(width=0.1,height=0)) +
  ggtitle("sch vs math") +
  xlab("mean of school") + ylab("mean of math score")
# birthq vs mean_math
p10 = ggplot(dat, aes(x = birthq, y = mean_math)) + geom_boxplot() +
  ggtitle("birthq vs math") +
  xlab("student's birth quarter") + ylab("mean of math score")
ggplot(dat, aes(x = birthq, y = mean_math)) + geom_point(position=position_jitter(width=0.1,height=0)) +
  ggtitle("The relation between student's birth quarter and mean of math score") +
  xlab("student's birth quarter") + ylab("mean of math score")
# birthy vs mean_math
ggplot(dat, aes(x = birthy, y = mean_math)) + geom_boxplot() +
  ggtitle("The relation between student's birth year and mean of math score") +
  xlab("student's birth year") + ylab("mean of math score")
p11 = ggplot(dat, aes(x = birthy, y = mean_math)) + geom_point(position=position_jitter(width=0.1,height=0)) +
  ggtitle("birthy vs math") +
  xlab("student's birth year") + ylab("mean of math score")
# schtype vs mean_math
p12 = ggplot(dat, aes(x = schtype, y = mean_math)) + geom_boxplot() +
  ggtitle("schtype vs math") +
  xlab("school type") + ylab("mean of math score")
ggplot(dat, aes(x = schtype, y = mean_math)) + geom_point(position=position_jitter(width=0.1,height=0)) +
  ggtitle("The relation between school type and mean of math score") +
  xlab("school type") + ylab("mean of math score")
# sex vs mean_math
p13 = ggplot(dat, aes(x = sx, y = mean_math)) + geom_boxplot() +
  ggtitle("sx vs math") +
  xlab("sex") + ylab("mean of math score")
ggplot(dat, aes(x = sx, y = mean_math)) + geom_point(position=position_jitter(width=0.1,height=0)) +
  ggtitle("The relation between sex and mean of math score") +
  xlab("sex") + ylab("mean of math score")



library(gridExtra)
p24 <- grid.arrange(p6, p7, p8, p9, p10, p11, p12, p13, ncol=2)

ggsave("p24.png", p24, width = 14, height = 14)


grid.arrange(p6, p7, p8, p9, p10, p11, p12, p13, ncol=3)

ggsave("p33.png",width = 7, height = 4)

ggsave("../plot/school.png", p1, width = 7, height = 4)
ggsave("../plot/mathread.png", p3, width = 7, height = 4)

p45 <- grid.arrange(p4, p5, ncol=2)
ggsave("../plot/math.png", p45, width = 7, height = 4)
