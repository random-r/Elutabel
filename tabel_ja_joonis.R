# Eesti 2018 surmad ja rahvastik soo lõikes, 1-aastased vanused 0-100+
# 100 on tegelikult 100+

dat1 <- read.csv("lt.csv", header=TRUE)
levels(dat1$sugu) <- c("Mees","Naine")

# meeste elutabel
mehed <- lifetable(x="vanus", Nx="N", Dx="D", data=subset(dat1, sugu=="Mees"))
mehed$sugu <- "M"

# naiste elutabel
naised <- lifetable(x="vanus", Nx="N", Dx="D", data=subset(dat1, sugu=="Naine"))
naised$sugu <- "N"

# mehed ja naised
df <- rbind(mehed, naised)

# suremustõenäosus aasta jooksul, vanused 80-99
ggplot(subset(df,Age%in% 80:99), aes(x=Age, y=qx, color=sugu)) + 
  geom_line() + theme_bw() +
  scale_y_log10()
