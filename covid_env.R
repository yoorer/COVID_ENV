##COVID Vs. Environmental factors
#Workspace path
code_root="/User/"
setwd(paste0(code_root, "covid_env"))



#Libraries
library(ggplot2)
library(scales)
library(plyr)


#Data input
data <- read.csv("covid.data.csv")
seoul <- subset(data, region == "seoul")
daegu <- subset(data, region == "daegu")


##Spearman Rank Test
seoul.spear.temp.avg <- cor.test(seoul$temp.avg, seoul$case, method="spearman")
seoul.spear.temp.low <- cor.test(seoul$temp.low, seoul$case, method="spearman")
seoul.spear.temp.high <- cor.test(seoul$temp.high, seoul$case, method="spearman")
seoul.spear.dtv <- cor.test(seoul$dtv, seoul$case, method="spearman")
seoul.spear.rainfall <- cor.test(seoul$rainfall, seoul$case, method="spearman")
seoul.spear.wind <- cor.test(seoul$wind, seoul$case, method="spearman")
seoul.spear.rel.hum <- cor.test(seoul$rel.hum, seoul$case, method="spearman")
seoul.spear.pressure <- cor.test(seoul$pressure, seoul$case, method="spearman")
seoul.spear.sunduration <- cor.test(seoul$sunduration, seoul$case, method="spearman")
seoul.spear.temp.ground <- cor.test(seoul$temp.ground, seoul$case, method="spearman")
seoul.spear.co <- cor.test(seoul$co, seoul$case, method="spearman")
seoul.spear.no2 <- cor.test(seoul$no2, seoul$case, method="spearman")
seoul.spear.o3 <- cor.test(seoul$o3, seoul$case, method="spearman")
seoul.spear.so3 <- cor.test(seoul$so3, seoul$case, method="spearman")
seoul.spear.pm10 <- cor.test(seoul$pm10, seoul$case, method="spearman")
seoul.spear.pm25 <- cor.test(seoul$pm25, seoul$case, method="spearman")

daegu.spear.temp.avg <- cor.test(daegu$temp.avg, daegu$case, method="spearman")
daegu.spear.temp.low <- cor.test(daegu$temp.low, daegu$case, method="spearman")
daegu.spear.temp.high <- cor.test(daegu$temp.high, daegu$case, method="spearman")
daegu.spear.dtv <- cor.test(daegu$dtv, daegu$case, method="spearman")
daegu.spear.rainfall <- cor.test(daegu$rainfall, daegu$case, method="spearman")
daegu.spear.wind <- cor.test(daegu$wind, daegu$case, method="spearman")
daegu.spear.rel.hum <- cor.test(daegu$rel.hum, daegu$case, method="spearman")
daegu.spear.pressure <- cor.test(daegu$pressure, daegu$case, method="spearman")
daegu.spear.sunduration <- cor.test(daegu$sunduration, daegu$case, method="spearman")
daegu.spear.temp.ground <- cor.test(daegu$temp.ground, daegu$case, method="spearman")
daegu.spear.co <- cor.test(daegu$co, daegu$case, method="spearman")
daegu.spear.no2 <- cor.test(daegu$no2, daegu$case, method="spearman")
daegu.spear.o3 <- cor.test(daegu$o3, daegu$case, method="spearman")
daegu.spear.so3 <- cor.test(daegu$so3, daegu$case, method="spearman")
daegu.spear.pm10 <- cor.test(daegu$pm10, daegu$case, method="spearman")
daegu.spear.pm25 <- cor.test(daegu$pm25, daegu$case, method="spearman")


##Kendall Rank Correlation
seoul.ken.temp.avg <- cor.test(seoul$temp.avg, seoul$case, method="kendall")
seoul.ken.temp.low <- cor.test(seoul$temp.low, seoul$case, method="kendall")
seoul.ken.temp.high <- cor.test(seoul$temp.high, seoul$case, method="kendall")
seoul.ken.dtv <- cor.test(seoul$dtv, seoul$case, method="kendall")
seoul.ken.rainfall <- cor.test(seoul$rainfall, seoul$case, method="kendall")
seoul.ken.wind <- cor.test(seoul$wind, seoul$case, method="kendall")
seoul.ken.rel.hum <- cor.test(seoul$rel.hum, seoul$case, method="kendall")
seoul.ken.pressure <- cor.test(seoul$pressure, seoul$case, method="kendall")
seoul.ken.sunduration <- cor.test(seoul$sunduration, seoul$case, method="kendall")
seoul.ken.temp.ground <- cor.test(seoul$temp.ground, seoul$case, method="kendall")
seoul.ken.co <- cor.test(seoul$co, seoul$case, method="kendall")
seoul.ken.no2 <- cor.test(seoul$no2, seoul$case, method="kendall")
seoul.ken.o3 <- cor.test(seoul$o3, seoul$case, method="kendall")
seoul.ken.so3 <- cor.test(seoul$so3, seoul$case, method="kendall")
seoul.ken.pm10 <- cor.test(seoul$pm10, seoul$case, method="kendall")
seoul.ken.pm25 <- cor.test(seoul$pm25, seoul$case, method="kendall")

daegu.ken.temp.avg <- cor.test(daegu$temp.avg, daegu$case, method="kendall")
daegu.ken.temp.low <- cor.test(daegu$temp.low, daegu$case, method="kendall")
daegu.ken.temp.high <- cor.test(daegu$temp.high, daegu$case, method="kendall")
daegu.ken.dtv <- cor.test(daegu$dtv, daegu$case, method="kendall")
daegu.ken.rainfall <- cor.test(daegu$rainfall, daegu$case, method="kendall")
daegu.ken.wind <- cor.test(daegu$wind, daegu$case, method="kendall")
daegu.ken.rel.hum <- cor.test(daegu$rel.hum, daegu$case, method="kendall")
daegu.ken.pressure <- cor.test(daegu$pressure, daegu$case, method="kendall")
daegu.ken.sunduration <- cor.test(daegu$sunduration, daegu$case, method="kendall")
daegu.ken.temp.ground <- cor.test(daegu$temp.ground, daegu$case, method="kendall")
daegu.ken.co <- cor.test(daegu$co, daegu$case, method="kendall")
daegu.ken.no2 <- cor.test(daegu$no2, daegu$case, method="kendall")
daegu.ken.o3 <- cor.test(daegu$o3, daegu$case, method="kendall")
daegu.ken.so3 <- cor.test(daegu$so3, daegu$case, method="kendall")
daegu.ken.pm10 <- cor.test(daegu$pm10, daegu$case, method="kendall")
daegu.ken.pm25 <- cor.test(daegu$pm25, daegu$case, method="kendall")


##Table for Spearman Rank Test
spearman.seoul <- data.frame(
    parameters = c(
        "temp.avg", "temp.low", "temp.high", "dtv", "rainfall", "wind", "rel.hum", "pressure", "sunduration", "temp.ground", "co", "no2", "o3", "so3", "pm10", "pm25"
    ),
    rho = c(
        seoul.spear.temp.avg$estimate,
        seoul.spear.temp.low$estimate,
        seoul.spear.temp.high$estimate,
        seoul.spear.dtv$estimate,
        seoul.spear.rainfall$estimate,
        seoul.spear.wind$estimate,
        seoul.spear.rel.hum$estimate,
        seoul.spear.pressure$estimate,
        seoul.spear.sunduration$estimate,
        seoul.spear.temp.ground$estimate,
        seoul.spear.co$estimate,
        seoul.spear.no2$estimate,
        seoul.spear.o3$estimate,
        seoul.spear.so3$estimate,
        seoul.spear.pm10$estimate,
        seoul.spear.pm25$estimate    
    ),
    p.value = c(
        seoul.spear.temp.avg$p.value,
        seoul.spear.temp.low$p.value,
        seoul.spear.temp.high$p.value,
        seoul.spear.dtv$p.value,
        seoul.spear.rainfall$p.value,
        seoul.spear.wind$p.value,
        seoul.spear.rel.hum$p.value,
        seoul.spear.pressure$p.value,
        seoul.spear.sunduration$p.value,
        seoul.spear.temp.ground$p.value,
        seoul.spear.co$p.value,
        seoul.spear.no2$p.value,
        seoul.spear.o3$p.value,
        seoul.spear.so3$p.value,
        seoul.spear.pm10$p.value,
        seoul.spear.pm25$p.value    
    )
)

spearman.daegu <- data.frame(
    parameters = c(
        "temp.avg", "temp.low", "temp.high", "dtv", "rainfall", "wind", "rel.hum", "pressure", "sunduration", "temp.ground", "co", "no2", "o3", "so3", "pm10", "pm25"
    ),
    rho = c(
        daegu.spear.temp.avg$estimate,
        daegu.spear.temp.low$estimate,
        daegu.spear.temp.high$estimate,
        daegu.spear.dtv$estimate,
        daegu.spear.rainfall$estimate,
        daegu.spear.wind$estimate,
        daegu.spear.rel.hum$estimate,
        daegu.spear.pressure$estimate,
        daegu.spear.sunduration$estimate,
        daegu.spear.temp.ground$estimate,
        daegu.spear.co$estimate,
        daegu.spear.no2$estimate,
        daegu.spear.o3$estimate,
        daegu.spear.so3$estimate,
        daegu.spear.pm10$estimate,
        daegu.spear.pm25$estimate    
    ),
    p.value = c(
        daegu.spear.temp.avg$p.value,
        daegu.spear.temp.low$p.value,
        daegu.spear.temp.high$p.value,
        daegu.spear.dtv$p.value,
        daegu.spear.rainfall$p.value,
        daegu.spear.wind$p.value,
        daegu.spear.rel.hum$p.value,
        daegu.spear.pressure$p.value,
        daegu.spear.sunduration$p.value,
        daegu.spear.temp.ground$p.value,
        daegu.spear.co$p.value,
        daegu.spear.no2$p.value,
        daegu.spear.o3$p.value,
        daegu.spear.so3$p.value,
        daegu.spear.pm10$p.value,
        daegu.spear.pm25$p.value    
    )
)

##Table for Kendall Rank Correlation
kendall.seoul <- data.frame(
    parameters = c(
        "temp.avg", "temp.low", "temp.high", "dtv", "rainfall", "wind", "rel.hum", "pressure", "sunduration", "temp.ground", "co", "no2", "o3", "so3", "pm10", "pm25"
    ),
    tau = c(
        seoul.ken.temp.avg$estimate,
        seoul.ken.temp.low$estimate,
        seoul.ken.temp.high$estimate,
        seoul.ken.dtv$estimate,
        seoul.ken.rainfall$estimate,
        seoul.ken.wind$estimate,
        seoul.ken.rel.hum$estimate,
        seoul.ken.pressure$estimate,
        seoul.ken.sunduration$estimate,
        seoul.ken.temp.ground$estimate,
        seoul.ken.co$estimate,
        seoul.ken.no2$estimate,
        seoul.ken.o3$estimate,
        seoul.ken.so3$estimate,
        seoul.ken.pm10$estimate,
        seoul.ken.pm25$estimate    
    ),
    p.value = c(
        seoul.ken.temp.avg$p.value,
        seoul.ken.temp.low$p.value,
        seoul.ken.temp.high$p.value,
        seoul.ken.dtv$p.value,
        seoul.ken.rainfall$p.value,
        seoul.ken.wind$p.value,
        seoul.ken.rel.hum$p.value,
        seoul.ken.pressure$p.value,
        seoul.ken.sunduration$p.value,
        seoul.ken.temp.ground$p.value,
        seoul.ken.co$p.value,
        seoul.ken.no2$p.value,
        seoul.ken.o3$p.value,
        seoul.ken.so3$p.value,
        seoul.ken.pm10$p.value,
        seoul.ken.pm25$p.value    
    )
)

kendall.daegu <- data.frame(
    parameters = c(
        "temp.avg", "temp.low", "temp.high", "dtv", "rainfall", "wind", "rel.hum", "pressure", "sunduration", "temp.ground", "co", "no2", "o3", "so3", "pm10", "pm25"
    ),
    tau = c(
        daegu.ken.temp.avg$estimate,
        daegu.ken.temp.low$estimate,
        daegu.ken.temp.high$estimate,
        daegu.ken.dtv$estimate,
        daegu.ken.rainfall$estimate,
        daegu.ken.wind$estimate,
        daegu.ken.rel.hum$estimate,
        daegu.ken.pressure$estimate,
        daegu.ken.sunduration$estimate,
        daegu.ken.temp.ground$estimate,
        daegu.ken.co$estimate,
        daegu.ken.no2$estimate,
        daegu.ken.o3$estimate,
        daegu.ken.so3$estimate,
        daegu.ken.pm10$estimate,
        daegu.ken.pm25$estimate    
    ),
    p.value = c(
        daegu.ken.temp.avg$p.value,
        daegu.ken.temp.low$p.value,
        daegu.ken.temp.high$p.value,
        daegu.ken.dtv$p.value,
        daegu.ken.rainfall$p.value,
        daegu.ken.wind$p.value,
        daegu.ken.rel.hum$p.value,
        daegu.ken.pressure$p.value,
        daegu.ken.sunduration$p.value,
        daegu.ken.temp.ground$p.value,
        daegu.ken.co$p.value,
        daegu.ken.no2$p.value,
        daegu.ken.o3$p.value,
        daegu.ken.so3$p.value,
        daegu.ken.pm10$p.value,
        daegu.ken.pm25$p.value    
    )
)



##Table export
write.csv(spearman.seoul, file = "spearman.seoul.csv", append = TRUE)
write.csv(spearman.daegu, file = "spearman.daegu.csv", append = TRUE)
write.csv(kendall.seoul, file = "kendall.seoul.csv", append = TRUE)
write.csv(kendall.daegu, file = "kendall.daegu.csv", append = TRUE)





##Histogram and cumulutive graph of new COVID-19 confirmed cases
seoul.graph <- ggplot() +
    geom_bar(data = seoul, aes(x = dday, y = case), stat = "identity", fill = "grey66", width = 1) +
    geom_line(data = seoul, aes(x = dday, y = total.case/30)) +
    scale_x_continuous(
        expand = c(0,0),
        limits = c(32, 213),
        breaks = c(32, 61, 92, 122, 153, 183),
        labels = NULL
    ) +
    scale_y_continuous(
        expand = c(0,0),
        limits = c(0,120),
        breaks = 20 * c(0:6),
        name = "New confirmed cases",
        sec.axis = sec_axis(~.*30, name = "Total confirmed cases", breaks = 500 * c(0:7))
    )

daegu.graph <- ggplot() +
    geom_bar(data = daegu, aes(x = dday, y = case), stat = "identity", fill = "grey66",  width = 1) +
    geom_line(data = daegu, aes(x = dday, y = total.case/10)) +
    scale_x_continuous(
        expand = c(0,0),
        limits = c(32, 213),
        breaks = c(32, 61, 92, 122, 153, 183),
        labels = NULL
    ) +
    scale_y_continuous(
        expand = c(0,0),
        limits = c(0,900),
        breaks = 100 * c(0:9),
        name = "New confirmed cases",
        sec.axis = sec_axis(~.*10, name = "Total confirmed cases", breaks = 1000 * c(0:9))
    )



##Graph exporting
pdf("seoul.pdf", width = 4, height = 3)
seoul.graph
dev.off()   

pdf("daegu.pdf", width = 4, height = 3)
daegu.graph
dev.off()