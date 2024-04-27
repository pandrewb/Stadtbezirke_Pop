install.packages("tidyverse")
library("tidyverse")
library(magrittr)
library(readxl)
library(easystats)

Frankfurt <- read.xlsx("Data/Bevölkerung in Haushalten (Stadtbezirke) 2022.xlsx")

# Plausibilitätsprüfung Bevölkerung Gesamt
# Frankfurt %>% select(Personen) %>% filter(row_number() %in% 1:119)

Frankfurt %>% select(Personen) %>% slice(1:119) %>% sum()

# Gesamtzeile streichen

Bevölkerung <- Frankfurt %>% slice(1:120)
Entgelte <- read_excel("Data/Bruttoarbeitsentgelte.xlsx", sheet = 7, skip = 9)
Entgelte <- Entgelte %>% rename(Stadtbezirk = ...1)
Entgelte <- Entgelte %>% slice(1:46)
Entgelte <- Entgelte %>% mutate_at(vars(2:13), ~as.numeric(gsub("\\.", "", .)))
Entgelte[46, 1] <- "Gesamt"

Bevölkerung_Stadtbezirke <- Bevölkerung %>% group_by(Stadtteil) %>% 
  summarise(Total_Personen = sum(Personen),
            Total_Haushalte = sum(Haushalte),
            Durchschnitt_Personen_pro_Haushalt = mean(`Personen je.Haushalt`),
)

Bevölkerung_Stadtbezirke <- Bevölkerung_Stadtbezirke %>% rename(Stadtbezirk = Stadtteil)

all(Bevölkerung_Stadtbezirke[,1] == Entgelte[,1])
Entgelte[14, 1] <- "14 - Sachsenhausen-Süd (inkl. Flughafen)"
all(Bevölkerung_Stadtbezirke[,1] == Entgelte[,1])

merge <- merge(Bevölkerung_Stadtbezirke, Entgelte, by = "Stadtbezirk")

merge %>% group_by(Stadtbezirk) %>% data_tabulate(Total_Personen)

merge <- merge %>% slice(1:45)

plot <- ggplot(merge, aes (y = Durchschnitt_Personen_pro_Haushalt, x = `Entgelt Median in €`)) + geom_point() + scale_x_log10()
plot <- plot + geom_text(aes(label = Stadtbezirk), hjust = -0.2, vjust = -0.2, size = 1)
print(plot)
ggsave("Output/Plots/scatterplot.png", plot, width = 6, height = 4, units = "in", dpi = 300)