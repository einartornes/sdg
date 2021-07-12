library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(forcats)

df <- readxl::read_excel("output/sdg_dataset.xlsx")

# The data are now wide with sdgs as columns. In the future, try doing such an operation with long data.
# since I am writing a function for a column name that's a variable, I have to write it in rlang syntax

cols <- c("sdg_1", "sdg_2", "sdg_3", "sdg_4", "sdg_5", "sdg_6", "sdg_7", "sdg_8", "sdg_9", 
          "sdg_10", "sdg_11", "sdg_12", "sdg_13", "sdg_14", "sdg_15", "sdg_16", "sdg_17", "sdg_0")


table <- map_df(syms(cols), ~df %>% # syms er nødvendig fordi kolonnenavnene er variabler
                filter(!!.x == TRUE) %>% # Merk rlang-syntaks
                filter(sdg_1 == TRUE) %>% # Denne avgrenser til kun det som også er markert med SDG 1.
                filter(type_of_flow == "ODA") %>%
                filter(type_of_agreement != "Rammeavtale") %>%
                summarise(nok_mill = sum(disbursed_mill_nok))
    )

table <- table %>%
  add_column(sdg_goal = tibble(sdg_goal = cols)$sdg_goal, .before = TRUE)


#writexl::write_xlsx(table, "sdg2.xlsx")


ggplot(table, aes(x = fct_reorder(sdg_goal, nok_mill), y = nok_mill)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label=round(nok_mill, 0)), hjust = 1, color = "white") +
  labs(title = "sdg_1 i kombinasjon med andre SDGer",
       subtitle = "Skiller ikke mellom viktigste og relevant SDG. Beløp dobbeltelles pga overlapp.")

ggsave("output/sdg_1.png")
