library(tidyverse)
library(cowplot)


ghg_data<-read_csv("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv")

can_ghg_data<- ghg_data %>%
  filter(Index==0) %>%
  filter(Region=="Canada") %>%
  select(Year,CO2eq) %>%
  add_row(Year=2030,CO2eq=NULL)

can_ghg_2019<- can_ghg_data %>%
  filter(Year==2019) %>%
  mutate(Old=CO2eq,
         New=CO2eq) %>%
  add_row(Year=2030,CO2eq=NULL,Old=517,New=443)

can_data<-left_join(can_ghg_data,can_ghg_2019) %>%
  pivot_longer(CO2eq:New, names_to="type",values_to="CO2",values_drop_na=TRUE ) %>%
  mutate(target=type!="CO2eq")

can_data %>%
  mutate(type=fct_relevel(type, "Old", "New", "CO2eq"))%>%
  arrange(type)%>%
    ggplot(aes(Year,CO2))+
  geom_line(aes(linetype=target,colour=type),size=1)+
  geom_point(aes(colour=type),size=2.1)+
  theme_cowplot(12)+
  theme(legend.position = "none")+
  annotate("text", x=2029, y = 650,
           label = "Old\nTarget",color="#F8766D")+
  annotate("text", x=2027, y = 400,
           label = "New\nTarget",color="#00BA38")+ 
  ylim(0,800)+
  labs(y="Mt CO2eq",
       title="Canadian Greenhouse Gas Emissions\n 1990-2019",
       caption="Data: Environment and Climate Change Canada, 2021\nNational Inventory Report\nChart: https://github.com/joelwwood/ChartToWatch_Dec2021")
  
