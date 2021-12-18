library(tidyverse)
library(jsonlite)
library(lubridate)
library(firatheme)
library(forcats)
library(viridis)
library(ggthemes)
library(scales)
library(grid)
library(gridExtra)
options(scipen = 999)



spotify <- fromJSON("StreamingHistory0.json")

spotify$endTime <- as.POSIXct(spotify$endTime)

str(spotify)


### Top 20 bar chart

by_artist <-  spotify %>%
        group_by(artistName) %>%
        summarise(total_listened = sum(msPlayed)) %>%
        arrange(desc(total_listened)) %>%
        mutate(minutes = (total_listened/60000))

toop20 <- by_artist %>%
        mutate(isTheWeeknd = (artistName == "The Weeknd")) %>%
        head(20) %>%
        ggplot(aes( x = minutes, y = fct_reorder(artistName,minutes),fill = isTheWeeknd))+
        geom_bar(stat = "identity")+
        theme_fira()+
        scale_x_continuous(expand = c(0,0), breaks = seq(0,700,50))+
        scale_fill_viridis(discrete = TRUE, guide = "none")+
        labs(title = "Top #20 de artistas mas escuchados en mi 2021",
             x = "Minutos",
             y = "Artista")


dias <- c("domingo", "sábado", "viernes", "jueves", "miércoles", "martes", "lunes")

# Heatmap

heatmap <- spotify %>%
        mutate(semana = week(endTime)) %>%
        mutate(dia = weekdays(endTime)) %>%
        group_by(semana,dia) %>%
        mutate(minutes = msPlayed/60000) %>%
        summarise(total = sum(minutes)) %>%
        ggplot(aes( x = factor(semana), y = factor(dia, levels = dias), fill = total))+
        geom_tile(color = "black")+
        scale_fill_viridis(option = "plasma")+
        theme_classic()+
        labs(fill = "Minutos",
             x = "Semana",
             y = "Día de la Semana",
             title = "Tiempo de escucha por cada día de la semana")+
             coord_equal()


# Months

by_month <- spotify %>%
       mutate(mes = month(endTime)) %>%
        mutate(meses = month.abb[mes]) %>%
        mutate(minutos = msPlayed/60000) %>%
        group_by(meses,artistName) %>%
        summarise(total_played = sum(minutos))


mes_bar <- by_month %>%
        group_by(meses, artistName) %>%
        filter(artistName != "Kaol_") %>%
        filter(total_played > 45) %>%
        ggplot(aes( x = factor(meses, levels = month.abb), y =total_played, fill = artistName))+
        geom_bar(stat = "identity", color = "black")+
        scale_y_continuous(labels = comma, breaks = seq(0,600,50), expand = c(0,0))+
        #scale_fill_viridis(discrete = TRUE, option = "plasma")+
        geom_text(aes( label = artistName), position = position_stack(vjust = 0.5), 
                  size =3.5, color = "black")+
        theme_fira()+
        labs(title = "Artistas mas escuchados por mes",
             x = "Mes",
             y= "Minutos",
             fill = "Artista")


# Polar
        
 
 fecha <- function(x) {
        format(as.POSIXct(as.character(x), format = "%H"), format = "%I %p")
}

 
by_hour <- spotify %>%
         mutate(horas = hour(endTime)) %>%
        mutate(hours =fecha(horas))

 
correct_order<- c("12 a. m.","01 a. m." , "02 a. m.", "03 a. m.", "04 a. m.", "05 a. m.", "06 a. m.",
                  "07 a. m.", "08 a. m.", "09 a. m.", "10 a. m.", "11 a. m.", "12 p. m.", "01 p. m.", "02 p. m.", "03 p. m.",
                  "04 p. m.", "05 p. m.", "06 p. m.", "07 p. m.", "08 p. m.", "09 p. m.", "10 p. m.", "11 p. m.")


polar <- by_hour %>%
        group_by(hours) %>%
        summarise(total = sum(msPlayed)) %>%
        mutate(minutos = total/60000) %>% 
        ggplot(aes( x = factor(hours, levels = correct_order), y = minutos, fill = minutos))+
        geom_col() +
        coord_polar()+
        scale_fill_viridis(option = "plasma", guide = "none")+
        theme_fira()+
        labs(x = "Hora",
             y = "Minutos",
             title = "Tiempo de escucha por hora del día")+
        scale_y_continuous(breaks = seq(0,1500,400), labels = comma)

        


wrapped <- grid.arrange(mes_bar,polar,heatmap,toop20,top = textGrob('Spotify Wrapped: R Edition',
                                                         gp=gpar(fontsize=20,font=3), hjust = -0.2)) 





ggsave("wrappped.jpg",plot = wrapped, width = 60,height = 40,units = "cm", dpi = 300)

