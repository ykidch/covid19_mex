list.of.packages <- c("rJava", "tabulizer","stringr","tibble","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library(rJava)
library(tabulizer)
library(stringr)
library(tibble)
library(tidyverse)


get_timeseries <- function(modalidad){

	if (modalidad != "positivos" && modalidad != "sospechosos") {
		stop()
	} else {
		
	states <- c("Aguascalientes","Baja California","Baja California Sur","Campeche","Chiapas","Chihuahua","Coahuila","Colima","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas")
#QUERETARO sin acento

	new_df <- data.frame(states,row.names=NULL)
	cols_new_df <- c("Estado")

	for (file in list.files("data",pattern=paste(modalidad,".*.csv",sep=""),recursive=TRUE,full.names=TRUE)) {
	
		df <- read.csv(file);
		
		new_df <- cbind(new_df,df[,2])

		filedate <- colnames(df)[2];
		filedate <- sub("X([0-9]{4}.[0-9]{2}.[0-9]{2})","\\1",filedate)
		cols_new_df <- c(cols_new_df,filedate)


	}

	colnames(new_df) <- cols_new_df

	write.csv(new_df, file=paste("timeseries/",modalidad,".csv",sep=""),row.names=FALSE,quote=FALSE)

	return(new_df)

	}
}

get_timeseries("positivos")
get_timeseries("sospechosos")

