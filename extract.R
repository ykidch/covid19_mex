list.of.packages <- c("rJava", "tabulizer","stringr","tibble","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library(rJava)
library(tabulizer)
library(stringr)
library(tibble)
library(tidyverse)

pop_column <- function(mat,vect) {
	res <- matrix(nrow=dim(mat)[1],ncol=0);
	for (i in 1:dim(mat)[2]) {
		if (!(i %in% vect)) {
			res <- cbind(res,mat[,i]);
		}
	}
	return(res)
}

get_data <- function(pdf) {

	filename <- pdf
	print(filename);

	mexico_casos <- extract_tables(file = filename, method = "stream",output = "matrix")

	tablenames <- c("n_caso","Estado","Sexo","Edad","Inicio_sintomas","Status_prueba","Procedencia","Llegada_Mexico")

	filter_page <- function(page) {

	#firstpage <- mexico_casos[[1]]
	firstpage <- page

	firstrow <- min(which(grepl("^[0-9]",firstpage[,1],perl=TRUE))) #no. 1ra fila
	firstfiltered <- firstpage[firstrow:dim(firstpage)[1],]

	pop_vec <- c();

	for (i in 1:dim(firstfiltered)[2]) {
		if (all(firstfiltered[,i] == "")) {
			pop_vec <- c(pop_vec,i);
		}
	}

	firstfiltered <- pop_column(firstfiltered,pop_vec);
	
	print("filter 1");
	if (any(grepl("\\s",firstfiltered[,1],perl=TRUE))){
		firstfiltered_1split <- cbind(sub("^([0-9]+)\\s(.*)$","\\1",firstfiltered[,1]),sub("^([0-9]+)\\s(.*)$","\\2",firstfiltered[,1]));
		if (dim(firstfiltered)[2] >= 2){
			firstfiltered <- cbind(firstfiltered_1split,firstfiltered[,2:dim(firstfiltered)[2]])} 
		else {
			firstfiltered <- cbind(firstfiltered_1split)
		}

	}


	print("filter 2");
	if (any(grepl("(M|F)(\\s|$)",firstfiltered[,2],perl=TRUE))){
		firstfiltered_2split <- cbind(sub("^(.*)\\s((M|F)(\\s.*|$))","\\1",firstfiltered[,2]),sub("^(.*)\\s((M|F)(\\s.*|$))","\\2",firstfiltered[,2]));
	
		if (dim(firstfiltered)[2] >= 3){
			firstfiltered <- cbind(firstfiltered[,1],firstfiltered_2split,firstfiltered[,3:dim(firstfiltered)[2]])}
		else {
			firstfiltered <- cbind(firstfiltered[,1],firstfiltered_2split)
		}
	}


	print("filter 3");
	if (!all(grepl("^(M|F)$",firstfiltered[,3],perl=TRUE))){
		firstfiltered_3split <- cbind(sub("^(M|F)\\s(.*)","\\1",firstfiltered[,3]),sub("^(M|F)\\s(.*)","\\2",firstfiltered[,3]));

		if (dim(firstfiltered)[2] >= 4){
			firstfiltered <- cbind(firstfiltered[,1:2],firstfiltered_3split,firstfiltered[,4:dim(firstfiltered)[2]])}
		else {
			firstfiltered <- cbind(firstfiltered[,1:2],firstfiltered_3split)
		}

	}

	print("filter 4");
	if (any(grepl("\\s",firstfiltered[,4],perl=TRUE))){
		firstfiltered_4split <- cbind(sub("^([0-9]+)\\s(.*)$","\\1",firstfiltered[,4]),sub("^([0-9]+)\\s(.*)$","\\2",firstfiltered[,4]));

		if (dim(firstfiltered)[2] >= 5){
			firstfiltered <- cbind(firstfiltered[,1:3],firstfiltered_4split,firstfiltered[,5:dim(firstfiltered)[2]])}
		else {
			firstfiltered <- cbind(firstfiltered[,1:3],firstfiltered_4split)
		}

	}

	print("filter 5");
	if (any(grepl("\\s",firstfiltered[,5],perl=TRUE))){
		firstfiltered_5split <- cbind(sub("^([0-9]{2}/[0-9]{2}/[0-9]{4})\\s(.*)$","\\1",firstfiltered[,5]),sub("^([0-9]{2}/[0-9]{2}/[0-9]{4})\\s(.*)$","\\2",firstfiltered[,5]));
		if (dim(firstfiltered)[2] >= 6){
			firstfiltered <- cbind(firstfiltered[,1:4],firstfiltered_5split,firstfiltered[,6:dim(firstfiltered)[2]])}
		else {
			firstfiltered <- cbind(firstfiltered[,1:4],firstfiltered_5split)
		}


	}

	print("filter 6");
	if (any(grepl("\\s",firstfiltered[,6],perl=TRUE))){
		firstfiltered_6split <- cbind(sub("^([a-z|A-Z]{10})\\s(.*)$","\\1",firstfiltered[,6]),sub("^([a-z|A-Z]{10})\\s(.*)$","\\2",firstfiltered[,6]));
		if (dim(firstfiltered)[2] >= 7){
			firstfiltered <- cbind(firstfiltered[,1:5],firstfiltered_6split,firstfiltered[,7:dim(firstfiltered)[2]])}
		else {
			firstfiltered <- cbind(firstfiltered[,1:5],firstfiltered_6split)
		}

	}

	print("filter 7");
	if (any(grepl("\\s([0-9]{2}/[0-9]{2}/[0-9]{4}|NA)",firstfiltered[,7],perl=TRUE))){
		firstfiltered_7split <- cbind(sub("^(.*)\\s([0-9]{2}/[0-9]{2}/[0-9]{4}|NA)$","\\1",firstfiltered[,7]),sub("^(.*)\\s([0-9]{2}/[0-9]{2}/[0-9]{4}|NA)$","\\2",firstfiltered[,7]));
		firstfiltered <- cbind(firstfiltered[,1:6],firstfiltered_7split);
	}


#	rexp <- "^(\\w+)\\s?(.*)$"
#	if (unique(sub(rexp,"\\2",firstfiltered[,6]) != "")) {
#	firstfiltered_6split <- cbind(sub(rexp,"\\1",firstfiltered[,6]),sub(rexp,"\\2",firstfiltered[,6]))
#	} else {
#	firstfiltered_6split <- sub(rexp,"\\1",firstfiltered[,6])
#	}


	#firstfiltered_6split <- str_split(firstfiltered[,6],pattern="\\s",n=2,simplify=TRUE)

#	confirmed_table <- cbind(firstfiltered[,1:5],firstfiltered_6split,firstfiltered[,7:dim(firstfiltered)[2]])

	confirmed_table <- firstfiltered;
	print(confirmed_table);
	return(confirmed_table);
	}

	final_table <- filter_page(mexico_casos[[1]]);


	for (i in 2:length(mexico_casos)) {
	#print(mexico_casos[[i]]);
	#confirmed_table <- rbind(confirmed_table,mexico_casos[[i]])
	final_table <- rbind(final_table,filter_page(mexico_casos[[i]]));
	}

	#colnames(confirmed_table) <- tablenames
	colnames(final_table) <- tablenames;

	#return(as.data.frame(confirmed_table))
	return(as.data.frame(final_table))
}



count_data <- function(df,date) {

	states <- c("AGUASCALIENTES","BAJA CALIFORNIA","BAJA CALIFORNIA SUR","CAMPECHE","CHIAPAS","CHIHUAHUA","COAHUILA","COLIMA","CIUDAD DE MÉXICO","DURANGO","GUANAJUATO","GUERRERO","HIDALGO","JALISCO","MÉXICO","MICHOACÁN","MORELOS","NAYARIT","NUEVO LEÓN","OAXACA","PUEBLA","QUERETARO","QUINTANA ROO","SAN LUIS POTOSÍ","SINALOA","SONORA","TABASCO","TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATÁN","ZACATECAS")
#QUERETARO sin acento
#	states <- c("AGUASCALIENTES","BAJA CALIFORNIA","BAJA CALIFORNIA SUR","CAMPECHE","CHIAPAS","CHIHUAHUA","COAHUILA","COLIMA","DISTRITO FEDERAL","DURANGO","GUANAJUATO","GUERRERO","HIDALGO","JALISCO","MEXICO","MICHOACAN","MORELOS","NAYARIT","NUEVO LEON","OAXACA","PUEBLA","QUERETARO","QUINTANA ROO","SAN LUIS POTOSI","SINALOA","SONORA","TABASCO","TAMAULIPAS","TLAXCALA","VERACRUZ","YUCATAN","ZACATECAS")

	new_df <- data.frame(matrix(ncol=2))
	for (i in 1:length(states)) {
		count <- nrow(filter(df,Estado==states[i]))
		newrow <- c(states[i],count)
		new_df[i,] <- newrow
	}

	colnames(new_df) <- c("Estado",date)
	return(new_df)
}


date0 <- as.Date("2020-03-16")
today <- Sys.Date()
n_days <- as.numeric(today-date0)

create_for_day <- function(modalidad){

	if (modalidad != "positivos" && modalidad != "sospechosos") {
		stop()
	} else {
	
	
	for (file in list.files("raw",pattern=paste(".*",modalidad,".*.pdf",sep=""),full.names=TRUE)) {
	
		rexp <- "^.*([0-9]{4}.[0-9]{2}.[0-9]{2}).pdf$"
		filedate <- paste(as.character(str_split(sub(rexp,"\\1",file),"\\.",simplify=TRUE)),collapse="-") #format to YYYY-MM-DD

		new_df <- count_data(get_data(file),filedate)
		
		data_dir <- paste("data/",filedate,"/",sep="")
		dir.create(data_dir)
		write.csv(new_df, file=paste(data_dir,modalidad,"_",filedate,".csv",sep=""),row.names=FALSE,quote=FALSE)
		}
	}
}

#create_for_day("sospechosos")
#create_for_day("positivos")
