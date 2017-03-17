rm(list = ls(all= TRUE))


##環境設定


##請更改到網站的目錄

##設定精確到小數點第6位
options(digits = 6)
##載入所需套件	
library(shiny)
library(dplyr)
library(tidyr)

##計算R_Squared
r.square = function(y, fitted.y)
{
		
	summary(lm(y~fitted.y))$r.squared
				
}
##擷取數字
utfunc = function(data){
			
	ut = as.numeric(unique(unlist(strsplit(colnames(data)[1], "[^[:digit:]]"))))
	ut = ut[!is.na(ut)]
	return(ut)
				
}
##調整時間格式問題
formattime <- function(t){
					
	paste(as.character(as.POSIXlt(as.numeric(t),origin = "1970-01-01")),"CST")
					
}

##前端


ui <- fluidPage(

  titlePanel("Go,Kaggle-591房東期望房租預測排行榜"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
						 'text/comma-separated-values,text/plain', 
						 '.csv')),
      tags$hr()
    ),
    mainPanel(
      tableOutput('contents')
    )
  )
)


##後端


server <- function(input,output){
  #讀入答案
	answer <- read.csv('answer.csv')
	##讀入隊伍名稱資料檔
	teamdata <- read.csv("teamdata.csv",header=T)
	teamdata$teamnumber <- as.character(round(teamdata$teamnumber))
	
	##show charts
	output$contents <- renderTable({
	
		##讀入排行榜資料檔
		charts <- read.csv("charts.csv",header=T)
		##讀入隊伍上傳檔
		inFile <- input$file1
		
		if (is.null(inFile)){
		
			##若沒有上傳檔案，顯示檔案中舊的排行榜
			charts[,4] = round(charts[,4],6)
			colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
			return(charts)
			
		}	else	{

		
			##若有上傳檔案則匯入檔案內容，計算出R square，更新排行榜

			
			##匯入資料、整理資料
			uploaddata <- read.csv(inFile$datapath, header=T, sep=",")
			##merge驗證檔和上傳檔
			colnames(uploaddata)[2]<-"id"
			newdata <- merge(answer[,c(1,4)],uploaddata[,2:3],by="id",all.x=T)

			##計算R_Squared與建立上傳檔與隊伍的資訊
			user.teamnumber = as.character(utfunc(uploaddata))
			user.teamnumber = as.integer(user.teamnumber)
			user.teamname = teamdata[teamdata$teamnumber==user.teamnumber,2]
			actual = newdata[,2]
			predict = newdata[,3]
			user.rs = round(r.square(as.numeric(actual),as.numeric(predict)),6)
			user.uploadtime = Sys.time()
			
			if (nrow(charts)==0){
			
				##若排行榜為空，直接建立新的表
				upt=paste(as.character(user.uploadtime),"CST")
				charts = data.frame("1",user.teamnumber,user.teamname,user.rs,upt)
				colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
				write.csv(charts,"charts.csv",row.names=F)
				return(charts)
				
			}	else	{	
			
				##若排行榜有資料，則比較舊資訊決定是否進行更新

				if (sum(as.numeric(charts[,2]==user.teamnumber)) == 0)
				{
				
					##若此上傳檔所屬的隊伍未在排行榜內，則新增、重新排序
					partofcharts = charts[,2:5]
					newrecord = data.frame(user.teamnumber,user.teamname,user.rs,user.uploadtime)
					colnames(newrecord) <- colnames(partofcharts)
					v <- vapply(partofcharts, is.factor, NA)
					partofcharts[v] <- lapply(partofcharts[v], as.character)
					partofcharts = rbind(partofcharts,newrecord)
					partofcharts[nrow(partofcharts),4] <- formattime(partofcharts[nrow(partofcharts),4])
					
					if (is.na(partofcharts[nrow(partofcharts),4])){
						
						ft=formattime(Sys.time())
						levels(partofcharts) <- c(levels(charts[,5]),ft)
						partofcharts[nrow(partofcharts),4]<-ft

					}
					
					colnames(partofcharts) <- c("隊號","隊名","R squared","上傳時間")
					colnames(partofcharts) <- c("teamnumber","teamname","rsquared","uploadtime")
					partofcharts[,3] = round(partofcharts[,3],6)
					charts = data.frame(rep(1:dim(partofcharts)[1]),arrange(partofcharts,desc(rsquared),uploadtime))
					colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
					write.csv(charts,"charts.csv",row.names=F)
					return(charts)
					
				}	else	{
				
					##若此上傳檔所屬的隊伍已在排行榜內，則比對R square是否有提高

					if	(user.rs > charts[charts[,2]==user.teamnumber,4])
					{
					
						##R_squared有提高，則更新、重新排序
						charts[charts[,2]==user.teamnumber,4] <- user.rs
						levels(charts[,5]) <- c(levels(charts[,5]),formattime(user.uploadtime))
						charts[charts[,2]==user.teamnumber,5] <- paste(as.character(user.uploadtime),"CST")
						##R_squared相同者看誰最早交，最早交者勝
						colnames(charts) <- c("rank","teamnumber","teamname","rsquared","uploadtime")
						charts[,4] = round(charts[,4],6)
						charts = data.frame(rep(1:dim(charts)[1]),arrange(charts[,2:5],desc(rsquared),uploadtime))
						colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
						##更新排行榜檔案
						write.csv(charts,"charts.csv",row.names=F)
						return(charts)

					}	else	{

						##若R_squared沒有提昇或不改變排行榜
						colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
						return(charts)

					}
				}
			}
	
			##update charts of webpage
			colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
			return(charts)
	
		}
	})
}

shinyApp(ui, server)
