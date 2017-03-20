rm(list = ls(all= TRUE))


##環境設定


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
					
	paste(as.character(as.POSIXlt(Sys.time(), "Asia/Taipei")),'CST')
					
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
		tags$hr(),
		tags$style("#errormessage{color: red;
                                 font-size: 16px;
                                 }"
                         )
    ),
    mainPanel(
		textOutput('rsmessage'),
		textOutput('errormessage'),
		tableOutput('contents')
    )
  )
)


##後端


server <- function(input,output,session){
	
	##讀入主辦單位驗證檔
	rawdata <- read.csv("answer.csv",header=T,stringsAsFactors=F)
	##讀入隊伍名稱資料檔
	teamdata <- read.csv("teamdata.csv",header=T,stringsAsFactors=F)
	teamdata$teamnumber <- as.character(round(as.numeric(teamdata$teamnumber)))
	
	##show charts
	output$contents <- renderTable({
	
		##讀入排行榜資料檔
		charts <- read.csv("charts.csv",header=T,stringsAsFactors=F)
		##讀入隊伍上傳檔
		inFile <- input$file1
		
		if (is.null(inFile)){
		
			output$errormessage <- renderText({NULL})
			output$rsmessage <- renderText({NULL})
			##若沒有上傳檔案，顯示檔案中舊的排行榜
			charts[,4] <- as.numeric(charts[,4])
			charts[,4] = round(charts[,4],6)
			colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
			return(charts)
			
		}	else	{

			output$errormessage <- renderText({NULL})
			output$rsmessage <- renderText({NULL})
			##若有上傳檔案則匯入檔案內容，計算出R square，更新排行榜

			
			##匯入資料、整理資料
			uploaddata <- read.csv(inFile$datapath, header=T, sep=",")
			user.teamnumber = as.character(utfunc(uploaddata))
			user.teamname = teamdata[teamdata$teamnumber==user.teamnumber,2]
			
			if ( length(user.teamname) == 0){
									
				output$errormessage <- renderText({"上傳失敗：您的隊伍不存在，請確認隊號是否正確"})
				output$rsmessage <- renderText({NULL})
				charts[,4] <- as.numeric(charts[,4])
				charts[,4] = round(charts[,4],6)
				colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
				return(charts)
						
			} else {
			
				if (nrow(uploaddata) < nrow(rawdata)) {
				
					output$rsmessage <- renderText({NULL})
					output$errormessage <- renderText({"上傳失敗：您上傳的資料檔資料筆數過少"})
					charts[,4] <- as.numeric(charts[,4])
					charts[,4] = round(charts[,4],6)
					colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
					return(charts)
			
				} else if (nrow(uploaddata) > nrow(rawdata)) {
					
					output$rsmessage <- renderText({NULL})
					output$errormessage <- renderText({"上傳失敗：您上傳的資料檔資料筆數過多"})
					charts[,4] <- as.numeric(charts[,4])
					charts[,4] = round(charts[,4],6)
					colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
					return(charts)
			
				} else {
					
					output$errormessage <- renderText({NULL})
					
					##merge驗證檔和上傳檔
					colnames(uploaddata)[2]<-"id"
					newdata <- merge(rawdata[,c(1,4)],uploaddata[,2:3],by="id",all.x=T)
	
					##計算R_Squared與建立上傳檔與隊伍的資訊
					actual = newdata[,2]
					predict = newdata[,3]
					user.rs = round(r.square(as.numeric(actual),as.numeric(predict)),6)
					user.uploadtime = paste(as.character(as.POSIXlt(Sys.time(), "Asia/Taipei")),'CST')
			
					if (nrow(charts)==0){
			
						output$rsmessage <- renderText({NULL})
						##若排行榜為空，直接建立新的表
						upt=  as.character(user.uploadtime)
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
					
								output$rsmessage <- renderText({paste0("根據您上傳資料，R squared是",user.rs)})
								##R_squared有提高，則更新、重新排序
								charts[charts[,2]==user.teamnumber,4] <- user.rs
								levels(charts[,5]) <- c(levels(charts[,5]),formattime(user.uploadtime))
								charts[charts[,2]==user.teamnumber,5] <- as.character(user.uploadtime)
								##R_squared相同者看誰最早交，最早交者勝
								colnames(charts) <- c("rank","teamnumber","teamname","rsquared","uploadtime")
								charts[,4] = round(charts[,4],6)
								charts = data.frame(rep(1:dim(charts)[1]),arrange(charts[,2:5],desc(rsquared),uploadtime))
								colnames(charts) <- c("排名","隊號","隊名","R squared","上傳時間")
								##更新排行榜檔案
								write.csv(charts,"charts.csv",row.names=F)
								return(charts)

							}	else	{

								output$rsmessage <- renderText({paste0("根據您上傳資料，R squared是",user.rs)})
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
			
			}
		}
	})
}

shinyApp(ui, server)
