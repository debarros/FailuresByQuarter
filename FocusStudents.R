#FocusStudents.R

# Determine the focus students for a particular quarter  ####

#Set the parameters
Quarter = "1617 Q2"
maxFs = 3
maxHorribles = 1
HorribleLevel = 50
maxAs = 2
minFs = 2

#Read in the data
x = read.xlsx(xlsxFile = "failures_by_quarter.xlsx", sheet = Quarter, cols = 1:9)

#Get the set of unique students
Students = x[!duplicated(x$Student.Number),1:3]

#Initialize the student-level variables
Students$Failures = NA_integer_
Students$Ds = NA_integer_
Students$As = NA_integer_
Students$GPA = NA_integer_
Students$Horribles = NA_integer_ 

#Fill in the values for the student-level variables
for(i in 1:nrow(Students)){
  Students$Failures[i] = sum(x$Student.Number == Students$Student.Number[i] & x$grade == "F")
  Students$Ds[i] = sum(x$Student.Number == Students$Student.Number[i] & x$grade %in% c("D","D+","D-"))
  Students$As[i] = sum(x$Student.Number == Students$Student.Number[i] & x$grade %in% c("A","A+","A-"))
  Students$GPA[i] = mean(x$Percent[x$Student.Number == Students$Student.Number[i]])
  Students$Horribles[i] = sum(x$Student.Number == Students$Student.Number[i] & x$Percent < HorribleLevel)
}


#Select students who have no more than 1 grade below HorribleLevel, no more than maxFs failures.  Then sort by lowest GPA.
S2 = Students[Students$Failures <= maxFs,]
S2 = S2[S2$Horribles <= maxHorribles,]
S2 = S2[order(S2$GPA),]


#Break into grade levels
S2.9 = S2[S2$Grade.Level == 9,]
S2.10 = S2[S2$Grade.Level == 10,]
S2.11 = S2[S2$Grade.Level == 11,]
S2.12 = S2[S2$Grade.Level == 12,]

#Limit to at most 15 students per grade level
if(nrow(S2.9) > 15){S2.9 = S2.9[1:15,]}
if(nrow(S2.10) > 15){S2.10 = S2.10[1:15,]}
if(nrow(S2.11) > 15){S2.11 = S2.11[1:15,]}
if(nrow(S2.12) > 15){S2.12 = S2.12[1:15,]}


# Create the output file
wb = createWorkbook()

addWorksheet(wb = wb, sheetName = "9")
freezePane(wb, "9", firstActiveRow = 2, firstActiveCol = 3)
setColWidths(wb, "9", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
writeData(wb, "9", S2.9)

addWorksheet(wb = wb, sheetName = "10")
freezePane(wb, "10", firstActiveRow = 2, firstActiveCol = 3)
setColWidths(wb, "10", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
writeData(wb, "10", S2.10)

addWorksheet(wb = wb, sheetName = "11")
freezePane(wb, "11", firstActiveRow = 2, firstActiveCol = 3)
setColWidths(wb, "11", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
writeData(wb, "11", S2.11)

addWorksheet(wb = wb, sheetName = "12")
freezePane(wb, "12", firstActiveRow = 2, firstActiveCol = 3)
setColWidths(wb, "12", cols = 1:10, widths = "auto", ignoreMergedCells = FALSE)
writeData(wb, "12", S2.12)

saveWorkbook(wb = wb, file = "focus.xlsx", overwrite = T)


# Focus Student Analysis ####
# Assess the effectiveness of the focus student initiative

#read in the set of all focus student selections
focus = read.xlsx(xlsxFile = "focus students.xlsx", sheet = "All focus student selections")

#read in all quarter grades
grades = read.xlsx(xlsxFile = "all quarter grades 1415 Q1 through 1617 Q2.xlsx")
grades = data.table(grades)
grades = grades[grades$SchoolName == "Green Tech High Charter School",]
grades = grades[!is.na(grades$Grade),]
grades = grades[!is.na(grades$Teacher_Name),]

#Create a variable that indicates the year and quarter of the grade
grades$Quarter = round(x = grades$TermID/100 - 10, digits = 0)
grades$Quarter = paste0(grades$Quarter, grades$Quarter+1, " ",grades$StoreCode)
grades$Code = paste0(grades$`[1]Student_Number`, grades$Quarter, grades$Teacher_Name)

grades$Code[duplicated(grades$Code)]

focus$PriorQuarter = paste0(substr(focus$Quarter,1,6), as.integer(substr(focus$Quarter,7,7))-1)
focus$StartCode = paste0(focus$ID,focus$PriorQuarter, focus$FullTeacher)
focus$EndCode = paste0(focus$ID,focus$Quarter, focus$FullTeacher)


colnames(focus)

# grades.wide = dcast.data.table(data = grades, `[1]Student_Number` + Teacher_Name + Course_Name ~ Quarter, value.var = c("Percent", "Grade"))
# 
# grades.wide[,paste0(unique(focus$Quarter), " focus")] = 0
# 
# 
# #This double loop takes a really long time to run
# for (i in 1:nrow(grades.wide)){
#   for(j in unique(focus$Quarter)){
#     grades.wide[i,paste0(j, " focus")] = sum(focus$Quarter == j & focus$ID == grades.wide$`[1]Student_Number`[i] & focus$FullTeacher == grades.wide$Teacher_Name[i])
#   }
# }
# 
# 
# 
# summary(grades.wide)

#For each quarter during which focus students were selected,
#grab the pre and post grades
#and label each enrollment as N (nonfocus), F (focus), or O (Other enrollment for a focus student)
FocusQuarters = unique(focus$Quarter)

grades$focus = factor("N", levels = c("N","F","O"), labels = c("Nonfocus","Focus","OtherThanFocus"))

for(i in 1:nrow(grades)){
  if(sum(focus$Quarter == grades$Quarter[i])>0){
    if(sum(focus$ID == grades$`[1]Student_Number`[i] & focus$Quarter == grades$Quarter[i])>0){
      grades$focus[i] = "OtherThanFocus"
      if(sum(focus$ID == grades$`[1]Student_Number`[i] & focus$Quarter == grades$Quarter[i] & focus$FullTeacher == grades$Teacher_Name[i]) > 0){
        grades$focus[i] = "Focus"
      }
    }
  }
}




grades$PriorQuarter = paste0(substr(grades$Quarter,1,6), as.integer(substr(grades$Quarter,7,7))-1)
grades$PriorCode = paste0(grades$`[1]Student_Number`, grades$PriorQuarter, grades$Teacher_Name)
grades$PriorPercent = grades$Percent[match(grades$PriorCode, grades$Code)]
grades$PriorGrade = grades$Grade[match(grades$PriorCode, grades$Code, nomatch = NA_character_)]

grades.reduced = grades[!is.na(grades$PriorGrade),]




results = vector(mode = "list", length = length(FocusQuarters))
for(i in 1:length(results)){ results[[i]] = list() }
names(results) = FocusQuarters


for(i in FocusQuarters){
  grades.1q = grades.reduced[grades.reduced$Quarter == i,]
  courses = unique(grades.1q$Course_Name)
  teachers = unique(grades.1q$Teacher_Name)
  for(j in courses){
    grades.1q.1c = grades.1q[grades.1q$Course_Name == j,]
    if(!all(grades.1q.1c$focus == "Nonfocus")){
      ThisResult = list(lm(Percent ~ PriorPercent + focus, data = grades.1q.1c))
      names(ThisResult) = j
      results[[i]] = c(results[[i]], ThisResult)
    }
  }
  for(k in teachers){
    grades.1q.1t = grades.1q[grades.1q$Teacher_Name == k,]
    if(!all(grades.1q.1t$focus == "Nonfocus")){
      ThisResult = list(lm(Percent ~ PriorPercent + focus, data = grades.1q.1t))
      names(ThisResult) = k
      results[[i]] = c(results[[i]], ThisResult)
    }
  }
}






resultsTable = expand.grid(FocusQuarters, c(unique(grades.reduced$Course_Name), unique(grades.reduced$Teacher_Name)))
colnames(resultsTable) = c("Quarter", "Course")
resultsTable$FocusEffect = NA_integer_
resultsTable$OtherEffect = NA_integer_
resultsTable$n = NA_integer_
resultsTable$Other.p = NA_integer_
resultsTable$n = NA_integer_

for(i in 1:length(results)){
  for(j in 1:length(results[[i]])){
    resultsTable$FocusEffect[resultsTable$Quarter == names(results[i]) & resultsTable$Course == names(results[[i]][j])] = results[[i]][[j]]$coefficients["focusFocus"]
    resultsTable$OtherEffect[resultsTable$Quarter == names(results[i]) & resultsTable$Course == names(results[[i]][j])] = results[[i]][[j]]$coefficients["focusOtherThanFocus"]
    resultsTable$Focus.p[resultsTable$Quarter == names(results[i]) & resultsTable$Course == names(results[[i]][j])] = lm.coeffp(results[[i]][[j]])["focusFocus"]
    resultsTable$Other.p[resultsTable$Quarter == names(results[i]) & resultsTable$Course == names(results[[i]][j])] = lm.coeffp(results[[i]][[j]])["focusFocus"]
    resultsTable$n[resultsTable$Quarter == names(results[i]) & resultsTable$Course == names(results[[i]][j])] = nobs(results[[i]][[j]])
  }
}

resultsTable = resultsTable[!(is.na(resultsTable$n)), ]

summary(resultsTable[resultsTable$Other.p < 0.1,])
summary(resultsTable[resultsTable$Focus.p < 0.1,])

for(i in FocusQuarters){print(summary(resultsTable[resultsTable$Quarter == i,]))}
for(i in unique(resultsTable$Course)){
  print(i)
  print(summary(resultsTable[resultsTable$Course == i,c(3,4)])[4,])
  print("")
}


teachers = sort(intersect(unique(grades.reduced$Teacher_Name), unique(resultsTable$Course)))
for(i in teachers ){
  print(i)
  print(summary(resultsTable[resultsTable$Course == i,])[,c(3,4)])
}

summary(resultsTable$Course)

resultsTable[resultsTable$Course == "Ramirez, Judith",]

resultsTable = resultsTable[resultsTable$n > 20,]

summary(resultsTable)


summary(ThisResult$`Wood-Wind Instruments`)


str(lm.coeffp(ThisResult$`Wood-Wind Instruments`))




overall = lm(Percent ~ PriorPercent + focus, data = grades.reduced)

overall2 = lm(Percent ~ PriorPercent + Quarter + focus + Teacher_Name, data = grades.reduced)

summary(overall2)

colnames(grades.reduced)
