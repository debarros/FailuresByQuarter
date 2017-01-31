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
