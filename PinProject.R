# PinProject.R

# number of students in each grade who earned at least a certain GPA in each quarter
# number of students in each grade who earned at least a certain GPA at least once by each quarter
# number of students in each grade who earned at least a certain GPA every quarter up through each quarter
# Each series of data should represent a particular year, maybe


# read in one tab at a time
# for each quarter, identify the set of students and create a data.frame with one row for each
# have one column for ID, one column for Quarter, and one column for GPA
# for each student, calculate the GPA
# rbindlist to make one data.frame

quartergpas = vector(mode = "list", length = 4)
year = "1617"
for(i in 1:4){
  tabname = paste0(year, " Q",i)
  quartergrades = read.xlsx("failures_by_quarter.xlsx", sheet = tabname, cols = 1:9)
  gpa.frame = data.frame(Student.Number = unique(quartergrades$Student.Number))
  gpa.frame$gpa = Vgpa(gpa.frame$Student.Number, quartergrades)
  gpa.frame$Quarter = i
  quartergpas[[i]] = gpa.frame
}

gpa.frame.all = rbindlist(quartergpas)



#number of students who haven’t received demerits for 18 15 12 9 consecutive weeks.

