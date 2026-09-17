student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

#PART A
section <-factor(section,levels=c("A","B")) 
levels(section)

students<-data.frame(student_id,section,quiz1,quiz2,passed)

score_matrix<-matrix(c(quiz1,quiz2),nrow=6,ncol=2)
rownames(score_matrix)<-c(student_id)
colnames(score_matrix)<-c("quiz1", "quiz2")

course_record <- list(course = "R Programming",scores = students,cutoffs = c(pass=70,excellent=90))
typeof(course_record)
class(course_record)
str(students)
dim(score_matrix)
length(students)
str(course_record)
typeof(students)
#A vector is 1D and the data type must be consistent throughout (numeric, char, logical), while a list can have strings and numbers mixed together
#A matrix is a 2D vector with rows and columns where the data within must all be the same type (numeric, char, logical).
#Factors are categorical data where it can be organized by levels, while data frames are tabular where each column can have different data types but must all be the same lengths, unlike a list.


#PART B
score_matrix
score_matrix[4,2]
score_matrix[1:2, , drop=FALSE]
course_record
course_record["course"]
course_record[["course"]]
course_record$"course"

#[ ] is used to select the sublist. [[ ]] is used to select the actual element itself. $ is the actual element by its name, not element number.

#PART C

students$average<-rowMeans(score_matrix)
students$excellent<-students$average>=90
students[students$section == "A" & students$average >= 80, c("student_id","section","average")]
average_named<-students$average
names(average_named)<-student_id

#These calculations are vectorized because they are being done on the entire column/vector at once without using any loops.


#PROBLEM 2

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

#PART A
measurements<-read.csv(text=csv_text,na.strings=c("","NA"))
measurements
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

sum(is.na(measurements$sample_id))
sum(is.na(measurements$site))
sum(is.na(measurements$temp_c))
sum(is.na(measurements$ph))
sum(is.na(measurements$status))

measurements_complete<- measurements[complete.cases(measurements),]
measurements_complete
measurements$sample_id[!complete.cases(measurements)]
#x == NA doesn't work because comparing anything to an unknown value NA will
#just return back NA, not TRUE or FALSE

#PART B
measurements$status<-factor(measurements$status,levels=c("ok","review"))
levels(measurements$status)
measurements$site<-factor(measurements$site,levels=c("North","South","East"))
levels(measurements$site)

measurements$temp_f = measurements$temp_c * 9 / 5 + 32
ph_below_7<-measurements$ph<7

measurements[complete.cases(measurements) & (measurements$site == "North" | measurements$site == "South") & measurements$status == "ok",
             c("sample_id", "site", "temp_c", "temp_f", "ph")]

mean(measurements$temp_c, na.rm = TRUE)
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

#PART C
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)
dim(A*B)
dim(A%*%B)
# A*B multiplies elements in the same position while A%*%B multiplies rows by 
#columns using dot products. Both are 2x2 matrix in this scenario  since A and B 
#are the same size but that is not always true.

#PROBLEM 3
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)
grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {if (is.na(score))return(NA_character_)
    if (score>=a_min){
      "A"
    }else if (score>=b_min){
      "B"
    }else if (score>=c_min){
      "C"
    }else if (score>=d_min){
      "D"
    }else {
      "F"
    }}

print(grade_one(NA)) #NA
print(grade_one(90)) #A
print(grade_one(80)) #B
print(grade_one(85)) #B
print(grade_one(74)) #C

#PART B
grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i]<-grade_one(scores[i])
}
names(grades) <- student_id
grades
#i is the position of the element in the scores vector used to input into the grade_one function

#PART C-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(count=length(x),
  missing_count=sum(is.na(x)),
  mean=round(mean(x,na.rm=na.rm),digits),
  std_dev=round(sd(x,na.rm=na.rm),digits),
  min=round(min(x,na.rm=na.rm),digits),
  max=round(max(x,na.rm=na.rm),digits))
}
summarize_scores(scores)
summarize_scores( x = scores, na.rm = TRUE, digits = 2 )

#PART C-2
plot_scores<- function(x,...) {
  plot(x,...)
  
}
plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )
#my prediction is that the plot will be a line graph showing the scores on the y axis and the x axis will be the position of the score in the vector.