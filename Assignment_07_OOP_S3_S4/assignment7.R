# Assignment #7 – OOP in R (S3 & S4)
# Andrew Burson


library(methods)  # S4 tools

line <- function(txt) cat("\n==================== ", txt, " ====================\n", sep = "")

# ------------------------------------------------------------
line("1) What data I'm using and what it looks like")
# ------------------------------------------------------------
data("mtcars")

cat("\nFirst 6 rows of mtcars:\n")
print(head(mtcars))

cat("\nStructure (str):\n")
str(mtcars)

cat("\nType vs Class (low-level vs high-level):\n")
cat("typeof(mtcars):", typeof(mtcars), "\n")  # list under the hood
cat("class(mtcars): ", paste(class(mtcars), collapse = ", "), "\n")  # data.frame

# 
line("2) Trying common R functions (summary/print/plot) on objects")
# 

cat("\nsummary(mtcars): this uses the data.frame version of summary\n")
print(summary(mtcars))

# Make a model to show a different class with different behavior
fit <- lm(mpg ~ wt + hp, data = mtcars)
cat("\nWhat class is the model?\n"); print(class(fit))

cat("\nsummary(fit): this uses the 'lm' version of summary (summary.lm)\n")
print(summary(fit))

cat("\nWhat methods exist for summary()? (first 12)\n")
print(utils::head(methods("summary"), 12))

cat("\nWhat methods exist for plot()? (first 12)\n")
print(utils::head(methods("plot"), 12))

# save a plot of the model to file (so it works in any environment)
# png("plot_lm.png"); plot(fit); dev.off()
# cat("\nSaved model plot to plot_lm.png\n")

# 
line("3) My S3 example (custom class + methods) + a no-method case")
# 

# Make a small S3 class
student <- list(name = "Andrew Burson", age = 24, GPA = 3.50)
class(student) <- "student_s3"

# print() for student_s3
print.student_s3 <- function(x, ...) {
  cat("Student (S3)\n")
  cat("Name:", x$name, "\n")
  cat("Age:", x$age, "\n")
  cat("GPA:", sprintf("%.2f", x$GPA), "\n")
  invisible(x)
}

# summary() for student_s3
summary.student_s3 <- function(object, ...) {
  status <- if (object$GPA >= 3.3) "Dean's List eligible" else "Good standing"
  out <- list(name = object$name, age = object$age, GPA = object$GPA, status = status)
  class(out) <- c("summary_student_s3", "list")
  out
}

# pretty print for the summary result
print.summary_student_s3 <- function(x, ...) {
  cat("Student summary\n")
  cat("Name:", x$name, "\n")
  cat("Age:", x$age, "\n")
  cat("GPA:", x$GPA, "\n")
  cat("Status:", x$status, "\n")
  invisible(x)
}

cat("\nprint(student):\n"); print(student)
cat("\nsummary(student):\n"); print(summary(student))

cat("\nTry plot(student): I did NOT write a plot method for this class, so it should error (that’s expected)\n")
res <- try(plot(student), silent = TRUE)
if (inherits(res, "try-error")) cat("plot error ->", attr(res, "condition")$message, "\n")

cat("\nDouble-check there is no plot.student_s3 method:\n")
print(getS3method("plot", "student_s3", optional = TRUE))  # should be NULL

# 
line("4) S3 inheritance: child uses parent method if needed")
# 

# Parent class + method
print.employee <- function(x, ...) {
  cat(x$name, "\n")
  cat("salary", x$salary, "\n")
  cat("union member", x$union, "\n")
  invisible(x)
}

joe <- list(name = "Joe", salary = 55000, union = TRUE)
class(joe) <- "employee"

# Child class extends parent (adds hrsthismonth), but has NO own print() method
kate <- list(name = "Kate", salary = 68000, union = FALSE, hrsthismonth = 2)
class(kate) <- c("hrlyemployee", "employee")  # child first, then parent

cat("\nprint(joe): parent has its own print\n"); print(joe)
cat("\nprint(kate): child has no print, so R falls back to print.employee\n"); print(kate)

# ------------------------------------------------------------
line("5) My S4 example (typed slots + safer errors)")
# ------------------------------------------------------------

# Define an S4 class with slots (types)
setClass("student_s4", slots = c(name = "character", age = "numeric", GPA = "numeric"))

# Create an object
s4stu <- new("student_s4", name = "Andrew Burson", age = 24, GPA = 3.50)

cat("\nDefault S4 show():\n"); show(s4stu)

# Custom show() (like print)
setMethod("show", "student_s4", function(object) {
  cat("Student (S4)\n")
  cat("Name:", object@name, "\n")
  cat("Age:", object@age, "\n")
  cat("GPA:", sprintf("%.2f", object@GPA), "\n")
})

cat("\nCustom S4 show():\n"); show(s4stu)

# Define a small S4 generic + method to demo dispatch
setGeneric("gpa_status", function(x) standardGeneric("gpa_status"))
setMethod("gpa_status", "student_s4", function(x) {
  if (x@GPA >= 3.3) "Dean's List eligible" else "Good standing"
})

cat("\nCall gpa_status(s4stu):\n"); print(gpa_status(s4stu))

# Show S4 safety: typo slot should error
cat("\nTry a wrong slot name (should error):\n")
bad <- try(s4stu@salry <- 4.0, silent = TRUE)
if (inherits(bad, "try-error")) cat("slot error ->", attr(bad, "condition")$message, "\n")

# 
line("6) Quick checks: S3 vs S4, class vs type, and method listings")
#

check_obj <- function(name, x) {
  cat("\n--", name, "--\n")
  cat("class():  ", paste(class(x), collapse = ", "), "\n")
  cat("typeof(): ", typeof(x), "\n")
  cat("isS4():   ", isS4(x), "\n")
}

check_obj("mtcars", mtcars)
check_obj("fit (lm)", fit)
check_obj("student (S3)", student)
check_obj("joe (S3)", joe)
check_obj("kate (S3 child)", kate)
check_obj("s4stu (S4)", s4stu)

cat("\nA few summary() methods:\n"); print(utils::head(methods("summary"), 10))
cat("\nA few plot() methods:\n"); print(utils::head(methods("plot"), 10))

cat("\nIs 'summary' an S3 generic name? ", utils::isS3stdGeneric("summary"), "\n")
cat("Is there an S4 generic named 'summary'? ", methods::isGeneric("summary"), "\n")

cat("\nDone.\n")
