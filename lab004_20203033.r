mpg_am_0 <- mtcars$mpg[mtcars$am == 0]

confidence_interval <- t.test(mpg_am_0, conf.level = 0.99)$conf.int

print(confidence_interval[1:2])

# am 필드가 1인 차량 필터링
am1_mpg <- mtcars$mpg[mtcars$am == 1]

# t-test 수행
result <- t.test(am1_mpg, mu = 20, alternative = "greater", conf.level = 0.95)

# 결과 출력
print(result$p.value)

# 데이터 파일 불러오기 #"/Users/ms/Downloads/lab23004/data.txt"
data <- read.table("data.txt",header = FALSE)

# 데이터프레임 생성
df <- data.frame(V1 = data$V1, V2 = data$V2, V3 = data$V3)

# 2번째 열과 3번째 열에 대한 t-test 수행
result_a <- t.test(df$V3, df$V2, paired = TRUE, conf.level = 0.95)

# 결과 출력
print(c(result_a$conf.int[1], result_a$conf.int[2]))

result_b <- t.test(df$V3, df$V2, conf.level = 0.95)

# 결과 출력
print(c(result_b$conf.int[1], result_b$conf.int[2]))

# 3번째 열의 값이 2번째 열의 값보다 큰 경우의 수 출력
count_cases <- sum(df$V3 > df$V2)

num_rows <- nrow(df)

# 결과 출력
print(c(num_rows,count_cases))

# 3번째 열의 값이 2번째 열의 값보다 큰 경우의 비율 계산
proportion_estimate <- mean(df$V3 > df$V2)

# 3번째 열의 값이 2번째 열의 값보다 큰 경우의 비율에 대한 95% 신뢰구간 계산
ci <- prop.test(sum(df$V3 > df$V2), nrow(df), conf.level = 0.95)$conf.int

# 결과 출력
print(proportion_estimate)
print(c(ci[1],ci[2]))
