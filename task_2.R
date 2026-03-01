# Thư viện 'boot' là công cụ tiêu chuẩn để thực hiện tái lấy mẫu (resampling) trong R
library(boot)

data <- read.table(here::here("cancersurvival.dat", header = TRUE))

# Chuyển đổi sang thang logarit theo yêu cầu đề bài để giảm độ lệch (skewness)
stomach <- log(data$survival[data$disease == 1])
breast  <- log(data$survival[data$disease == 2])

# 3. ĐỊNH NGHĨA CÁC HÀM THỐNG KÊ (STATISTIC FUNCTIONS)

# 3.1. Hàm dùng cho Studentized Bootstrap
# Hàm này phải trả về một vector: [giá trị trung bình, phương sai của giá trị đó]
boot_stud_func <- function(data, indices) {
  sample_data <- data[indices] # Lấy mẫu bootstrap dựa trên chỉ số indices
  m <- mean(sample_data)       # Tính trung bình mẫu
  v <- var(sample_data) / length(sample_data) # Tính phương sai của trung bình mẫu (SE^2)
  return(c(m, v))
}

# 3.2. Hàm dùng cho BCa Bootstrap
# Phương pháp BCa chỉ yêu cầu thống kê chính (trung bình)
boot_mean_func <- function(data, indices) {
  return(mean(data[indices]))
}

# 4. THỰC HIỆN BOOTSTRAP VÀ TÍNH KHOẢNG TIN CẬY (CI)
# Thiết lập seed để kết quả có thể tái lập (reproducible)
set.seed(2024) 
R_iterations <- 2000 # Số lần lặp lại bootstrap (thường chọn từ 1000-2000)

# --- NHÓM 1: UNG THƯ DẠ DÀY (STOMACH) ---

# Thực hiện bootstrap
boot_obj_stomach <- boot(data = stomach, statistic = boot_stud_func, R = R_iterations)

# Tính CI Studentized và BCa
ci_stomach <- boot.ci(boot_obj_stomach, conf = 0.95, type = c("stud", "bca"))

# --- NHÓM 2: UNG THƯ VÚ (BREAST) ---

# Thực hiện bootstrap
boot_obj_breast <- boot(data = breast, statistic = boot_stud_func, R = R_iterations)

# Tính CI Studentized và BCa
ci_breast <- boot.ci(boot_obj_breast, conf = 0.95, type = c("stud", "bca"))

# 5. XUẤT KẾT QUẢ RA MÀN HÌNH
cat("\n--- KẾT QUẢ KHOẢNG TIN CẬY 95% (THANG LOG) ---\n")

print_results <- function(label, ci_obj) {
  cat("\n", label, ":\n")
  cat("  + Studentized CI: [", ci_obj$stud[4], ",", ci_obj$stud[5], "]\n")
  cat("  + BCa CI        : [", ci_obj$bca[4], ",", ci_obj$bca[5], "]\n")
}

print_results("UNG THƯ DẠ DÀY", ci_stomach)
print_results("UNG THƯ VÚ", ci_breast)

# 6. (TÙY CHỌN) KIỂM TRA PHÂN PHỐI BOOTSTRAP
# Biểu đồ giúp kiểm tra xem phân phối mẫu có xấp xỉ chuẩn không
# plot(boot_obj_stomach)