set.seed(123)
delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
partial_counts <- c(80, 100, 180, 80, 140)

# Create a complete triangle based on the known delay PMF
rep_mat_rows <- lapply(partial_counts, function(x) x * delay_pmf)
rep_mat <- do.call(rbind, rep_mat_rows)
triangle <- generate_triangle(rep_mat)
reporting_triangle <- rbind(rep_mat, triangle)


pt_nowcast_mat <- generate_pt_nowcast_mat(reporting_triangle)


# in order from horizon 1 to 4
disp_params <- c(10,10,10,10)
pred_nowcast_mat <- .extract_predictions(pt_nowcast_mat, reporting_triangle)
final_counts_with_uncertainty <- rowSums(reporting_triangle, na.rm = TRUE) + 
  c(rep(0,6), 
    rnbinom(n = length(disp_params), 
            size = rev(disp_params), 
            mu = rowSums(pred_nowcast_mat, na.rm = TRUE)[7:10])
  )
df <- data.frame(t = 1:10,
                 mu = rowSums(pt_nowcast_mat, na.rm = TRUE),
                 sample = final_counts_with_uncertainty)


# Make a plot to check that a draw and a point appear to have correct 
# error
ggplot(df) +
  geom_line(aes(x = t, y = mu)) +
  geom_point(aes(x = t, y = final_counts_with_uncertainty), color = "blue")


# Create a reporting triangle that is jumbled to different degrees by h
# horizon
max_t <-nrow(reporting_triangle)
rep_tri_new <- reporting_triangle
for (i in 1:length(disp_params)){
  rep_tri_new[(max_t -i + 1), 1:i] <- rnbinom(
    n = i,
    size = disp_params[i],
    mu = reporting_triangle[(max_t -i + 1), 1:i])
}

trunc_rep_tri_list <- truncate_triangles(rep_tri_new)
reporting_triangle_list <- generate_triangles(trunc_rep_tri_list)

pt_nowcast_mat_list <- generate_pt_nowcast_mat_list(reporting_triangle_list)

dispersion <- estimate_dispersion(
  pt_nowcast_mat_list,
  trunc_rep_tri_list,
  reporting_triangle_list
)

nowcast_draws_df <- get_nowcast_draws(
  generate_pt_nowcast_mat(rep_tri_new),
  rep_tri_new,
  dispersion = dispersion,
  draws = 100
) |>
  left_join(data.frame(time = 1:10, 
                       pt_nowcast = rowSums(generate_pt_nowcast_mat(rep_tri_new)),
                       orig_data = rowSums(rep_tri_new, na.rm = TRUE))
  )

ggplot(nowcast_draws_df) + 
  geom_line(aes(x = time, y = pred_count, group = draw), alpha = 0.1) +
  geom_line(aes(x = time, y = pt_nowcast), color = "red")+
  geom_line(aes(x = time, y = orig_data), color = "blue", size = 2)
