set terminal postscript eps enhanced color
set output "run_time.eps"
set title "Run Times using various options in Polly"
set ylabel "Time (seconds)"
set xlabel "N in matmul.c"
set xtics 1024, 32, 2048 rotate
set key font ",10"
set key at 1440, 35
plot "./matmul_2D/nopolly_run.txt" using (1024+($0*32)):1 with lines title "Just O3",\
"./matmul_2D/polly_normal_run.txt" using (1024+($0*32)):1 with lines title "Just Polly",\
"./matmul_2D/polly_parallel_1_run.txt" using (1024+($0*32)):1 with lines title "Polly with Parallel (num-threads=1)",\
"./matmul_2D/polly_parallel_2_run.txt" using (1024+($0*32)):1 with lines title "Polly with Parallel (num-threads=2)",\
"./matmul_2D/polly_parallel_3_run.txt" using (1024+($0*32)):1 with lines title "Polly with Parallel (num-threads=3)",\
"./matmul_2D/polly_parallel_4_run.txt" using (1024+($0*32)):1 with lines title "Polly with Parallel (num-threads=4)",\
"./matmul_2D/polly_tile_run.txt" using (1024+($0*32)):1 with lines title "Polly with Tiling",\
"./matmul_2D/polly_vector_run.txt" using (1024+($0*32)):1 with lines title "Polly with Vectorizer (Stripmining)",\
"./matmul_2D/polly_vector_tile_run.txt" using (1024+($0*32)):1 with lines title "Polly with Tiling and Vectorizer (Stripmining)"
