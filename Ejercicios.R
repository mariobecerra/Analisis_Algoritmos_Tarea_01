library(Rcpp)
library(dplyr)
library(tidyr)
library(ggplot2)
library(microbenchmark)

#######################################
#######################################
######### Pregunta1
#######################################
#######################################

#######################################
### Insertion sort: http://recologia.com.br/2013/10/insertionsort-em-r-e-c-usando-o-pacote-rcpp/
### Merge sort: http://recologia.com.br/2014/09/mergesort-em-c-usando-o-pacote-rcpp/
#######################################

sourceCpp("merge_sort.cpp")
sourceCpp("insertion_sort.cpp")


#######################################
### Comparaciones
#######################################

# n = 10,000
x <- rnorm(10000)
insertion_10000 <- microbenchmark(insertionsortC(x), times = 50, unit = "s")
merge_10000 <- microbenchmark(mergesortC(x), times = 50, unit = "s")

data.frame(insertion = insertion_10000$time/1000000000,
           merge = merge_10000$time/1000000000) %>%
  mutate(iteracion = 1:nrow(.)) %>% 
  gather(algoritmo, tiempo, 1:2) %>% 
  ggplot() +
  geom_boxplot(aes(x = algoritmo, y = tiempo)) +
  ggtitle("Desempeño de algoritmos con n = 10,000") +
  ylab("Tiempo en segundos") +
  theme_bw() +
  ggsave("out/Comparacion_sorts_10000.png") 

data.frame(insertion = insertion_10000$time/1000000000,
           merge = merge_10000$time/1000000000) %>%
  write.table("out/tiempos_sorts_10000",
              sep = "|",
              row.names = F)


# n = 100,000
x <- rnorm(100000)
insertion_100000 <- microbenchmark(insertionsortC(x), times = 10, unit = "s")
merge_100000 <- microbenchmark(mergesortC(x), times = 10, unit = "s")

data.frame(insertion = insertion_100000$time/1000000000,
           merge = merge_100000$time/1000000000) %>%
  mutate(iteracion = 1:nrow(.)) %>% 
  gather(algoritmo, tiempo, 1:2) %>% 
  ggplot() +
  geom_boxplot(aes(x = algoritmo, y = tiempo)) +
  ggtitle("Desempeño de algoritmos con n = 100,000") +
  ylab("Tiempo en segundos") +
  theme_bw() +
  ggsave("out/Comparacion_sorts_100000.png")

data.frame(insertion = insertion_100000$time/1000000000,
           merge = merge_100000$time/1000000000) %>%
  write.table("out/tiempos_sorts_100000",
              sep = "|",
              row.names = F)



# n = 1,000,000
x <- rnorm(1000000)
insertion_1000000 <- microbenchmark(insertionsortC(x), times = 10, unit = "s")
merge_1000000 <- microbenchmark(mergesortC(x), times = 10, unit = "s")

data.frame(insertion = insertion_1000000$time/1000000000,
           merge = merge_1000000$time/1000000000) %>%
  mutate(iteracion = 1:nrow(.)) %>% 
  gather(algoritmo, tiempo, 1:2) %>% 
  ggplot() +
  geom_boxplot(aes(x = algoritmo, y = tiempo)) +
  ggtitle("Desempeño de algoritmos con n = 1,000,000") +
  ylab("Tiempo en segundos") +
  theme_bw() +
  ggsave("out/Comparacion_sorts_1000000.png")

data.frame(insertion = insertion_1000000$time/1000000000,
           merge = merge_1000000$time/1000000000) %>%
  write.table("out/tiempos_sorts_1000000",
              sep = "|",
              row.names = F)


#######################################
### Bubble sort (modificado del original para que no use inline)
### https://www.r-bloggers.com/much-more-efficient-bubble-sort-in-r-using-the-rcpp-and-inline-packages/
#######################################

cppFunction('
NumericVector bubble_sort_cpp(NumericVector vec_in) {
  NumericVector vec = NumericVector(vec_in);                               
  double tmp = 0;                                                                      
  int no_swaps;                                                                        
  while(true) {                                                                        
  no_swaps = 0;                                                                    
  for (int i = 0; i < vec.size()-1; ++i) {                                         
  if(vec[i] > vec[i+1]) {                                                      
  no_swaps++;                                                              
  tmp = vec[i];                                                            
  vec[i] = vec[i+1];                                                       
  vec[i+1] = tmp;                                                          
  };                                                                           
  };                                                                               
  if(no_swaps == 0) break;                                                         
  };                                                                                   
  return vec;
}'  
)


#######################################
#######################################
######### Pregunta2
#######################################
#######################################

datos1 <- data.frame(n = seq(1, 50, 0.5)) %>% 
  mutate(MyF = 64*n*log(n)/log(2),
         Inserc = 8*n^2) %>% 
  gather(func, valores, MyF:Inserc)


datos2 <- data.frame(n = seq(1, 30, 0.5)) %>% 
  mutate(MyF = 32*n*log(n)/log(2) + 5*n,
         Inserc = 6*n^2) %>% 
  gather(func, valores, MyF:Inserc)

datos1 %>% 
  ggplot(aes(x = n, y = valores, group = func)) +
  geom_line(size = 0.5, aes(linetype = func)) +
  theme_bw() +
  ggsave("out/Comparacion_cuadrado_log_1.png")


datos2 %>% 
  ggplot(aes(x = n, y = valores, group = func)) +
  geom_line(size = 0.5, aes(linetype = func)) +
  theme_bw() +
  ggsave("out/Comparacion_cuadrado_log_2.png")

#######################################
#######################################
######### Pregunta 3
#######################################
#######################################


data.frame(n = seq(0, 15, 0.5)) %>% 
  mutate(exponencial = 2^n,
         cuadrado = 100*n^2) %>% 
  gather(func, valores, exponencial:cuadrado) %>% 
  ggplot(aes(x = n, y = valores, group = func)) +
  geom_line(size = 0.5, aes(linetype = func)) +
  theme_bw() +
  ggsave("out/Comparacion_cuadrado_exp.png")


funcion_n2 <- function(n) {100*n*n - 2^n}
funcion_n2(0.1)
funcion_n2(0.103)
funcion_n2(0.105)
funcion_n2(0.103)
funcion_n2(0.104)

# Zooming

data.frame(n = seq(0, 0.25, 0.005)) %>% 
  mutate(exponencial = 2^n,
         cuadrado = 100*n^2) %>% 
  gather(func, valores, exponencial:cuadrado) %>% 
  ggplot(aes(x = n, y = valores, group = func)) +
  geom_line(size = 0.5, aes(linetype = func)) +
  theme_bw() +
  ggsave("out/Comparacion_cuadrado_exp_zoom_1.png")


data.frame(n = seq(10, 15, 0.5)) %>% 
  mutate(exponencial = 2^n,
         cuadrado = 100*n^2) %>% 
  gather(func, valores, exponencial:cuadrado) %>% 
  ggplot(aes(x = n, y = valores, group = func)) +
  geom_line(size = 0.5, aes(linetype = func)) +
  theme_bw() +
  ggsave("out/Comparacion_cuadrado_exp_zoom_2.png")




