#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector insertionsortC(NumericVector vetor) {
        int n = vetor.size();
 
        double aux;
        int i , j;
 
        for(i=1;i<n;i++) {
            aux=vetor[i];
            j=i-1;
            while(j>=0 && vetor[j]>aux) {
                vetor[j+1]=vetor[j];
                j=j-1;
                }
            vetor[j+1]=aux;
            }
        return vetor;
        }